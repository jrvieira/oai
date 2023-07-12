module Main where

import Zero.Color
import Oai.Transit hiding ( id )

import System.Environment
import System.Directory ( createDirectoryIfMissing, listDirectory, doesFileExist, removeFile )
import System.Process
import System.Console.Readline ( readline, addHistory )
import System.IO
import Data.Maybe ( listToMaybe, catMaybes )
import Data.List ( intercalate, sort )
import Data.Foldable ( toList )
import Data.Aeson ( encode, decodeStrict, encodeFile, decodeFileStrict )
import Data.ByteString.Lazy.UTF8 qualified as BU ( toString )
import Data.Text qualified as T ( pack )
import Data.Text.Encoding ( encodeUtf8 )
import Data.DList ( DList, snoc )
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import Control.Monad ( when )

data Ctx = Ctx
   { key  :: String  -- API key ( file can contain multiple lines, first one is used )
   , sess :: String  -- session name
   , file :: String  -- current log file
   , past :: [Message]  -- mem past context
   , logs :: [Message]  -- mem current context
   , pipe :: DList String  -- mem stream buffer
   }

prime :: Message
prime = Message
   { role = Just "system"
   , content = Just m
   , name = Just "stick"
   , function_call = Nothing
   }
   where m = "You are a humble AI that gives short and to the point answers. No chattiness. No false information. No wrong answers. Admits when unsure. Strongly avoids repetition."

main :: IO ()
main = do
   key  <- head . lines <$> readFile "key.txt"
   sess <- maybe "none" id . listToMaybe <$> getArgs
   let path = "mem/" <> sess <> "/"
   createDirectoryIfMissing True path
   files <- sort <$> listDirectory path
   past <- fmap (mconcat . catMaybes) . sequence $ decodeFileStrict <$> (path <>) <$> files
   time <- formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime
   let file = path <> time <> ".json"
   let ctx = Ctx key sess file (if sess == "none" then mempty else past) (if null past then [prime] else []) mempty
   -- add past to commandline history
   mapM_ (addHistory . maybe "" id . content) $ filter ((Just "user" ==) . role) past
   _ <- system "clear"
   hSetBuffering stdout NoBuffering
   putStrLn $ clr Bold $ clr Inverse $ clr Magenta $ unwords ["",sess,""]
   putStrLn " "
   wait ctx
   where

wait :: Ctx -> IO ()
wait ctx = do
   hSetEcho stdin True
   prompt <- maybe "" id <$> readline ""
   hSetEcho stdin False
   when (not $ null prompt) $ addHistory prompt
   loop ctx prompt

-- MAIN LOOP

loop :: Ctx -> String -> IO ()
loop ctx prompt
   | null prompt = wait ctx
   | ':' <- head prompt = comm
   | ('!':q) <- prompt = do
      o <- readProcess "sh" ["-c",q] ""
      putStrLn o
      wait ctx
   | otherwise = do
      putStrLn " "
      logs' <- mem ctx "user" prompt
      o <- createProcess (shell $ call $ req logs') { std_out = CreatePipe , std_err = NoStream }
      ctx' <- res (ctx { logs = logs' }) o
      wait ctx'

   where

   comm :: IO ()
   comm
      -- help
      | elem prompt [":h",":help"] = do
         tell $ unlines
            ["help\n"
            , ":quit\tquit"
            , ":restart\trestart (beta)"
            , ":list\tlist sessions"
            , ":save\tsave entire session in human readable format"
            , ":stat\tshow memory status"
            , ":cpast\tclear past context (keeps current context)"
            , ":clogs\tclear current context (keeps past context)"
            , ":kp\thalve past context (keeps current)"
            , ":kl\thalve current context (keeps past context)"
            , ":stick\tstick last prompt + response"
            , ":prime\tprime with system message"
            , ":file ...\tinclude a file"
            ]
         wait ctx
      -- quit
      | elem prompt [":q",":quit"] = pure ()
      -- restart (beta)
      | elem prompt [":r",":restart"] = do
         e <- doesFileExist $ file ctx
         when e $ removeFile $ file ctx
         tell "restart session"
         main
      -- list sessions
      | elem prompt [":l",":list"] = do
         l <- sort <$> listDirectory "mem"
         tell "list sessions"
         putStrLn $ clr Bold $ intercalate " " $ (clr Bold $ clr Inverse $ clr Magenta $ unwords ["",sess ctx,""]) : ((\s -> clr Inverse $ unwords ["",s,""]) <$> filter (/= sess ctx) l)
         putStrLn " "
         wait ctx
      -- save entire session
      | elem prompt [":s",":save"] = do
         writeFile ("mem/" <> sess ctx <> ".txt") (unlines $ map (\m -> unwords [maybe "" id $ role m,":",maybe "" id $ content m,"\n"]) $ past ctx <> logs ctx)
         tell $ unwords ["saved:",sess ctx]
         wait ctx
      -- show memory status
      | ":stat" <- prompt = do
         tell $ unwords ["past",show $ length $ past ctx,"/","logs",show $ length $ logs ctx]
         wait ctx
      -- clear past context
      | ":cpast" <- prompt = do
         tell "cleared past context"
         wait (ctx { past = filter sticky $ past ctx })
      -- clear current context
      | ":clogs" <- prompt = do
         tell "cleared current context"
         wait (ctx { logs = filter sticky $ logs ctx })
      -- halve past context (not past)
      | ":kp" <- prompt = do
         tell "halved past context"
         let (kill,rest) = splitAt (div (length $ past ctx) 2) $ past ctx
         wait (ctx { past = filter sticky kill <> rest })
      -- halve current context (not past)
      | ":kl" <- prompt = do
         tell "halved current context"
         let (kill,rest) = splitAt (div (length $ logs ctx) 2) $ logs ctx
         wait (ctx { logs = filter sticky kill <> rest })
      -- stick last prompt + response
      | ":stick" <- prompt = do
         tell "sticked last prompt + response"
         let (l,s) = splitAt (length (logs ctx) - 2) $ logs ctx
         wait (ctx { past = l <> map stick s })
      -- prime with system message
      | (":prime":p) <- words prompt = do
         tell "primed"
         logs' <- mem ctx "system" $ unwords p
         wait (ctx { logs = logs' })
      -- include file
      | (c:f:q) <- words prompt , elem c [":f",":file"] = do
         e <- doesFileExist f
         if e then do
            i <- readFile f
            loop ctx $ unlines ["consider this:","","```",i,"```","",unwords q]
         else do
            tell "file not found"
            wait ctx
      -- no command
      | otherwise = do
         tell $ unwords ["^no command",head $ words prompt]
         wait ctx

   tell :: String -> IO ()
   tell s = putStrLn $ clr Grey $ ' ' : s <> "\n"

   call :: Request -> String
   call r = unwords
      [ "curl https://api.openai.com/v1/chat/completions"
      , "--no-buffer"
      , "-H 'Content-Type: application/json'"
      , "-H 'Authorization: Bearer " <> key ctx <> "'"
      , "-d '" <> (escape $ BU.toString $ encode r) <> "'"
      ]
      where
      escape :: String -> String
      escape [] = []
      escape (x:xs)
         | '\'' <- x = "'\\''" <> escape xs
         | otherwise = x : escape xs

   req :: [Message] -> Request
   req l = Request
      { model = "gpt-3.5-turbo"
      , messages = past ctx <> l
      , functions = Nothing
      , function_call = Nothing
      , temperature = Nothing
      , top_p = Just 0.01
      , n = Nothing
      , stream = Just True
      , stop = Nothing
      , max_tokens = Nothing
      , presence_penalty = Nothing
      , frequency_penalty = Just 1.4
      , logit_bias = Nothing
      , user = Nothing
      }

-- IO

res :: Ctx -> (Maybe Handle,Maybe Handle,Maybe Handle,ProcessHandle) -> IO Ctx
res ctx h

   | (i,Nothing,e,_) <- h = do
      il <- maybe (pure "no handle for stdin") hGetLine i
      el <- maybe (pure "no handle for stderr") hGetLine e
      putStr $ clr Blue $ "stdin: " <> il
      putStr $ clr Red $ "stderr: " <> el
      pure ctx

   | (_,Just o,_,_) <- h = do
      eof <- hIsEOF o
      if eof then do
         logs' <- mem ctx "assistant" (concat $ toList $ pipe ctx)
         putStrLn " "
         pure $ ctx { logs = logs' , pipe = mempty }
      else do
         ol <- hGetLine o
         e <- echo ol
         res (ctx { pipe = snoc (pipe ctx) e }) h

   where

   echo :: String -> IO String
   echo load
      -- ignore empty load
      | null load = pure ""
      -- normal response
      | Just (json :: Response Choice) <- decodeStrict $ encodeUtf8 $ T.pack load = do
         let e = maybe "" id $ content $ message $ head $ choices json
         putStrLn $ clr Magenta e
         pure e
      -- end of openai's Server Sent Event stream
      | "data: [DONE]" <- load = do
         putStrLn " "
         pure ""
      -- SSE stream
      | ("data: ",d) <- splitAt 6 load , Just (json :: Response Delta) <- decodeStrict $ encodeUtf8 $ T.pack d = do
         let e = maybe "" id $ content $ delta $ head $ choices json
         putStr $ clr Magenta e
         pure e
      -- otherwise
      | otherwise = do
         putStrLn (clr Red load)
         pure ""

mem :: Ctx -> String -> String -> IO [Message]
mem ctx r c = do
   encodeFile (file ctx) logs'
   pure logs'
   where
   logs' = logs ctx <>
      [ Message
         { role = Just r
         , content = Just c
         , name = Just $ sess ctx
         , function_call = Nothing
         }
      ]

