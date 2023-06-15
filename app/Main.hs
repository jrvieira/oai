module Main where

import Zero.Color
import Oai.Transit hiding ( id )

import System.Environment
import System.Directory ( createDirectoryIfMissing, listDirectory, doesFileExist )
import System.Process
import System.Console.Readline ( readline, addHistory )
import System.IO
import Data.Maybe ( catMaybes )
import Data.Foldable ( toList )
import Data.Aeson ( encode, decodeStrict, encodeFile, decodeFileStrict )
import Data.ByteString.Lazy.UTF8 qualified as BU ( toString )
import Data.Text qualified as T ( pack )
import Data.Text.Encoding ( encodeUtf8 )
import Data.DList ( DList, singleton, snoc )
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import Control.Monad ( when )

data Ctx = Ctx
   { key  :: String
   , sess :: String
   , file :: String
   , past :: [Message]
   , logs :: DList Message
   , pipe :: DList String
   }

sys :: Message
sys = Message
   { role = Just "system"
   , content = Just prime
   , name = Nothing
   }
   where
   prime = "You are a humble robot that gives short and to the point answers."

main :: IO ()
main = do
   key  <- head . lines <$> readFile "key.txt"
   sess <- head <$> getArgs
   let path = "mem/" <> sess <> "/"
   createDirectoryIfMissing True path
   files <- listDirectory path
   past <- fmap (mconcat . catMaybes) . sequence $ decodeFileStrict <$> (path <>) <$> files
   time <- formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime
   let file = path <> time <> ".json"
   let ctx = Ctx key sess file past (singleton sys) mempty
   -- add past to commandline history
   mapM_ (addHistory . maybe "" id . content) $ filter ((Just "user" ==) . role) past
   _ <- system "clear"
   hSetBuffering stdout NoBuffering
   putStrLn $ clr Bold $ clr Inverse $ clr Magenta $ unwords ["",sess,""]
   putStr "\n"
   wait ctx

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
   | otherwise = do
      putStr "\n"
      logs' <- mem ctx "user" prompt
      o <- createProcess (shell $ call $ req logs') { std_out = CreatePipe , std_err = NoStream }
      ctx' <- res (ctx { logs = logs' }) o
      wait ctx'

   where

   comm :: IO ()
   comm
      -- quit
      | ":q" <- prompt = pure ()
      -- clear past context
      | ":c" <- prompt = do
         tell "cleared past context"
         wait (ctx { past = [] })
      -- list sessions
      | ":l" <- prompt = do
         l <- listDirectory "mem"
         tell $ unwords $ "list sessions:" : map (\s -> "'" <> s <> "'") l
         wait ctx
      -- save current session
      | ":s" <- prompt = do
         appendFile ("mem/" <> sess ctx <> ".txt") (unlines $ map (\m -> unwords [maybe "" id $ role m,":",maybe "" id $ content m,"\n"]) $ toList $ logs ctx)
         tell $ unwords ["saved:",sess ctx]
         wait ctx
      -- include file
      | (":f":f:q) <- words prompt = do
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
   tell s = putStrLn $ clr Dim $ ' ' : s <> "\n"

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

   req :: DList Message -> Request
   req l = Request
      { model = "gpt-3.5-turbo"
      , messages = past ctx <> toList l
      , top_p = Just 0.01
      , stream = Just True
      , frequency_penalty = Just 1
      , temperature = Nothing
      , n = Nothing
      , stop = Nothing
      , max_tokens = Nothing
      , presence_penalty = Nothing
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
         putStr "\n"
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
         putStr "\n"
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

mem :: Ctx -> String -> String -> IO (DList Message)
mem ctx r c = do
   encodeFile (file ctx) logs'
   pure logs'
   where
   logs' = snoc (logs ctx) $ Message
      { role = Just r
      , content = Just c
      , name = Just $ sess ctx
      }

