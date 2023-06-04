module Main where

import Zero
import Zero.Color
import Oai.Transit hiding ( id )

import System.Environment
import System.Process
import System.IO ( Handle, hIsEOF, hGetLine )
import Data.Aeson ( encode, decodeStrict, encodeFile, decodeFileStrict )
import Data.ByteString.Lazy.UTF8 qualified as BU ( toString )
import Data.Text qualified as T ( pack )
import Data.Text.Encoding ( encodeUtf8 )
import Data.DList ( DList, snoc )
import Control.Monad ( forever )

main :: IO ()
main = do
   sess <- head <$> getArgs
   let file = "mem/" <> sess <> ".json"
   _ <- system $ unwords ["touch",file,"&& clear"]
   putStrLn $ clr Bold $ clr Inverse $ clr Magenta $ unwords ["",sess,""]
   putStr "\n"
   forever $ getLine >>= oai sess

oai :: String -> String -> IO ()
oai sess prompt = do
   ctx <- mem "user" prompt
   putStr "\n"
   o <- createProcess (shell $ cmd ctx) { std_out = CreatePipe , std_err = NoStream }
   res "" o

   where

   sys :: Message
   sys = Message { role = Just "system" , content = Just prime , name = Just sess }
      where
      prime = "You are a humble robot that gives short and to the point answers."

   cmd :: [Message] -> String
   cmd ctx = "curl https://api.openai.com/v1/chat/completions \
          \-H 'Content-Type: application/json' \
          \-H 'Authorization: Bearer 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' \
          \-d '" <> (BU.toString $ encode $ req ctx) <> "'"

   req :: [Message] -> Request
   req ctx = Request
      { model = "gpt-3.5-turbo"
      , messages = sys : ctx
      , top_p = Just 0.01
      , stream = Just False
      }

   res :: String -> (Maybe Handle,Maybe Handle,Maybe Handle,ProcessHandle) -> IO ()
   res msg h

      | False  # unwords ["res",msg] = undefined

      | (i,Nothing,e,_) <- h = do
         stdin <- maybe (pure "no handle for stdin") hGetLine i
         stderr <- maybe (pure "no handle for stderr") hGetLine e
         putStr $ clr Blue $ "stdin: " <> stdin
         putStr $ clr Red $ "stderr: " <> stderr

      | (_,Just o,_,_) <- h = do
         eof <- hIsEOF o
         if eof then do
            _ <- mem "assistant" msg
            putStr "\n"
         else do
            l <- hGetLine o
            s <- say l
            res s h

   say :: String -> IO String
   say load

      | null load = pure ""

      | Just (json :: Response Choice) <- decodeStrict $ encodeUtf8 $ T.pack load = do
         let msg = maybe "" id $ content $ message $ head $ choices json
         putStrLn $ clr Magenta msg
         pure msg

      | ("data: ",d) <- splitAt 6 load
      , Just (json :: Response Delta) <- decodeStrict $ encodeUtf8 $ T.pack d = do
         let msg = maybe "" id $ content $ delta $ head $ choices json
         putStr $ clr Magenta msg
         pure msg

      | "data: [DONE]" <- load = putStr "\n" >> pure ""
      | otherwise = putStrLn (clr Red load) >> pure ""

   mem :: String -> String -> IO [Message]
   mem r c = do
      let file = "mem/" <> sess <> ".json"
      p :: [Message] <- maybe (error "mem") id <$> decodeFileStrict file
      let past = p <> [Message { role = Just r , content = Just c , name = Just sess }]
      if null c then do
         pure past
      else do
         encodeFile file past
         pure past

   writeMem :: String -> DList Message -> IO ()
   writeMem r d = do
      -- d will be a DList in memory instead of:
      -- past <> [Message { role = Just r , content = Just c , name = Just sess }]
      -- turn into YYYYMMDD-N.json
      let file = "mem/" <> sess <> ".json"
      encodeFile file d
      pure ()

   readMem :: IO [Message]
   readMem = do
      -- turn into files :: [file]
      let file = "mem/" <> sess <> ".json"
      maybe (error "readMem") id <$> decodeFileStrict file

