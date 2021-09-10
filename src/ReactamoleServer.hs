-- |
-- Module      :  Main
-- Copyright   :  (c) DigMP Research Group 2021
-- License     :  MIT
--
-- Simple web server that supports querying the Reactamole DSL.

{-# LANGUAGE DataKinds, DeriveGeneric, TemplateHaskell, TypeOperators   #-}

module Main where

import Bio.Reactamole
import Bio.Reactamole.Export
import Data.Aeson
import Data.Aeson.TH
import Data.List (isPrefixOf)
import Control.Monad.IO.Class
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment

import qualified Language.Haskell.Interpreter as Hint
import qualified Data.List as L
import qualified Data.Text as T

----- Server Logic {{{ ---------------------------------------------------------

strip :: String -> String
strip = T.unpack . T.strip . T.pack

parseDecls :: String -> [String]
parseDecls = collapse Nothing . map strip . lines
  where
    collapse :: Maybe String -> [String] -> [String]
    collapse Nothing     [] = []
    collapse (Just stmt) [] = [stmt]
    collapse Nothing     (l:ls)
      | "let" `isPrefixOf` l = collapse (Just l) ls
      | otherwise            = l : collapse Nothing ls  -- N.B., bad, but Hint will catch it
    collapse (Just stmt) (l:ls)
      | "let" `isPrefixOf` l = stmt : collapse (Just l) ls
      | otherwise            = collapse (Just $ stmt ++ " " ++ l) ls

evalProg :: String -> String -> IO (Either Hint.InterpreterError String)
evalProg decls expr = Hint.runInterpreter $ do
  Hint.set [Hint.languageExtensions Hint.:= [Hint.GADTs, Hint.ScopedTypeVariables]]
  Hint.setImports [ "Prelude"
                  , "Bio.Reactamole"
                  , "Bio.Reactamole.ArrChoice"
                  , "Bio.Reactamole.Examples"
                  , "Bio.Reactamole.Export"
                  ]
  mapM_ Hint.runStmt (parseDecls decls)
  Hint.eval $ combineLines expr
    where
      combineLines :: String -> String
      combineLines =
        unwords
        . filter (not . ("--" `isPrefixOf`))
        . filter (not . null)
        . map strip
        . lines

----- }}} ----------------------------------------------------------------------

----- API+Server Definition {{{ ------------------------------------------------

data EvaluateA = EvaluateA { decls :: String, expr :: String }
  deriving Generic

instance FromJSON EvaluateA

data EvaluateR = EvaluateR { success :: Bool, output :: String }
  deriving Generic

instance ToJSON EvaluateR

type API = "evaluate" :> ReqBody '[JSON] EvaluateA :> Post '[JSON] EvaluateR
      :<|> "static" :> Raw

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = result :<|> serveDirectoryWebApp "static"
  where
    result :: EvaluateA -> Handler EvaluateR
    result (EvaluateA decls expr) = do
      result <- liftIO $ evalProg decls expr
      pure $ case result of
        Left err     -> EvaluateR False (show err)
        Right output -> EvaluateR True output

----- }}} ----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then
    putStrLn "Usage: reactamole-server <port>"
  else
    run (read (args !! 0) :: Int) app
