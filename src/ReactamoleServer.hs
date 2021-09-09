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

import Control.Monad.IO.Class

import GHC.Generics

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import System.Environment

import qualified Language.Haskell.Interpreter as Hint

----- Server Logic {{{ ---------------------------------------------------------

evalOne :: String -> IO (Either Hint.InterpreterError String)
evalOne src = Hint.runInterpreter $ do
    Hint.setImports ["Prelude", "Bio.Reactamole", "Bio.Reactamole.Export"]
    Hint.eval src

----- }}} ----------------------------------------------------------------------

----- API+Server Definition {{{ ------------------------------------------------

data EvaluateR = EvaluateR { success :: Bool, output :: String }
  deriving Generic

instance ToJSON EvaluateR

type API = "evaluate" :> ReqBody '[PlainText] String :> Post '[JSON] EvaluateR
      :<|> "static" :> Raw

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = result :<|> serveDirectoryWebApp "static"
  where
    result :: String -> Handler EvaluateR
    result src = do
      result <- liftIO $ evalOne src
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
