module Main where

import Config
import Env
import Server

import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  config <- mkConfig
  let env =
        Env
        { envDBPool = cfgPool config
        , envLogAction = cfgLogAction config
        }
  run 8080 (app env)
