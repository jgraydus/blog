module Main where

import DevServer (defaultDevServerConfig, DevServerConfig(..), runDevServer)

main :: IO ()
main = do
  let config = defaultDevServerConfig
               { serverExeName = "blog"
               , clientFileExtensions = ["js", "jsx"]
               }
  runDevServer config

