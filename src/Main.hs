module Main where

import React.Flux

main :: IO ()
main = reactRender "main" lolo ()

lolo :: ReactView ()
lolo = defineView "lololo" $ \() ->
  h1_ [] $ elemText "Hello, world!"
