--cabal install ansi-terminal
module Limpar_tela where

import System.Console.ANSI

main = putStr "\ESC[2J"
