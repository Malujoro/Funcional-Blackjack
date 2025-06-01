module Jogador (
    Jogador (..)
) where

import Carta

data Jogador = Jogador
    { nome :: String,
      mao :: Mao,
      historico :: [Int]
    } deriving Show