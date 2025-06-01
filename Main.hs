module Main where

import Carta
import Jogador

main :: IO ()
main = do
    let teste = Jogador "Cristina" [Carta Tres Ouros, Carta Seis Paus, Carta J Espadas] [2000, -500]
    print (teste)