module Carta (
    Naipe(..),
    Valor(..),
    Carta(..),
    Mao,
    Valoravel(..),
) where

data Naipe = Copas | Espadas | Ouros | Paus

data Valor = A | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | J | Q | K

data Carta = Carta Valor Naipe

type Mao = [Carta]

instance Show Naipe where
    show Copas = "♥"
    show Espadas = "♠"
    show Ouros = "♦"
    show Paus = "♣"

instance Show Valor where
    show A = "A"
    show Dois = "2"
    show Tres = "3"
    show Quatro = "4"
    show Cinco = "5"
    show Seis = "6"
    show Sete = "7"
    show Oito = "8"
    show Nove = "9"
    show Dez = "10"
    show J = "J"
    show Q = "Q"
    show K = "K"

instance Show Carta where
    show (Carta valor naipe) = "[" ++ show valor ++ " de " ++ show naipe ++ "]"

class Valoravel a where
    valorNumerico :: a -> Int

instance Valoravel Int where
    valorNumerico n = n

instance Valoravel Valor where
    valorNumerico A = 1
    valorNumerico J = 10
    valorNumerico Q = 10
    valorNumerico K = 10
    valorNumerico val = read (show val) :: Int

