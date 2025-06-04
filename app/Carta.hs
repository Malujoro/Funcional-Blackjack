module Carta (
  Carta (..),
  Mao,
  todosNaipes,
  todosValores,
  showMao,
  somarMao,
) where

data Naipe = Copas | Espadas | Ouros | Paus

todosNaipes :: [Naipe]
todosNaipes = [Copas, Espadas, Ouros, Paus]

data Valor = A | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | J | Q | K deriving (Eq)

todosValores :: [Valor]
todosValores = [A, Dois, Tres, Quatro, Cinco, Seis, Sete, Oito, Nove, Dez, J, Q, K]

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

showMao :: Mao -> String
showMao [item] = show item
showMao (cabeca : cauda) = do
  show cabeca ++ ", " ++ showMao cauda

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

instance Valoravel Carta where
  valorNumerico (Carta valor _) = case valor of
    A -> 1
    Dois -> 2
    Tres -> 3
    Quatro -> 4
    Cinco -> 5
    Seis -> 6
    Sete -> 7
    Oito -> 8
    Nove -> 9
    Dez -> 10
    J -> 10
    Q -> 10
    K -> 10

somarMao :: Mao -> Int
somarMao [Carta valor1 _, Carta valor2 _]
  | (valor1 == A && valorNumerico valor2 == 10) ||
    (valor2 == A && valorNumerico valor1 == 10) = 21
  | otherwise = valorNumerico valor1 + valorNumerico valor2
somarMao cartas = sum (map valorNumerico cartas)