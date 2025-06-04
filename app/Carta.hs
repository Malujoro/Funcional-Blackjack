module Carta (
  Carta (..),
  Mao,
  todosNaipes,
  todosValores,
  showMao,
  somarMao,
) where

-- Tipo algébrico que representa os naipes de uma carta
data Naipe = Copas | Espadas | Ouros | Paus

-- Função para retornar todos os naipes
todosNaipes :: [Naipe]
todosNaipes = [Copas, Espadas, Ouros, Paus]

-- Tipo algébrico que representa os valores de uma quarta
data Valor = A | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | J | Q | K deriving (Eq)

-- Função para retornar todos os valores
todosValores :: [Valor]
todosValores = [A, Dois, Tres, Quatro, Cinco, Seis, Sete, Oito, Nove, Dez, J, Q, K]

-- Tipo algébrico que representa uma carta
data Carta = Carta Valor Naipe

-- Define um sinônimo de tipo para uma list de cartas
type Mao = [Carta]

-- Define como exibir cada naipe
instance Show Naipe where
  show Copas = "♥"
  show Espadas = "♠"
  show Ouros = "♦"
  show Paus = "♣"

-- Define como exibir os valores
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

-- Define como exibir uma carta
instance Show Carta where
  show (Carta valor naipe) = "[" ++ show valor ++ " de " ++ show naipe ++ "]"

-- Função para exibir todas as cartas de uma mão
-- Recebe a mão
-- Retorna uma String com todas as cartas
showMao :: Mao -> String
showMao [item] = show item
showMao (cabeca : cauda) = do
  show cabeca ++ ", " ++ showMao cauda

-- Classe de tipo que seja valorável
class Valoravel a where
  valorNumerico :: a -> Int

-- Instancia o valorNumerico para o tipo Int
instance Valoravel Int where
  valorNumerico n = n

-- Instancia o valorNumerico para o tipo Valor
instance Valoravel Valor where
  valorNumerico A = 1
  valorNumerico J = 10
  valorNumerico Q = 10
  valorNumerico K = 10
  valorNumerico val = read (show val) :: Int

-- Instancia o valorNumerico para o tipo Carta
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

-- Função para verificar se uma carta é reconhecida como 10
-- Recebe a carta
-- Retorna um booleano
temCarta10 :: Carta -> Bool
temCarta10 (Carta v _) = valorNumerico v == 10

-- Função para buscar se existe um A em uma mão
-- Recebe a mão
-- Retorna um booleano 
existeA :: Mao -> Bool
existeA [] = False
existeA [Carta A _] = True
existeA (_ : cauda) = existeA cauda

-- Função para somar todas as cartas de uma mão (aplicando a troca de valor do A quando necessário)
-- Recebe a mão
-- Retorna o resultado da soma
somarMao :: Mao -> Int
somarMao cartas
  | length cartas == 2 && existeA cartas &&
    (temCarta10 (head cartas) || temCarta10 (cartas !! 1)) = 21
  | otherwise = sum (map valorNumerico cartas)