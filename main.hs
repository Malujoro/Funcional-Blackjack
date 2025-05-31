data Naipe = Copas | Espadas | Ouros | Paus

data Valor = A | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | J | Q | K deriving Show

data Carta = Carta Valor Naipe deriving Show

type Mao = [Carta]

data Jogador = Jogador
    { nome :: String,
      mao :: Mao,
      historico :: [Int]
    } deriving Show

instance Show Naipe where
    show Copas = "♥"
    show Espadas = "♠"
    show Ouros = "♦"
    show Paus = "♣"

class Valoravel a where
    valorNumerico :: a -> Int

instance Valoravel Valor where
valorNumerico A = 1
valorNumerico Dois = 2
valorNumerico Tres = 3
valorNumerico Quatro = 4
valorNumerico Cinco = 5
valorNumerico Seis = 6
valorNumerico Sete = 7
valorNumerico Oito = 8
valorNumerico Nove = 9
valorNumerico Dez = 10
valorNumerico J = 10
valorNumerico Q = 10
valorNumerico K = 10


main = do
    let teste = Jogador "Cristina" [Carta Tres Ouros, Carta Seis Paus] [2000, -500]
    print (teste)