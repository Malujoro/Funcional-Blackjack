module Main where

import Carta
import Jogador
import System.Random (randomRIO)
import Data.Char (isSpace)

saldoAtual :: [Float] -> Float
saldoAtual = sum

somarMao :: Mao -> Int
somarMao [Carta valor1 _, Carta valor2 _]
  | (valor1 == A && valorNumerico valor2 == 10) ||
    (valor2 == A && valorNumerico valor1 == 10) = 21
  | otherwise = valorNumerico valor1 + valorNumerico valor2
somarMao cartas = sum (map valorNumerico cartas)

data Resultado = VitoriaBlackjack | Vitoria | Empate | Derrota deriving (Eq)

data Situacao = Blackjack | VinteUm | Estouro | Incompleto deriving (Eq)

verificarSituacao :: Mao -> Situacao
verificarSituacao cartas =
  let total = somarMao cartas
   in if total > 21 then Estouro
      else if total == 21 && length cartas == 2 then Blackjack
      else if total == 21 then VinteUm
      else Incompleto

verificarVitoria :: Mao -> Mao -> Resultado
verificarVitoria jogadorMao dealerMao =
  let totalJogador = somarMao jogadorMao
      totalDealer = somarMao dealerMao
   in if totalJogador > totalDealer then Vitoria
      else if totalJogador == totalDealer then Empate
      else Derrota

verificarResultado :: Jogador -> Jogador -> Resultado
verificarResultado jogador dealer =
  let situacaoJogador = verificarSituacao (mao jogador)
      situacaoDealer = verificarSituacao (mao dealer)
   in if situacaoJogador == Incompleto && situacaoDealer == Incompleto then verificarVitoria (mao jogador) (mao dealer)
      else if situacaoJogador == situacaoDealer then Empate
      else if situacaoJogador == Blackjack then VitoriaBlackjack
      else if situacaoDealer == Blackjack || situacaoDealer == VinteUm || situacaoJogador == Estouro then Derrota
      -- else if situacaoJogador == VinteUm || situacaoDealer == Estouro then 
      else Vitoria

limparMao :: Jogador -> Jogador
limparMao jogador = jogador {mao = []}

criarBaralho :: Mao
criarBaralho = [Carta valor naipe | valor <- todosValores, naipe <- todosNaipes]

-- Ao comprar a carta o jogador pega a carta
pegarCarta :: Jogador -> Carta -> Jogador
pegarCarta jogador carta = jogador {mao = mao jogador ++ [carta]}

comprarCarta :: Mao -> IO (Carta, Mao)
comprarCarta baralho = do
  let len = length baralho
  if len == 0
    then error "Baralho vazio!"
    else do
      randomNu <- randomRIO (0, len - 1)
      let carta = baralho !! randomNu
          novoBaralho = take randomNu baralho ++ drop (randomNu + 1) baralho
      return (carta, novoBaralho)

comprarCartaParaJogador :: Jogador -> Mao -> IO (Jogador, Mao)
comprarCartaParaJogador jogador baralho = do
  (carta, novoBaralho) <- comprarCarta baralho
  let novoJogador = pegarCarta jogador carta
  return (novoJogador, novoBaralho)

jogadaDealer :: Jogador -> Mao -> IO (Jogador, Mao)
jogadaDealer dealer baralho
  | somarMao (mao dealer) >= 17 = return (dealer, baralho)
  | otherwise = do
      (novoDealer, novoBaralho) <- comprarCartaParaJogador dealer baralho
      jogadaDealer novoDealer novoBaralho

-- TODO: Interface (menu e scanf)
-- TODO: E apostar

addHistorico :: Jogador -> Float -> Jogador
addHistorico jogador valor = jogador {historico = historico jogador ++ [valor]}

apostar :: Jogador -> Float -> Jogador
apostar jogador valor 
  | valor > 0 = addHistorico jogador (-valor)
  | valor < 0 = addHistorico jogador valor
  | otherwise = jogador

atualizarSaldo :: Jogador -> Resultado -> Float -> Jogador
atualizarSaldo jogador resultado aposta =
  case resultado of
    VitoriaBlackjack -> addHistorico jogador (2.5 * aposta)
    Vitoria -> addHistorico jogador (2 * aposta)
    Empate -> addHistorico jogador aposta
    Derrota -> addHistorico jogador 0

trim :: String -> String
trim = funcao . funcao where funcao = reverse . dropWhile isSpace

obterNome :: IO String
obterNome = do
  print "Digite seu nome:"
  name <- getLine
  if null (trim name) then do
    print "Nome inválido! Tente novamente"
    obterNome
  else return name

criarJogador :: String -> Jogador
criarJogador name = Jogador name [] []
    

main :: IO ()
main = do

  -- let name = obterNome
  let name = "Cristina"

  let player = criarJogador name
  let dealer = criarJogador "dealer"

  print player
  print dealer

  let new = addHistorico player 100
  print new

  let new2 = apostar new 200
  print new2

-- print teste
-- let novoJogador = pegarCarta teste (Carta A Copas)
-- print novoJogador
-- let estourou = verificarEstouro novoJogador
-- putStrLn $ if estourou
--     then "O jogador estourou!"
--     else "o jogador fez o L 🤣🤣🤣"