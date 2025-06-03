module Main where

import Carta
import Jogador
import System.Random (randomRIO)

saldoAtual :: [Int] -> Int
saldoAtual = sum

somarMao :: Mao -> Int
somarMao [Carta valor1 _, Carta valor2 _]
  | (valor1 == A && valorNumerico valor2 == 10)
      || (valor2 == A && valorNumerico valor1 == 10) =
      21
  | otherwise = valorNumerico valor1 + valorNumerico valor2
somarMao mao = sum (map valorNumerico mao)

data Resultado = Vitoria | Empate | Derrota deriving (Eq)

data Situacao = VinteUm | Estouro | Incompleto deriving (Eq)

verificarSituacao :: Mao -> Situacao
verificarSituacao mao =
  let total = somarMao mao
   in if total > 21
        then Estouro
        else
          if total == 21
            then VinteUm
            else Incompleto

verificarVitoria :: Mao -> Mao -> Resultado
verificarVitoria jogadorMao dealerMao =
  let totalJogador = somarMao jogadorMao
      totalDealer = somarMao dealerMao
   in if totalJogador > totalDealer
        then Vitoria
        else
          if totalJogador == totalDealer
            then Empate
            else Derrota

verificarResultado :: Jogador -> Jogador -> Resultado
verificarResultado jogador dealer =
  let situacaoJogador = verificarSituacao (mao jogador)
      situacaoDealer = verificarSituacao (mao dealer)
   in if situacaoJogador == Incompleto && situacaoDealer == Incompleto
        then
          verificarVitoria (mao jogador) (mao dealer)
        else
          if situacaoJogador == situacaoDealer
            then Empate
            else
              if situacaoJogador == VinteUm || situacaoDealer == Estouro
                then Vitoria
                -- situacaoDealer == VinteUm || situacaoJogador == Estouro
                else Derrota

addHistorico :: Jogador -> Int -> Jogador
addHistorico (Jogador nome mao historico) valor =
  Jogador nome mao (historico ++ [valor])

limparBaralho :: Jogador -> Jogador
limparBaralho (Jogador nome mao historico) =
  Jogador nome [] historico

criarBaralho :: Mao
criarBaralho = [Carta valor naipe | valor <- todosValores, naipe <- todosNaipes]

-- TODO: Estado de blackjack

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

atualizarSaldo :: Jogador -> Resultado -> Int -> Jogador
atualizarSaldo jogador resultado aposta =
  case resultado of
    Vitoria -> addHistorico jogador (2 * aposta)
    Empate -> addHistorico jogador aposta
    Derrota -> jogador

main :: IO ()
main = do
  let player = Jogador "Cristina" [] []
  let dealer = Jogador "Dealer" [] []

  print player
  print dealer

  let new = addHistorico player 100
  print new

-- print teste
-- let novoJogador = pegarCarta teste (Carta A Copas)
-- print novoJogador
-- let estourou = verificarEstouro novoJogador
-- putStrLn $ if estourou
--     then "O jogador estourou!"
--     else "o jogador fez o L 🤣🤣🤣"