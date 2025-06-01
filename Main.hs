module Main where

import Carta
import Jogador

pegarCarta :: Jogador -> Carta -> Jogador
pegarCarta jogador carta = jogador {mao = mao jogador ++ [carta]}

saldoAtual :: [Int] -> Int
saldoAtual = sum

somarMao :: Mao -> Int
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