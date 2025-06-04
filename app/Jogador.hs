module Jogador (
  Jogador (..),
  criarJogador,
  addHistorico,
  apostar,
  saldoAtual,
  exibirHistorico,
  exibirMaoFinal,
  limparMao,
  assinarContrato,
  pegarCarta,
) where

import Carta
import Desenho
import Utils
import Text.Printf (printf)

-- Define o tipo de dado nomeado do jogador
data Jogador = Jogador
    { nome :: String,
      mao :: Mao,
      historico :: [Float],
      contratado :: Bool
    } deriving Show

-- Função para criar um jogador
-- Recebe o nome e o saldo
-- Retorna o jogador criado
criarJogador :: String -> Float -> Jogador
criarJogador name saldo = Jogador name [] [saldo] False

-- Função para adicionar um valor ao histórico do jogador
-- Recebe o jogador e o valor
-- Retorna o jogador atualizado
addHistorico :: Jogador -> Float -> Jogador
addHistorico jogador valor = jogador {historico = historico jogador ++ [valor]}

-- Função para adicionar uma aposta ao histórico do jogador
-- Recebe o jogador e o valor
-- Retorna o jogador atualizado
apostar :: Jogador -> Float -> IO Jogador
apostar jogador valor 
  | valor > 0 = return (addHistorico jogador (-valor))
  | valor < 0 = return (addHistorico jogador valor)
  | otherwise = return jogador

-- Função para calcular o saldo atual
-- Recebe o historico
-- Retorna o saldo atual 
saldoAtual :: [Float] -> Float
saldoAtual = sum

-- Função para exibir (formatado) os ganhos e perdas do histórico
-- Recebe o historico e um número indicador
exibirGanhosPerdas :: [Float] -> Int -> IO ()
exibirGanhosPerdas [] _ = return ()
exibirGanhosPerdas (valor : cauda) i = do
  let prefixo = if even i then "Aposta" else "Resultado"
      sufixo = if even i then " | " else "\n"
  printf "%s: R$%.2f%s" prefixo valor sufixo
  exibirGanhosPerdas cauda (i+1)

-- Função para exibir o histórico completo
-- Recebe o histórico
exibirHistorico :: [Float] -> IO ()
exibirHistorico [] = return ()
exibirHistorico (cabeca : cauda) = do
  printf "Saldo inicial: R$%.2f\n" cabeca
  exibirGanhosPerdas cauda 0
  printf "Saldo atual: R$%.2f\n" (saldoAtual (cabeca : cauda))
  voltar

-- Função para exibir uma mão
-- Recebe o texto a ser exibido antes da mão e o jogador
exibirMaoFinal :: String -> Jogador -> IO ()
exibirMaoFinal texto jogador = do
  putStrLn (texto ++ show (somarMao (mao jogador)))
  putStrLn (showMao (mao jogador))

-- Função para limpar a mão de um jogador
-- Recebe o jogador
-- Retorna o jogador atualizado
limparMao :: Jogador -> Jogador
limparMao jogador = jogador {mao = []}

-- Função para um jogador assinar o contrato
-- Recebe o jogador
-- Retorna o jogador atualizado
assinarContrato :: Jogador -> IO Jogador
assinarContrato jogador 
  | not (contratado jogador) && saldoAtual (historico jogador) == 0 = do
    contrato
    return jogador {contratado = True}
  | otherwise = return jogador

-- Função para adicionar uma carta na mão do jogador
-- Recebe o jogador e a carta
-- Retorna o jogador atualizado
pegarCarta :: Jogador -> Carta -> Jogador
pegarCarta jogador carta = jogador {mao = mao jogador ++ [carta]}