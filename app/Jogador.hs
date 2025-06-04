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

data Jogador = Jogador
    { nome :: String,
      mao :: Mao,
      historico :: [Float],
      contratado :: Bool
    } deriving Show

criarJogador :: String -> Float -> Jogador
criarJogador name saldo = Jogador name [] [saldo] False

addHistorico :: Jogador -> Float -> Jogador
addHistorico jogador valor = jogador {historico = historico jogador ++ [valor]}

apostar :: Jogador -> Float -> IO Jogador
apostar jogador valor 
  | valor > 0 = return (addHistorico jogador (-valor))
  | valor < 0 = return (addHistorico jogador valor)
  | otherwise = return jogador

saldoAtual :: [Float] -> Float
saldoAtual = sum

exibirGanhosPerdas :: [Float] -> Int -> IO ()
exibirGanhosPerdas [] _ = return ()
exibirGanhosPerdas (valor : cauda) i = do
  let prefixo = if even i then "Aposta" else "Resultado"
      sufixo = if even i then " | " else "\n"
  printf "%s: R$%.2f%s" prefixo valor sufixo
  exibirGanhosPerdas cauda (i+1)

exibirHistorico :: [Float] -> IO ()
exibirHistorico [] = return ()
exibirHistorico (cabeca : cauda) = do
  printf "Saldo inicial: R$%.2f\n" cabeca
  exibirGanhosPerdas cauda 0
  printf "Saldo atual: R$%.2f\n" (saldoAtual (cabeca : cauda))
  voltar

exibirMaoFinal :: String -> Jogador -> IO ()
exibirMaoFinal texto jogador = do
  putStrLn (texto ++ show (somarMao (mao jogador)))
  putStrLn (showMao (mao jogador))

limparMao :: Jogador -> Jogador
limparMao jogador = jogador {mao = []}

assinarContrato :: Jogador -> IO Jogador
assinarContrato jogador 
  | not (contratado jogador) && saldoAtual (historico jogador) == 0 = do
    contrato
    return jogador {contratado = True}
  | otherwise = return jogador


pegarCarta :: Jogador -> Carta -> Jogador
pegarCarta jogador carta = jogador {mao = mao jogador ++ [carta]}