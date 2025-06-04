module Main where

import Blackjack
import Jogador
import Utils
import Text.Printf (printf)

menu :: Jogador -> IO ()
menu jogador = do
  exibirTela

  let saldo = saldoAtual (historico jogador)

  putStrLn "\nOPÇÕES"
  putStrLn "[1] - Jogar"
  putStrLn "[2] - Ver histórico"
  putStrLn "[0] - Sair"
  printf "Saldo atual: R$%.2f\n" saldo
  printf "Aposta máxima: R$%.2f\n" (apostaMaxima saldo)

  op <- lerString "Opção: "

  putStrLn ""

  case op of
    "1" -> do
      jogador2 <- iniciarJogo jogador
      menu jogador2
    "2" -> do
      exibirHistorico (historico jogador)
      menu jogador
    "0" -> do
      putStrLn "Saindo...\n"
      return ()
    _ -> do
      putStrLn "Opção inválida! Tente novamente"
      menu jogador



main :: IO ()
main = do
  limparTela

  name <- lerString "Digite seu nome: "

  saldo <- lerFloat "Saldo inicial: " saldoInicial
  -- let saldo = saldoInicial

  let player = criarJogador name saldo

  menu player