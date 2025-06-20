module Main where

import Blackjack
import Jogador
import Utils
import Text.Printf (printf)

-- Função para exibir o menu, obter uma opção como entrada do usuário e agir conforme ela
-- Recebe o jogador
menu :: Jogador -> IO ()
menu jogador = do
  exibirTela

  let saldo = saldoAtual (historico jogador)

  jogador <- assinarContrato jogador

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


-- Função responsável por obter os dados iniciais do jogador e começar o jogo
main :: IO ()
main = do
  limparTela

  name <- lerString "Digite seu nome: "
  saldo <- lerFloat "Saldo inicial: " saldoInicial

  putStrLn ""
  putStrLn "Aperte ENTER para iniciar o jogo de acordo com os termos"
  getLine

  let player = criarJogador name saldo

  menu player