module Main where

import Carta
import Jogador
import Logo
import System.Random (randomRIO)
import Data.Char (isSpace)
import Text.Read (readMaybe)
import System.Info (os)
import System.Process (callCommand)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

limparTela :: IO ()
limparTela = callCommand $ if os == "mingw32" then "cls" else "clear"

voltar :: IO ()
voltar = do
  putStr "\nPressione qualquer tecla para voltar\n"
  hFlush stdout
  getLine
  return ()

saldoInicial :: Float
saldoInicial = 500

limiteNegativo :: Float
limiteNegativo = -100

apostaMaxima :: Float -> Float
apostaMaxima 0 = saldoInicial
apostaMaxima saldo
  | saldo == 0 = saldoInicial
  | saldo < 0 = (saldo / 2) * (-1)
  | saldo > 0 = saldo

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


pegarCarta :: Jogador -> Carta -> Jogador
pegarCarta jogador carta = jogador {mao = mao jogador ++ [carta]}


comprarCarta :: Mao -> IO (Carta, Mao)
comprarCarta baralho = do
  let len = length baralho
  if len == 0
    then error "\nBaralho vazio!\n"
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


jogadaDealer :: Jogador -> Mao -> Bool -> IO (Jogador, Mao)
jogadaDealer dealer baralho finalizar
  | somarMao (mao dealer) >= 17 = return (dealer, baralho)
  | otherwise = do
    (novoDealer, novoBaralho) <- comprarCartaParaJogador dealer baralho
    if finalizar then jogadaDealer novoDealer novoBaralho finalizar
    else return (novoDealer, novoBaralho)


comprarParaTodos :: Jogador -> Jogador -> Mao -> Int -> IO (Jogador, Jogador, Mao)
comprarParaTodos jogador dealer baralho quant 
  | quant > 0 = do
    (jogador, baralho) <- comprarCartaParaJogador jogador baralho
    (dealer, baralho) <- jogadaDealer dealer baralho False
    comprarParaTodos jogador dealer baralho (quant-1)
  | otherwise = return (jogador, dealer, baralho)


addHistorico :: Jogador -> Float -> Jogador
addHistorico jogador valor = jogador {historico = historico jogador ++ [valor]}


apostar :: Jogador -> Float -> IO Jogador
apostar jogador valor 
  | valor > 0 = return (addHistorico jogador (-valor))
  | valor < 0 = return (addHistorico jogador valor)
  | otherwise = return jogador


atualizarSaldo :: Jogador -> Resultado -> Float -> IO Jogador
atualizarSaldo jogador resultado aposta =
  case resultado of
    VitoriaBlackjack -> return (addHistorico jogador (2.5 * aposta))
    Vitoria -> return (addHistorico jogador (2 * aposta))
    Empate -> return (addHistorico jogador aposta)
    Derrota -> return (addHistorico jogador 0)

trim :: String -> String
trim = funcao . funcao where funcao = reverse . dropWhile isSpace

lerString :: String -> IO String
lerString texto = do
  putStr texto
  hFlush stdout
  input <- getLine

  if null (trim input) then do
    putStrLn "\nEntrada invalida! Tente novamente\n"
    lerString texto
  else return input

lerFloat :: String -> Float -> IO Float
lerFloat texto maximo = do
  input <- lerString texto

  case readMaybe input :: Maybe Float of
    Just valor | valor > 0 && valor <= maximo -> return valor
    _ -> do
      putStrLn ("\nEntrada invalida! Digite um número maior que 0 e menor ou igual a " ++ show maximo ++ "\n")
      lerFloat texto maximo

criarJogador :: String -> Float -> Jogador
criarJogador name saldo = Jogador name [] [saldo]
    

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

iniciarJogo :: Jogador -> IO Jogador
iniciarJogo jogador = do
  let saldo = saldoAtual (historico jogador)
  valorApostado <- lerFloat "Valor apostado: " (apostaMaxima saldo)
  jogador <- apostar jogador valorApostado 

  let dealer = criarJogador "dealer" 0
  let baralho = criarBaralho

  (jogador, dealer, baralho) <- comprarParaTodos jogador dealer baralho 2
  (jogador, dealer) <- rodada jogador dealer baralho

  let resultado = verificarResultado jogador dealer

  jogador <- atualizarSaldo jogador resultado valorApostado

  exibirMaoFinal "\nSua mão deu " jogador
  exibirMaoFinal "\nA mão do dealer deu " dealer

  case resultado of
    VitoriaBlackjack -> putStr "\nParabéns, você ganhou com um BLACKJACK\n"
    Vitoria -> putStr "\nParabéns, você ganhou\n"
    Empate -> putStr "\nOcorreu um empate\n"
    Derrota -> putStr "\nQue pena, você perdeu! Tente recuperar na próxima\n"

  voltar

  return (limparMao jogador)

rodada :: Jogador -> Jogador -> Mao -> IO (Jogador, Jogador)
rodada jogador dealer baralho = do
  putStrLn "\nOPÇÕES"
  putStrLn "[1] - Comprar"
  putStrLn "[0] - Parar"
  putStrLn ("Mão atual: " ++ showMao (mao jogador))
  putStrLn ("Soma: " ++ show (somarMao (mao jogador)))

  op <- lerString "Opção: "

  case op of
    "1" -> do
      (jogador, dealer, baralho) <- comprarParaTodos jogador dealer baralho 1
      rodada jogador dealer baralho
    "0" -> do
      putStrLn "\nParando de jogar..."
      (dealer, baralho) <- jogadaDealer dealer baralho True
      return (jogador, dealer)
    _ -> do
      putStrLn "\nOpção inválida! Tente novamente"
      rodada jogador dealer baralho


menu :: Jogador -> IO ()
menu jogador = do
  let saldo = saldoAtual (historico jogador)

  desenho1
  logo

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
  -- limparTela

  name <- lerString "Digite seu nome: "

  saldo <- lerFloat "Saldo inicial: " saldoInicial
  -- let saldo = saldoInicial

  let player = criarJogador name saldo

  menu player