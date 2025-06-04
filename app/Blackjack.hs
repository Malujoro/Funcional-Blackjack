module Blackjack (
  saldoInicial,
  apostaMaxima,
  iniciarJogo,
) where

import Carta
import Jogador
import Utils
import System.Random (randomRIO)

-- Tipo algébrico que representa os possíveis estados do jogo
data Resultado = VitoriaBlackjack | Vitoria | Empate | Derrota deriving (Eq)

-- Tipo algébrico que representa as possíveis situações de um jogador
data Situacao = Blackjack | VinteUm | Estouro | Incompleto deriving (Eq)

-- Função para definir o valor máximo para o saldo inicial
saldoInicial :: Float
saldoInicial = 500

-- Função para atualizar o histórico do jogador dependendo do resultado do seu jogo
-- Recebe o jogador, o resultado, o valor da aposta
-- Retorna o jogador atualizado
atualizarSaldo :: Jogador -> Resultado -> Float -> IO Jogador
atualizarSaldo jogador resultado aposta =
  case resultado of
    VitoriaBlackjack -> return (addHistorico jogador (2.5 * aposta))
    Vitoria -> return (addHistorico jogador (2 * aposta))
    Empate -> return (addHistorico jogador aposta)
    Derrota -> return (addHistorico jogador 0)

-- Função para definir o valor máximo de aposta permitido
-- Recebe o saldo do jogador
-- Retorna o valor máximo permitido
apostaMaxima :: Float -> Float
apostaMaxima 0 = saldoInicial
apostaMaxima saldo
  | saldo == 0 = saldoInicial
  | saldo < 0 = max saldoInicial (arredondar2 (saldo / 2  * (-1)))
  | saldo > 0 = saldo

-- Função para verificar a situação de uma mão
-- Recebe a mão
-- Retorna a situação
verificarSituacao :: Mao -> Situacao
verificarSituacao cartas =
  let total = somarMao cartas
   in if total > 21 then Estouro
      else if total == 21 && length cartas == 2 then Blackjack
      else if total == 21 then VinteUm
      else Incompleto

-- Função para verificar o resultado de uma partida (com mãos incompletas)
-- Recebe a mão do jogador e a mão do dealer
-- Retorna o resultado
verificarVitoria :: Mao -> Mao -> Resultado
verificarVitoria jogadorMao dealerMao =
  let totalJogador = somarMao jogadorMao
      totalDealer = somarMao dealerMao
   in if totalJogador > totalDealer then Vitoria
      else if totalJogador == totalDealer then Empate
      else Derrota

-- Função para verificar o resultado de uma partida
-- Recebe o jogador e o dealer
-- Retorna o resultado
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

-- Função para criar um baralho com listas compreensivas
-- Retorna o baralho criado
criarBaralho :: Mao
criarBaralho = [Carta valor naipe | valor <- todosValores, naipe <- todosNaipes]

-- Função para comprar uma carta do baralho
-- Recebe o baralho
-- Retorna uma tupla com a carta retirada e o baralho atualizado
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

-- Função para o jogador comprar uma carta
-- Recebe o jogador e o baralho
-- Retorna o jogador e o baralho atualizados
comprarCartaParaJogador :: Jogador -> Mao -> IO (Jogador, Mao)
comprarCartaParaJogador jogador baralho = do
  (carta, novoBaralho) <- comprarCarta baralho
  let novoJogador = pegarCarta jogador carta
  return (novoJogador, novoBaralho)

-- Função para executar as ações do dealer (comprar seguindo as regras ou passar a vez)
-- Recebe o dealer, o baralho e um booleano (indica se o dealer deve comprar até atingir suas regras)
-- Retorna o dealer e o baralho atualizados
jogadaDealer :: Jogador -> Mao -> Bool -> IO (Jogador, Mao)
jogadaDealer dealer baralho finalizar
  | somarMao (mao dealer) >= 17 = return (dealer, baralho)
  | otherwise = do
    (novoDealer, novoBaralho) <- comprarCartaParaJogador dealer baralho
    if finalizar then jogadaDealer novoDealer novoBaralho finalizar
    else return (novoDealer, novoBaralho)

-- Função para fazer todos os jogadores comprarem uma quantia de cartas
-- Recebe o jogador, o dealer, o baralho e a quantidade e cartas
-- Retorna o jogador, o dealer e o baralho atualizados
comprarParaTodos :: Jogador -> Jogador -> Mao -> Int -> IO (Jogador, Jogador, Mao)
comprarParaTodos jogador dealer baralho quant 
  | quant > 0 = do
    (jogador, baralho) <- comprarCartaParaJogador jogador baralho
    (dealer, baralho) <- jogadaDealer dealer baralho False
    comprarParaTodos jogador dealer baralho (quant-1)
  | otherwise = return (jogador, dealer, baralho)

-- Função para iniciar o jogo
-- Recebe o jogador
-- Retorna o jogador com mão limpa e histórico atualizado
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

-- Função para executar a lógica da rodada
-- Recebe o jogador, o dealer e o baralho
-- Retorna o jogador e o dealer atualizados
rodada :: Jogador -> Jogador -> Mao -> IO (Jogador, Jogador)
rodada jogador dealer baralho = do
  exibirTela

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
