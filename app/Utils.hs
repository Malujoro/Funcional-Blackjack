module Utils (
  limparTela,
  voltar,
  exibirTela,
  lerString,
  arredondar2,
  lerFloat,
) where

import Data.Char (isSpace)
import Desenho
import Text.Read (readMaybe)
import System.Info (os)
import System.Process (callCommand)
import System.IO (hFlush, stdout)

-- Função para limpar a tela dependendo do sistema operacional
limparTela :: IO ()
limparTela = callCommand $ if os == "mingw32" then "cls" else "clear"

-- Função para exibir a ação de voltar
voltar :: IO ()
voltar = do
  putStr "\nPressione qualquer tecla para voltar\n"
  hFlush stdout
  getLine
  return ()

-- Função para exibir os desenhos da tela
exibirTela :: IO ()
exibirTela = do
  limparTela
  desenho
  logo

-- Função para remover espaços no início e fim de uma string
-- Recebe uma string
-- Retorna a string tratada
trim :: String -> String
trim = funcao . funcao where funcao = reverse . dropWhile isSpace

-- Função para ler uma string do usuário que seja não vazia
-- Recebe o texto a ser exibido
-- Retorna a string lida
lerString :: String -> IO String
lerString texto = do
  putStr texto
  hFlush stdout
  input <- getLine

  if null (trim input) then do
    putStrLn "\nEntrada invalida! Tente novamente\n"
    lerString texto
  else return input

-- Função para arredondar um valor decimal em duas casas
-- Recebe o valor
-- Retorna o valor tratado
arredondar2 :: Float -> Float
arredondar2 x = fromIntegral (round (x * 100)) / 100

-- Função para ler um float do usuário que seja positivo e com limite máximo
-- Recebe o texto a ser exibido
-- Retorna o float lido
lerFloat :: String -> Float -> IO Float
lerFloat texto maximo = do
  input <- lerString texto

  case readMaybe input :: Maybe Float of
    Just valor | valor > 0 && valor <= maximo -> return (arredondar2 valor)
    _ -> do
      putStrLn ("\nEntrada invalida! Digite um número maior que 0 e menor ou igual a " ++ show maximo ++ "\n")
      lerFloat texto maximo
