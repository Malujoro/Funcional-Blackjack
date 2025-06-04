module Utils (
  limparTela,
  voltar,
  exibirTela,
  lerString,
  arredondar2,
  lerFloat,
) where

import Logo
import Data.Char (isSpace)
import Text.Read (readMaybe)
import System.Info (os)
import System.Process (callCommand)
import System.IO (hFlush, stdout)


limparTela :: IO ()
limparTela = callCommand $ if os == "mingw32" then "cls" else "clear"

voltar :: IO ()
voltar = do
  putStr "\nPressione qualquer tecla para voltar\n"
  hFlush stdout
  getLine
  return ()

exibirTela :: IO ()
exibirTela = do
  limparTela
  desenho1
  logo

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

arredondar2 :: Float -> Float
arredondar2 x = fromIntegral (round (x * 100)) / 100

lerFloat :: String -> Float -> IO Float
lerFloat texto maximo = do
  input <- lerString texto

  case readMaybe input :: Maybe Float of
    Just valor | valor > 0 && valor <= maximo -> return (arredondar2 valor)
    _ -> do
      putStrLn ("\nEntrada invalida! Digite um número maior que 0 e menor ou igual a " ++ show maximo ++ "\n")
      lerFloat texto maximo
