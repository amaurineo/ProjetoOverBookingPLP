module Util where
import System.IO
import Data.List

headCadastrado :: String -> [String] -> Bool
headCadastrado c (x:xs) = (c == x)

aux :: String -> [String] -> Bool
aux v (x:xs) = (v == x)

escreverHorarioCpf :: String -> IO()
escreverHorarioCpf n = do

    arq <- openFile "arquivos/horario-cpf.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s

--- FUNÇÕES QUE GERAM STRING NO FORMATO DE ESCRITA DE UM ARQUIVO ---
primeiraHorarioCpf :: [[String]] -> String
primeiraHorarioCpf [] = ""
primeiraHorarioCpf (x:xs) = head x ++ "," ++ (x !! 1) ++ "\n" ++ primeiraHorarioCpf xs


escreveFuncionario :: String -> IO()
escreveFuncionario n = do

    arq <- openFile "arquivos/funcionarios.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq


--- GERA UMA LISTA DE LISTA SEM A LISTA QUE CONTEM O STRING PASSADO COMO PARÂMETRO ---
opcaoVaga :: String -> [[String]] -> [[String]]
opcaoVaga _ [] = []
opcaoVaga v (x:xs) | (aux v x) == True = opcaoVaga v xs
                   | otherwise = x:opcaoVaga v xs

parseToInt2 :: String -> Int
parseToInt2 s = read (s) :: Int

escreveAssento :: String -> IO()
escreveAssento n = do

    arq <- openFile "arquivos/assentos.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq

escreveDesconto :: String -> IO()
escreveDesconto n = do

    arq <- openFile "arquivos/descontos.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq

temCadastro :: String -> [[String]] -> Bool
temCadastro _ [] = False
temCadastro c (x:xs) | not (headCadastrado c x)  = temCadastro c xs
                   | otherwise = True


getNome :: String -> [[String]] -> String
getNome _ [] = ""
getNome c (x:xs)   | ((headCadastrado c x) == False) = getNome c xs
                   | otherwise = x !! 1

lerEntradaString :: IO String
lerEntradaString = do
    getLine



split     :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'