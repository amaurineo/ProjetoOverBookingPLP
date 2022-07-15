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
    x <- getLine
    return x


split :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'


temAssento :: String -> [[String]] -> Bool
temAssento _ [] = False
temAssento c (x:xs) | not (headAssentoDisponivel c x) = temAssento c xs
                    | otherwise = True

headAssentoDisponivel :: String -> [String] -> Bool
headAssentoDisponivel _ [] = False
headAssentoDisponivel c (x:xs) = c == x

parseDicToList :: [[String]] -> [String]
parseDicToList [] = []
parseDicToList lista = head (head lista):parseDicToList (tail lista)

escreveCompra :: String -> IO()
escreveCompra n = do
    
    arq <- openFile "arquivos/compra.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq
   
escreveCliente :: String -> IO()
escreveCliente n = do
    
    arq <- openFile "arquivos/clientes.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq

escreveAssento1 :: String -> IO()
escreveAssento1 n = do

    arq <- openFile "arquivos/assentos_executivo_disponivel.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq

escreveAssento2 :: String -> IO()
escreveAssento2 n = do

    arq <- openFile "arquivos/assentos_economico_disponivel.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq

ehCadastrado :: String -> [[String]] -> Bool
ehCadastrado _ [] = False
ehCadastrado c (x:xs) | ((headCadastrado c x) == False) = ehCadastrado c xs
                      | otherwise = True

primeiroAssento :: [[String]] -> String
primeiroAssento [] = ""
primeiroAssento (x:xs) = head x ++ "\n" ++ primeiroAssento xs

primeiroAssento1 :: [String] -> String
primeiroAssento1 [] = ""
primeiroAssento1 (x:xs) = x ++ "\n" ++ primeiroAssento1 xs 

