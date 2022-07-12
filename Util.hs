module Util where
import System.IO
import Data.List

lerEntradaString :: IO String
lerEntradaString = do
    getLine



split     :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'


temCadastro :: String -> [[String]] -> Bool
temCadastro _ [] = False
temCadastro c (x:xs) | not (headCadastrado c x)  = temCadastro c xs
                   | otherwise = True

headCadastrado :: String -> [String] -> Bool
headCadastrado c (x:xs) = c == x


exibirAssentos :: IO()
exibirAssentos = do

    putStrLn"-----ASSENTOS DISPONÍVEIS-----\n"
    arq <- readFile "arquivos/assentos.txt"
    let lista = Data.List.map (Util.split(==',') ) (lines arq)

    print (sortLista (parseDicToList (lista)))


parseDicToList :: [[String]] -> [String]
parseDicToList [] = []
parseDicToList lista = head (head lista):parseDicToList (tail lista)

parseToTxt :: [String] -> String
parseToTxt [] = ""
parseToTxt lista = head lista ++ "," ++ "\n" ++ parseToTxt (tail lista)

sortLista :: [String] -> [String]
sortLista lista = ordena [] lista

ordena :: [String] -> [String] -> [String]
ordena lista_ordenada [] = lista_ordenada
ordena lista_ordenada listaOriginal = ordena (lista_ordenada++[getMenor listaOriginal]) (removeMenor listaOriginal)



getMenor :: [String] -> String
getMenor [x] = x
getMenor(x:xs) | ( parseToInt2 (x) < parseToInt2 (maxi)) = x
               | otherwise = maxi
             where maxi = getMenor xs

removeMenor :: [String] -> [String]
removeMenor [] = []
removeMenor (x:xs) | (x == getMenor(x:xs)) = xs
                   | otherwise = (x:removeMenor xs)

parseToInt2 :: String -> Int
parseToInt2 s = read (s) :: Int


escolheAssento :: String  -> IO()
escolheAssento cpf = do

    arq <- readFile "arquivos/assentos.txt"
    let lista = ((Data.List.map (split(==',') ) (lines arq)))
    
    if lista == []
        then do print ("Não há vagas")
    else
        print (ordenarLista (parseDicToList (lista))) >>
        putStr "\nQual assento você deseja?" 

    assento <- lerEntradaString
    let lista2 = opcaoAssento assento lista

    putStr"\nDeseja realizar upgrade para a classe executiva? [S/N] "
    servicoextra <- Util.lerEntradaString



    let n = primeira (lista2)
    escreveAssento (primeira (lista2))

    let cpvStr = cpf ++ "," ++ "," ++ assento ++ "\n"
    appendFile "arquivos/cpf-assento.txt" (cpvStr)


    let cpvh = cpf ++ "," ++ "," ++ assento ++ ","  ++ "," ++ servicoextra ++ "\n"

    appendFile "arquivos/horario-cpf.txt"  (cpvh)

    let cpfUltimaVaga = cpf ++ "," ++ assento ++ "\n"
    appendFile "arquivos/recomendarAssentos.txt" (cpfUltimaVaga)

escreveCliente :: String -> IO()
escreveCliente n = do

    arq <- openFile "arquivos/clientes.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq


primeiraCliente :: [[String]] -> String
primeiraCliente [] = ""
primeiraCliente (x:xs) = head x ++ "," ++ (x !! 1) ++ "," ++ (x !! 2) ++ "\n" ++ primeiraCliente xs

aux :: String -> [String] -> Bool
aux v (x:xs) = (v == x)

opcaoAssento :: String -> [[String]] -> [[String]]
opcaoAssento _ [] = []
opcaoAssento v (x:xs) | (aux v x) == True = opcaoAssento v xs
                   | otherwise = x:opcaoAssento v xs

ordenarLista :: [String] -> [String]
ordenarLista listaOriginal = ordena [] listaOriginal

getNome :: String -> [[String]] -> String
getNome _ [] = ""
getNome c (x:xs)   | ((headCadastrado c x) == False) = getNome c xs
                   | otherwise = x !! 1


primeira :: [[String]] -> String
primeira [] = ""
primeira (x:xs) = head x ++ "," ++ "\n" ++ primeira xs


escreveAssento :: String -> IO()
escreveAssento n = do

    arq <- openFile "arquivos/assentos.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq

auxRecomendar :: String -> [[String]] -> [[String]]
auxRecomendar _ [] = []
auxRecomendar v (x:xs) | (aux v x) == False = auxRecomendar v xs
                   | otherwise = x:auxRecomendar v xs

getIndiceCpv :: [[String]] -> String
getIndiceCpv l = l !! 0 !! 2

getVagaCpv :: String -> [[String]] -> [[String]]
getVagaCpv _ [] = []
getVagaCpv v (x:xs) | (aux v x) == False = getVagaCpv v xs
                   | otherwise = x:getVagaCpv v xs


escreverCpv :: String -> IO()
escreverCpv n = do

    arq <- openFile "arquivos/cpf-assento.txt" WriteMode
    hPutStr arq n
    hFlush arq
    hClose arq


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