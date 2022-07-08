module Mensagens where
import Data.List
import System.IO

menuPrincipal :: IO ()
menuPrincipal = do
    putStr("\nMenu Principal")
    putStr("\nEscolha uma opcao")
    putStr("\n[1] Area do cliente")
    putStr("\n[2] Area do funcionario")
    putStr("\n[3] Area da empresa")
    putStr("\n[4] Sair\n")

opcaoInvalida :: IO ()
opcaoInvalida = do
    putStr("\nERRO: Escolha uma opção válida\n")

mensagemSaida :: IO ()
mensagemSaida = do
    putStr("\nAté mais!")