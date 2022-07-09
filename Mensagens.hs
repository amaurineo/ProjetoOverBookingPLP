module Mensagens where
import System.IO
import Util

menuPrincipal :: IO ()
menuPrincipal = do
    putStr("\nEscolha uma opcao para prosseguir")
    putStr("\n[1] Area do cliente")
    putStr("\n[2] Area do funcionario")
    putStr("\n[3] Area da empresa")
    putStr("\n[4] Sair\n")

opcaoInvalida :: IO ()
opcaoInvalida = do
    putStr("\nERRO: Escolha uma opcao valida\n\n")

mensagemSaida :: IO ()
mensagemSaida = do
    putStr("\nAté mais!\n")