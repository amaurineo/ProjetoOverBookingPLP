module Mensagens where
import System.IO
import Util

menuPrincipal :: IO ()
menuPrincipal = do
    putStr"\nEscolha uma opcao para prosseguir"
    putStr"\n[1] Area do cliente"
    putStr"\n[2] Area do funcionario"
    putStr"\n[3] Area da empresa"
    putStr"\n[4] Sair\n"

opcaoInvalida :: IO ()
opcaoInvalida =
    putStr("\nERRO: Escolha uma opcao valida\n\n")

mensagemSaida :: IO ()
mensagemSaida =
    putStr("\nAté mais!\n")

usuarioNaoCadastrado :: IO()
usuarioNaoCadastrado =
    putStrLn("\nErro: usuário não cadastrado!\n")

menuFuncionario :: IO()
menuFuncionario = do
    putStrLn"\n       -----FUNCIONÁRIO-----"
    putStrLn"\nOlá, funcionário!"
    putStrLn"\nComo deseja prosseguir?"
    putStrLn"[1] Exibir lista de assentos disponíveis"
    putStrLn"[2] Escolher assento para um cliente"
    putStrLn"[3] Exibir clientes cadastrados"
    putStrLn"[4] Excluir cliente do sistema"
    putStrLn"[5] Calcular valor do assento"
    putStrLn"[6] Cadastrar cliente no sistema"
    putStrLn"[7] Voltar ao menu principal\n"


exibirListaClientesCadastrados :: IO()
exibirListaClientesCadastrados = do
    putStrLn"-----CLIENTES CADASTRADOS-----\n"
    arq <- openFile "arquivos/clientes.txt" ReadMode
    conteudo <- hGetContents arq
    putStrLn conteudo
    hClose arq

getCpf :: IO()
getCpf = do
    putStr"Insira seu CPF: "


usuarioAssentoOcupado :: IO()
usuarioAssentoOcupado = do
    putStrLn"\nErro: usuário já está ocupando um assento."

usuarioInvalido :: IO()
usuarioInvalido = do
    putStrLn"\nErro: usuário não cadastrado no sistema.\n"

valorPago :: String -> [[String]] -> IO()
valorPago cpf lista2 = do

    putStr("O valor a ser pago em REAIS pelo cliente " ++ Util.getNome cpf lista2 ++ " é: ")