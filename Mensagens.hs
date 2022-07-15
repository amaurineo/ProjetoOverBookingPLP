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
    putStrLn"[6] Voltar ao menu principal\n"


exibirListaClientesCadastrados :: IO()
exibirListaClientesCadastrados = do
    putStrLn"-----CLIENTES CADASTRADOS-----\n"
    arq <- openFile "arquivos/clientes.txt" ReadMode
    conteudo <- hGetContents arq
    putStrLn conteudo
    hClose arq

getCpf :: IO()
getCpf = do
    putStrLn"Insira seu CPF: "


usuarioAssentoOcupado :: IO()
usuarioAssentoOcupado = do
    putStrLn"\nErro: usuário já está ocupando um assento."

usuarioInvalido :: IO()
usuarioInvalido = do
    putStrLn"\nErro: usuário não cadastrado no sistema.\n"

valorPago :: String -> [[String]] -> IO()
valorPago cpf lista2 = do

    putStr("O valor a ser pago em REAIS pelo cliente " ++ Util.getNome cpf lista2 ++ " é: ")


-- Parte Relacionada a Cliente 👪

cadastroEfetuado :: IO()
cadastroEfetuado = do
    putStr("\nCADASTRADO EFETUADO COM SUCESSO!")

loginouCadastroCliente :: IO()
loginouCadastroCliente = do
    putStrLn("Bem vindo a area dos clientes! digite:")
    putStrLn("[1] Se voce ja tem um log in conosco")
    putStrLn("[2] Se voce deseja se cadastrar")

cadastrarNome :: IO()
cadastrarNome = do
    putStrLn("\n       -----CADASTRO DE USUÁRIO-----")
    putStrLn("\nInforme o nome: ") 

informeCpf :: IO()
informeCpf = do
    putStrLn("Informe o CPF: ")

informeIdade :: IO()
informeIdade = do
    putStrLn("Informe sua Idade: ")

usuarioCadastrado :: IO()
usuarioCadastrado = do
    putStrLn("Erro: usuário já cadastrado!")


menuCliente :: IO()
menuCliente = do
    putStrLn"\n       -----Cliente-----"
    putStrLn"\nOlá, Cliente!"
    putStrLn"\nComo deseja prosseguir?"
    putStrLn"[1] Alterar dados no meu cadastro"
    putStrLn"[2] Deletar meu cadastro no sistema"
    putStrLn"[3] Exibir lista de assentos disponíveis para compra"
    putStrLn"[4] Recomendar assento"
    putStrLn"[5] Realizar compra"
    putStrLn"[6] Cancela todas as compras"
    putStrLn"[7] Voltar ao menu principal\n"

menuSelectAssentoCliente :: IO()
menuSelectAssentoCliente = do
    putStrLn"[3.1] Indicar assento baseado no perfil do cliente"
    putStrLn"[3.2] Escolher assento"
    putStrLn"[3.3] Calcular valor do assento"
    putStrLn"[3.4] Fechar compra do assento"
    putStrLn"[3.5] Cancelar compra do assento"


alteraNomeouIdade :: IO()
alteraNomeouIdade = do
    putStrLn("Que dado você quer alterar? digite:")
    putStrLn("[1] Para alterar o Nome")
    putStrLn("[2] Para alterar o endereço")

funcionarioExcluido :: IO()
funcionarioExcluido = do
    putStr("\nFUNCIONÁRIO EXCLUIDO COM SUCESSO!")

clienteExcluido :: IO()
clienteExcluido = do
    putStr("\nCLIENTE EXCLUIDO COM SUCESSO!") 
    
clienteAlterado :: IO()
clienteAlterado = do
    putStr("\nCLIENTE alterado COM SUCESSO!") 

assentoInvalido :: IO()
assentoInvalido = do
    putStr("\nASSENTO INDISPONÍVEL\n")