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

getCpf :: IO()
getCpf = do
    putStrLn"Insira seu CPF: "

usuarioInvalido :: IO()
usuarioInvalido = do
    putStrLn"\nErro: usuário não cadastrado no sistema.\n"

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
    putStrLn"[7] altera Assento"
    putStrLn"[8] Voltar ao menu principal\n"

clienteExcluido :: IO()
clienteExcluido = do
    putStr("\nCLIENTE EXCLUIDO COM SUCESSO!") 

clienteAlterado :: IO()
clienteAlterado = do
    putStr("\nCLIENTE alterado COM SUCESSO!") 

assentoInvalido :: IO()
assentoInvalido = do
    putStr("\nASSENTO INDISPONÍVEL\n")