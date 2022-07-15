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

menuEmpresa :: IO()
    putStrLn("\n       -----Menu da Empresa-----")

    putStrLn("\nComo deseja prosseguir?")
    putStrLn("[1] Cadastrar funcionário")
    putStrLn("[2] Alterar funcionário")
    putStrLn("[3] Excluir funcionário")
    putStrLn("[4] Visualizar funcionários ativos")
    putStrLn("[5] Listar assentos executivos e econômicos disponíveis")
    putStrLn("[6] Listar valores para cada tipo de assento")
    putStrLn("[7] Criar descontos")
    putStrLn("[8] Alterar descontos")
    putStrLn("[9] Excluir descontos")
    putStrLn("[10] Voltar ao menu principal\n")

cadastroEfetuado :: IO()
cadastroEfetuado = do
    putStr("\nCADASTRADO EFETUADO COM SUCESSO!")

cadastrarNome :: IO()
cadastrarNome = do
    putStrLn("\n       -----CADASTRO DE USUÁRIO-----")
    putStr("\nInforme o nome: ")

cadastrarIdAssento :: IO()
cadastrarIdAssento = do
    putStrLn("\n       -----CADASTRO DE ASSENTO-----")
    putStr("\nInforme o ID poltrona: ")

getTipo :: IO()
getTipo = do
    putStr"Insira o tipo da poltrona: "

cadastrarDesconto :: IO()
cadastrarDesconto = do
    putStrLn("\n       -----CADASTRO DE Desconto-----")
    putStr("Insira tipo da poltrona: ")

    

funcionarioExcluido :: IO()
funcionarioExcluido = do
    putStr("\nFUNCIONÁRIO EXCLUIDO COM SUCESSO!")

assentoExcluido :: IO()
assentoExcluido = do
    putStr("\nAssento EXCLUIDO COM SUCESSO!")


getValorDoDesconto :: IO()
getValorDoDesconto = do
    putStr("Insira o valor do desconto: ")

tipoJaCadastrado:: IO()
tipoJaCadastrado =
    putStrLn("\nErro: tipo já cadastrado!\n")

assentoJaCadastrado:: IO()
assentoJaCadastrado =
    putStrLn("\nErro: tipo já cadastrado!\n")


descontoExcluido :: IO()
descontoExcluido = do
    putStr("\nAssento EXCLUIDO COM SUCESSO!")

getCpf :: IO()
getCpf = do
    putStr"Insira seu CPF: "

opcaoInvalida :: IO ()
opcaoInvalida =
    putStr("\nERRO: Escolha uma opcao valida\n\n")

usuarioInvalido :: IO()
    putStrLn"\nErro: usuário não cadastrado no sistema.\n"

-- Parte Relacionada a Cliente 👪

cadastroEfetuado :: IO()
cadastroEfetuado = do
    putStr("\nCADASTRADO EFETUADO COM SUCESSO!")
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

usuarioNaoCadastrado :: IO()
usuarioNaoCadastrado =
    putStrLn("\nErro: usuário não cadastrado!\n")
