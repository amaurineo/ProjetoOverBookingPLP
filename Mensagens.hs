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


mensagemSaida :: IO ()
mensagemSaida =
    putStr("\nAté mais!\n")


menuFuncionario :: IO()
menuFuncionario = do
    putStrLn"\n       -----FUNCIONÁRIO-----"
    putStrLn"\nOlá, funcionário!"
    putStrLn"\nComo deseja prosseguir?"
    putStrLn"[0] Exibir descontos disponíveis"
    putStrLn"[1] Exibir lista de assentos disponíveis"
    putStrLn"[2] Escolher assento para um cliente"
    putStrLn"[3] Exibir clientes cadastrados"
    putStrLn"[4] Excluir cliente do sistema"
    putStrLn"[5] Cadastrar cliente no sistema"
    putStrLn"[6] Alterar dado do cliente"
    putStrLn"[7] Recomendar assento"
    putStrLn"[8] Voltar ao menu principal"
    putStrLn"[9] Exibir assentos indisponíveis"
    putStrLn"[10] Exibir valores para cada tipo de assento"
    putStrLn"[11] Alterar assento de um cliente\n"
menuEmpresa :: IO()
menuEmpresa = do
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

listaDescontos :: IO()
listaDescontos = do
    putStrLn"-----DESCONTOS DISPONÍVEIS-----\n"
    arq <- openFile "arquivos/descontos.txt" ReadMode
    conteudo <- hGetContents arq
    putStrLn conteudo
    hClose arq

exibirListaDescontos :: IO()
exibirListaDescontos = do
    putStrLn "DESCONTOS DISPONÍVEIS:\n"
    arq <- openFile "arquivos/descontos.txt" ReadMode
    conteudo <- hGetContents arq
    putStrLn conteudo
    hClose arq

getCpf :: IO()
getCpf = do
    putStrLn"Insira seu CPF: "

opcaoInvalida :: IO ()
opcaoInvalida =
    putStr("\nERRO: Escolha uma opcao valida\n\n")

usuarioInvalido :: IO()
usuarioInvalido = do
    putStrLn"\nErro: usuário não cadastrado no sistema.\n"


    putStr("O valor a ser pago em REAIS pelo cliente " ++ Util.getNome cpf lista2 ++ " é: ")


clienteExcluido :: IO()
clienteExcluido = do
    putStr("\nCLIENTE EXCLUIDO COM SUCESSO!")


informeCpf :: IO()
informeCpf = do
    putStrLn("Informe o CPF: ")

informeIdade :: IO()
informeIdade = do
    putStrLn("Informe sua Idade: ")


usuarioCadastrado :: IO()
usuarioCadastrado = do
    putStrLn("Erro: usuário já cadastrado!")


clienteAlterado :: IO()
clienteAlterado = do
    putStr("\nCLIENTE alterado COM SUCESSO!") 

assentoInvalido :: IO()
assentoInvalido = do
    putStr("\nASSENTO INDISPONÍVEL\n")
usuarioNaoCadastrado :: IO()
usuarioNaoCadastrado =
    putStrLn("\nErro: usuário não cadastrado!\n")
