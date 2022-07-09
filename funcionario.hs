import Util
import Mensagens
import Data.List

-- Recebe cpf do funcionário
cpfFuncionario :: IO()
cpfFuncionario = do
    putStr "\nInforme seu CPF para fazer o login: "

-- Verifica se o cpf está cadastrado no sistema
verificaFuncionario :: IO() -> IO()
verificaFuncionario menu = do
    cpfFuncionario
    cpf <- Util.lerEntradaString

    arq <- readFile "arquivos/funcionarios.txt"
    let lista = Data.List.map (Util.split(==',')) (lines arq)

    if Util.temCadastro cpf lista
        then do {putStr "\nBem vindo de volta!\n"; logaFuncionario menu}
    else do
        {Mensagens.usuarioNaoCadastrado ; menu}


--
logaFuncionario :: IO() -> IO()
logaFuncionario menu = do
    Mensagens.menuFuncionario



    putStr"Opção: "
    op <- Util.lerEntradaString
    if op == "1"
        then do {Util.exibirAssentos; logaFuncionario menu}
    else if op == "2"
        then do {escolheAssento; logaFuncionario menu}
    else if op == "3"
        then do {Mensagens.exibirListaClientesCadastrados; logaFuncionario menu}
    else if op == "4"
        then do {excluirCliente menu; logaFuncionario menu}
    else if op == "5"
        then do calcularValorPassagem menu
    else if op == "6"
        then do menu
    else do
        {Mensagens.opcaoInvalida; logaFuncionario menu}