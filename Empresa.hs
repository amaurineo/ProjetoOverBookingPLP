module Empresa where
import Funcionario
import Util
import Mensagens
import System.IO
import Data.List


--- Chama uma das funcionalidades do menu da empresa
menuEmpresa:: (IO()) -> IO()
menuEmpresa menu = do
                Mensagens.menuEmpresa
                funcionalidade <- Util.lerEntradaString

                if funcionalidade == "1"
                    then do cadastroDeFuncionario menu
                else if funcionalidade == "2"
                    then do alterarFuncionario 
                else if funcionalidade == "3"
                    then do excluirFuncionario menu
                else if funcionalidade == "4"
                    then do listaTodosFuncionarios menu
                else if funcionalidade == "5"
                    then do listaTodosAssentosDisponiveis menu
                {-else if funcionalidade == "6"
                    then do {} -}
                else if funcionalidade == "7"
                    then do cadastroDeAssentos menu
                {-else if funcionalidade == "8"
                    then do {} -}
                else if funcionalidade == "9"
                    then do excluirAssentos menu
                else if funcionalidade == "10"
                  then do valoresDeCadaTipoo menu
                else if funcionalidade == "11"
                  then do cadastroDeDescontos menu
               {-} else if funcionalidade == "12"
                  then do {} -}
                else if funcionalidade == "13"
                  then do excluirDesconto menu
                {-else if funcionalidade == "14"
                  then do {}
                else if funcionalidade == "15"
                  then do menu-}
                else do
                  {putStrLn("\nError: OPÇÃO INVÁLIDA\n"); Empresa.menuEmpresa menu}

                
-- Cadastrado de funcionario na empresa
cadastroDeFuncionario:: (IO()) -> IO()
cadastroDeFuncionario menu = do
                Mensagens.cadastrarNome
                nome <- Util.lerEntradaString

                Mensagens.getCpf
                cpf <- Util.lerEntradaString

                arquivo <- readFile "arquivos/funcionarios.txt"
                let lista = ((Data.List.map (Util.wordsWhen(==',') ) (lines arquivo)))

                if (Util.temCadastro cpf lista)
                    then do {Mensagens.usuarioNaoCadastrado; cadastroDeFuncionario menu}
                else do
                    let funcionarioString = cpf ++ "," ++ nome ++ "\n"
                    appendFile "arquivos/funcionarios.txt" (funcionarioString)
                    Mensagens.cadastroEfetuado

-- Altera Funcinário
alterarFuncionario :: IO()
alterarFuncionario = do
    putStr("\nQual a alteração que deseja fazer? ")
    newValue <- Util.lerEntradaString

    arq <- openFile "arquivos/funcionarios.txt" WriteMode
    hPutStr arq newValue
    hFlush arq
    hClose arq

    putStr("\nFuncionário alterado com sucesso!\n")
-- pergunta ao monitor como fazer atualização ou da head ou do tail da lista

-- Exclusão de um funcionario do sistema da empresa
getLinesFuncionarios :: Handle -> IO [String]
getLinesFuncionarios h = hGetContents h >>= return . lines

excluirFuncionario:: (IO()) -> IO()
excluirFuncionario menu = do
                arquivo <- openFile "arquivos/funcionarios.txt" ReadMode
                linhasFunc <- getLinesFuncionarios arquivo
                let listaDeFunc = ((Data.List.map (split(==',') ) linhasFunc))
                putStr("\nAtualmente temos os seguintes funcionários no sistema: ")
                print(listaDeFunc)

                putStr("\nInforme o CPF do funcionário que deseja excluir: ")
                cpf <- Util.lerEntradaString
                print(cpf)
                if not (Util.temCadastro cpf listaDeFunc)
                    then do {Mensagens.usuarioInvalido; excluirFuncionario menu}
                else do
                    let funcionarios = Util.primeiraHorarioCpf (opcaoVaga cpf listaDeFunc)
                    Util.escreveFuncionario ""

                    appendFile "arquivos/funcionarios.txt" (funcionarios)
                    Mensagens.funcionarioExcluido


--Lista todos os funcionarios
listaTodosFuncionarios :: (IO()) -> IO()
listaTodosFuncionarios menu = do
                arquivo <- openFile "arquivos/funcionarios.txt" ReadMode
                linhasFunc <- getLinesFuncionarios arquivo
                let listaDeFunc = ((Data.List.map (split(==',') ) linhasFunc))
                putStr("\nAtualmente temos os seguintes funcionários no sistema: ")
                print(listaDeFunc)


--Listar assentos executivos e econômico disponíveis
getLinesAssentos :: Handle -> IO [String]
getLinesAssentos h = hGetContents h >>= return . lines


listaTodosAssentosDisponiveis:: (IO()) -> IO()
listaTodosAssentosDisponiveis menu = do
                arquivo <- openFile "arquivos/assentos.txt" ReadMode
                linhasAssentos <- getLinesAssentos arquivo
                let listaDeAssentos = ((Data.List.map (split(==',') ) linhasAssentos))
                putStr("\nAtualmente temos os seguintes assentos executivos e econômicos no sistema: ")
                print(listaDeAssentos)


--Adicionar assentos e tipo dos assentos
cadastroDeAssentos:: (IO()) -> IO()
cadastroDeAssentos menu = do
                Mensagens.cadastrarIdAssento
                id <- Util.lerEntradaString

                Mensagens.getTipo
                tipo <- Util.lerEntradaString

                arquivo <- readFile "arquivos/assentos.txt"
                let listaDeAssentos = ((Data.List.map (Util.wordsWhen(==',') ) (lines arquivo)))

                if (Util.temCadastro id listaDeAssentos)
                    then do {Mensagens.assentoJaCadastrado; cadastroDeAssentos menu}
                else do
                    let assentoString = id ++ "," ++ tipo ++ "\n"
                    appendFile "arquivos/assentos.txt" (assentoString)
                    Mensagens.cadastroEfetuado

-- Exclusão de um assento
getLinesAssent :: Handle -> IO [String]
getLinesAssent h = hGetContents h >>= return . lines

excluirAssentos:: (IO()) -> IO()
excluirAssentos menu = do
                arquivo <- openFile "arquivos/assentos.txt" ReadMode
                linhasAssentos <- getLinesAssent arquivo
                let listaDeAssentos = ((Data.List.map (split(==',') ) linhasAssentos))
                putStr("\nAtualmente temos os seguintes assentos no sistema: ")
                print(listaDeAssentos)

                putStr("\nInforme o ID da poltrona que deseja excluir: ")
                id <- Util.lerEntradaString

                if not (Util.temCadastro id listaDeAssentos)
                    then do {Mensagens.usuarioInvalido; excluirFuncionario menu}
                else do
                    let assentos = Util.primeiraHorarioCpf (opcaoVaga id listaDeAssentos)
                    Util.escreveAssento ""

                    appendFile "arquivos/assentos.txt" (assentos)
                    Mensagens.assentoExcluido


-- Cria descontos para poltronas
cadastroDeDescontos:: (IO()) -> IO()
cadastroDeDescontos menu = do
                Mensagens.cadastrarDesconto
                tipo <- Util.lerEntradaString

                Mensagens.getValorDoDesconto
                valor <- Util.lerEntradaString
                --let valorInt = parseToInt valor

                arquivo <- readFile "arquivos/descontos.txt"
                let listaDeDescontos = ((Data.List.map (Util.wordsWhen(==',') ) (lines arquivo)))

                if (Util.temCadastro tipo listaDeDescontos)
                    then do {Mensagens.tipoJaCadastrado; cadastroDeDescontos menu}
                else do
                    let descontoString = tipo ++ "," ++ valor ++ "\n"
                    appendFile "arquivos/descontos.txt" (descontoString)
                    Mensagens.cadastroEfetuado

-- Exclusão de um assento
getLinesDesconto :: Handle -> IO [String]
getLinesDesconto h = hGetContents h >>= return . lines

excluirDesconto:: (IO()) -> IO()
excluirDesconto menu = do
                arquivo <- openFile "arquivos/descontos.txt" ReadMode
                linhasDescontos <- getLinesDesconto arquivo
                let listaDeDescontos = ((Data.List.map (split(==',') ) linhasDescontos))
                putStr("\nAtualmente temos os seguintes descontos relacionados a tipos de poltrona no sistema: ")
                print(listaDeDescontos)

                putStr("\nInforme o TIPO da poltrona que deseja excluir: ")
                tipo <- Util.lerEntradaString

                if not (Util.temCadastro tipo listaDeDescontos)
                    then do {Mensagens.usuarioInvalido; excluirDesconto menu}
                else do
                    let descontos = Util.primeiraHorarioCpf (opcaoVaga tipo listaDeDescontos)
                    Util.escreveDesconto ""

                    appendFile "arquivos/descontos.txt" (descontos)
                    Mensagens.descontoExcluido


--Listar valores relacionados aos tipo de assentos disponíveis (executivos e econômico)
getLinesValoresDeCadaTipo :: Handle -> IO [String]
getLinesValoresDeCadaTipo h = hGetContents h >>= return . lines

valoresDeCadaTipoo :: (IO()) -> IO()
valoresDeCadaTipoo menu = do
                arquivo <- openFile "arquivos/valoresDeCadaTipo.txt" ReadMode
                linhasValores <- getLinesValoresDeCadaTipo arquivo
                let listaDeValores = ((Data.List.map (split(==',') ) linhasValores))
                putStr("\nAtualmente temos os seguintes valores relacionados aos tipos de assentos no sistema: ")
                print(listaDeValores)

