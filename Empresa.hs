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
               {-else if funcionalidade == "2"
                    then do {}-}
                else if funcionalidade == "3"
                    then do excluirFuncionario menu
                else if funcionalidade == "4"
                    then do listaTodosFuncionarios menu
                {-else if funcionalidade == "5"
                    then do listaTodosAssentosDisponiveis menu
                else if funcionalidade == "6"
                    then do {}
                else if funcionalidade == "7"
                    then do {}
                else if funcionalidade == "8"
                    then do {}
                else if funcionalidade == "9"
                    then do {}
                else if funcionalidade == "10"
                  then do {}
                else if funcionalidade == "11"
                  then do {}
                else if funcionalidade == "12"
                  then do {}
                else if funcionalidade == "13"
                  then do {}
                else if funcionalidade == "14"
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

{-}
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
                else
                    let funcionarios = Util.primeiraHorarioCpf (opcaoVaga cpf listaDeFunc)
                    Util.escreveFuncionario ""

                    appendFile "arquivos/funcionarios.txt" (funcionarios)
                    Mensagens.funcionarioExcluido-}
                  
                
--Lista todos os funcionarios
listaTodosFuncionarios :: (IO()) -> IO()
listaTodosFuncionarios menu = do
                arquivo <- openFile "arquivos/funcionarios.txt" ReadMode
                linhasFunc <- getLinesFuncionarios arquivo
                let listaDeFunc = ((Data.List.map (split(==',') ) linhasFunc))
                putStr("\nAtualmente temos os seguintes funcionários no sistema: ")
                print(listaDeFunc)



{- Listar assentos executivos e econômico disponíveis
listaTodosAssentosDisponiveis:: (IO() -> IO)
listaTodosAssentosDisponiveis menu = do
                arquivo <- openFile "arquivos/assentos.txt" ReadMode
                linhasAssentos <- getLines arquivo
                let listaDeAssentos = ((Data.List.map (split(==',') ) linhasAssentos))
                putStr("\nAtualmente temos os seguintes assentos executivos e econômicos no sistema: ")
                print(listaDeAssentos)-}