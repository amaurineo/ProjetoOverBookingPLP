import Funcionario
import Util
import Mensagens
import System.IO
import Data.List


--- Chama uma das funcionalidades do menu da empresa
menuEmpresa:: (IO()) -> IO ()
menuEmpresa menu = do
                Mensagens.menuEmpresa
                funcionalidade <- Util.lerEntradaString

                if funcionalidade == "1"
                    then do cadastroDeFuncionario menu
                else if funcionalidade == "2"
                    then do {}
                else if funcionalidade == "3"
                    then do {}
                else if funcionalidade == "4"
                    then do {}
                else if funcionalidade == "5"
                    then do {}
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
                  then do menu
                else do
                  {{putStrLn("\nError: OPÇÃO INVÁLIDA\n"); }

                

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
                    then do {Mensagens.usuarioCadastrado, cadastroDeFuncionario menu}
                else do
                    let funcionarioString = cpf ++ "," ++ nome ++ "\n"
                    appendFile "arquivos/funcionarios.txt" (funcionarioString)


-- Exclusão de um funcionario do sistema da empresa
excluirFuncionario:: (IO()) -> IO()
excluirFuncionario menu = do
                arquivo <- openFile "arquivos/funcionarios.txt" ReadMode
                linhasFunc <- getLines arquivo
                let listaDeFunc <- ((Data.List.map (split(==',') ) linhasFunc))
                putStr("\nAtualmente temos os seguintes funcionários no sistema: ")
                print(listaDeFunc)

                putStr("\nInforme o CPF do funcionário que deseja excluir: ")
                cpf <- Util.lerEntradaString

                if not (Util.temCadastro cpf listaDeFunc)
                    then do Mensagens.usuarioInvalido
                
