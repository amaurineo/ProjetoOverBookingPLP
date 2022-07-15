import Mensagens
import Util
import Cliente( acessoCliente )
import Funcionario
import Empresa

main :: IO ()
main = do
	putStr("\n--------------------Seja bem vinde ao sistema OVERBOOKING!--------------------\n")
	Mensagens.menuPrincipal
	opcao <- Util.lerEntradaString
	escolheOpcao opcao

escolheOpcao :: String -> IO()
escolheOpcao opcao | opcao == "1" = Cliente.acessoCliente main
				   | opcao == "2" = Funcionario.verificaFuncionario main
				   | opcao == "3" = Empresa.menuEmpresa  main
				   | opcao == "4" = Mensagens.mensagemSaida
                   | otherwise = do {Mensagens.opcaoInvalida; main}


