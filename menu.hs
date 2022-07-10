import Mensagens
import Util
import Funcionario

main :: IO ()
main = do
	putStr("\n--------------------Seja bem vinde ao sistema OVERBOOKING!--------------------\n")
	Mensagens.menuPrincipal

	opcao <- Util.lerEntradaString
	escolheOpcao opcao

escolheOpcao :: String -> IO()
escolheOpcao opcao | opcao == "1" = putStr"Area do cliente"
				   | opcao == "2" = Funcionario.verificaFuncionario main
				   | opcao == "3" = putStr("Area da empresa")
				   | opcao == "4" = Mensagens.mensagemSaida
                   | otherwise = do {Mensagens.opcaoInvalida; main}


