import Mensagens
import Util
import Cliente( acessoCliente )

main :: IO ()
main = do
	putStr("\n--------------------Seja bem vinde ao sistema OVERBOOKING!--------------------\n")
	Mensagens.menuPrincipal
	opcao <- Util.lerEntradaString
	escolheOpcao opcao

escolheOpcao :: String -> IO()
escolheOpcao opcao | opcao == "1" = Cliente.acessoCliente main
				   | opcao == "2" = putStr("Area da funcionario")
				   | opcao == "3" = putStr("Area da empresa")
				   | opcao == "4" = Mensagens.mensagemSaida
                   | otherwise = do {Mensagens.opcaoInvalida; main}


