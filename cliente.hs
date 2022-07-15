module Cliente where
import Control.DeepSeq
import Control.Exception
import Util
import Mensagens
import Prelude
import Data.List
import System.IO
    ( hClose,
      hFlush,
      openFile,
      hGetContents,
      hPutStr,
      Handle,
      IOMode(ReadMode, WriteMode,ReadWriteMode) )


--- O cliente pode acessar pelo login ou se cadastrar ---
acessoCliente :: (IO()) -> IO()
acessoCliente menu = do
    Mensagens.loginouCadastroCliente
    escolha <- Util.lerEntradaString

    if(escolha == "1")
        then do verificaCliente menu
    else if(escolha == "2")
        then do cadastrarCliente menu
    else do
        Mensagens.opcaoInvalida
        acessoCliente menu

--- Cadasto de Cliente no sistema ---
cadastrarCliente :: (IO()) -> IO()
cadastrarCliente menu = do
    Mensagens.cadastrarNome
    nome <- Util.lerEntradaString

    Mensagens.informeCpf
    cpf <- Util.lerEntradaString

    Mensagens.informeIdade
    idade <-Util.lerEntradaString

    arq <- readFile "arquivos/clientes.txt"
    let lista = ((Data.List.map (Util.wordsWhen(==',') ) (lines arq)))

    if (Util.ehCadastrado cpf lista)
       then do {Mensagens.usuarioCadastrado; acessoCliente menu}
    else do
        let clienteStr = cpf ++ "," ++ idade ++ "\n"
        appendFile "arquivos/clientes.txt" (clienteStr)
        Mensagens.cadastroEfetuado
        loginCliente menu

--realiza o login do cliente
loginCliente :: (IO()) -> IO()
loginCliente menu = do
    Mensagens.menuCliente

    putStrLn"Opção: "
    op <- Util.lerEntradaString
    if op == "1"
        then do {alteraDadoCliente menu; loginCliente menu} --- alterar cadastro
    else if op == "2"
        then do {excluirCliente menu; loginCliente menu} --- deleta cadastro
    else if op == "3"
        then do {listaTodosAssentosDisponiveis menu; loginCliente menu} --- exibir assentos disponiveis
    else if op == "4"
        then do {realizarCompra menu; loginCliente menu}
    else if op == "7"
        then do {renovaAssentos menu; loginCliente menu}
    else if op == "8"
        then do {recomendaAssento menu; loginCliente menu} --- recomendar assentos
    else if op == "5"
        then do menu
    else do
        {Mensagens.opcaoInvalida; loginCliente menu}

-- Verifica se o cpf está cadastrado no sistema
verificaCliente :: (IO()) -> IO()
verificaCliente menu = do
    Mensagens.getCpf 
    cpf <- Util.lerEntradaString

    arq <- readFile "arquivos/clientes.txt"
    let lista = Data.List.map (Util.split(==',') ) (lines arq)

    if Util.temCadastro cpf lista
        then do {putStr"\nBem vindo de volta!\n"; loginCliente menu}
    else do
        {Mensagens.usuarioInvalido; menu}

realizarCompra :: (IO()) -> IO()
realizarCompra menu = do

    arquivo <- openFile "arquivos/clientes.txt" ReadMode
    linhasCliente <- getLinesClientes arquivo

    arquivo1 <- readFile "arquivos/assentos_disponiveis.txt"
    let listaDeAssentosDisponiveis = ((Data.List.map (wordsWhen(==',') ) (lines arquivo1)))

    arquivo2 <- readFile "arquivos/assentos.txt"
    let listaDeTodosAssentos = ((Data.List.map (wordsWhen(==',') ) (lines arquivo2)))

    arquivo4 <- readFile "arquivos/assentos_economico.txt"
    let listaDeAssentosEconomico = (lines arquivo4)

    arquivo5 <- readFile "arquivos/assentos_executivo_disponivel.txt"
    let listaDeAssentosExecutivoDisponivel =  (Data.List.map (wordsWhen(==',') )(lines arquivo5))

    arquivo6 <- readFile "arquivos/assentos_economico_disponivel.txt"
    let listaDeAssentosEconomicoDisponivel =  (Data.List.map (wordsWhen(==',') )(lines arquivo6))


    evaluate (force arquivo1)
    evaluate (force arquivo2)
    evaluate (force arquivo4)
    evaluate (force arquivo5)
    evaluate (force arquivo6)

    Mensagens.informeCpf
    cpf <- Util.lerEntradaString

    let listaDeCliente = ((Data.List.map (wordsWhen(==',') ) (linhasCliente)))

    if not (Util.temCadastro cpf listaDeCliente)
        then do {Mensagens.usuarioInvalido; realizarCompra menu}
    else do
        putStr("Qual tipo de classe vc deseja? [1]Executivo ou [2]Economico\n")
        tipoClasse <- Util.lerEntradaString
     
        if (tipoClasse == "1")
            then do
                putStr("Os assentos disponíveis são: ")
                print(listaDeAssentosExecutivoDisponivel)

                putStr("Qual assento você deseja? ")
                tipoAssento <- Util.lerEntradaString

                if not (Util.temAssento tipoAssento listaDeAssentosExecutivoDisponivel)
                    then do {Mensagens.assentoInvalido; realizarCompra menu}
                else do
                    let assentoStr = cpf ++ "," ++ tipoAssento ++ "," ++ "125" ++ "\n"
                    appendFile "arquivos/compra.txt" (assentoStr)

                    let aux = Util.primeiroAssento(Util.opcaoVaga tipoAssento listaDeAssentosExecutivoDisponivel)
                    Util.escreveAssento1 ""
                    appendFile "arquivos/assentos_executivo_disponivel.txt" (aux)
        
        else if (tipoClasse == "2")
            then do
                putStr("Os assentos disponíveis são: ")
                print(listaDeAssentosEconomicoDisponivel)

                putStr("Qual assento você deseja? ")
                tipoAssento <- Util.lerEntradaString

                if not (Util.temAssento tipoAssento listaDeAssentosEconomicoDisponivel)
                    then do {Mensagens.assentoInvalido; realizarCompra menu}
                else do
                    let assentoStr = cpf ++ "," ++ tipoAssento ++ "," ++ "50" ++ "\n"
                    appendFile "arquivos/compra.txt" (assentoStr)

                    let aux = Util.primeiroAssento(Util.opcaoVaga tipoAssento listaDeAssentosEconomicoDisponivel)
                    Util.escreveAssento2 ""
                    appendFile "arquivos/assentos_economico_disponivel.txt" (aux)

        else
            Mensagens.opcaoInvalida


renovaAssentos :: (IO()) -> IO()
renovaAssentos menu = do
    arquiv1 <- readFile "arquivos/assentos_executivo.txt"
    let listaDeTodosAssentosExecutivo = (lines arquiv1)
    let x = Util.primeiroAssento1(listaDeTodosAssentosExecutivo)
    

    arquiv2 <- readFile "arquivos/assentos_economico.txt"
    let listaDeTodosAssentosEconomico = (lines arquiv2)
    let y = Util.primeiroAssento1(listaDeTodosAssentosEconomico)

    evaluate (force arquiv1)
    evaluate (force arquiv2)

    Util.escreveAssento1 ""
    Util.escreveAssento2 ""
    appendFile "arquivos/assentos_executivo_disponivel.txt" (x)
    appendFile "arquivos/assentos_economico_disponivel.txt" (y)


-- alterar cadastro do cliente
alteraDadoCliente :: (IO()) -> IO()
alteraDadoCliente menu = do
    arquivo <- readFile "arquivos/clientes.txt"
    --linhasCliente <- getLinesClientes arquivo

    putStr("Informe o CPF do Cliente que deseja alterar: ")
    cpf <- Util.lerEntradaString

    let lista = ((Data.List.map (split(==',') ) (lines arquivo)))
    evaluate (force arquivo)


    putStr("\nAtualmente temos os seguintes clientes no sistema: ")
    print(lista)

    if not (Util.temCadastro cpf lista)
        then do {Mensagens.usuarioInvalido; excluirCliente menu}     
    else do 
        putStrLn("Nova Idade: ")
        idade <- Util.lerEntradaString

        let clientesExc = Util.primeiraHorarioCpf (Util.opcaoVaga cpf lista)
        Util.escreveCliente ""
    
        appendFile "arquivos/clientes.txt" (clientesExc)
        
        let clienteStr = cpf ++ "," ++ idade ++ "\n"
        appendFile "arquivos/clientes.txt" (clienteStr)
        --Mensagens.cadastroEfetuado
        Mensagens.clienteAlterado

        loginCliente menu

    
-- #
getLinesClientes :: Handle -> IO [String]
getLinesClientes h = hGetContents h >>= return . lines

excluirCliente:: (IO()) -> IO()
excluirCliente menu = do

                arquivo <- openFile "arquivos/clientes.txt" ReadMode
                linhasCliente <- getLinesClientes arquivo

                putStr("\nAtualmente temos os seguintes funcionários no sistema: ")
                print(linhasCliente)

                putStr("\nInforme o CPF do Cliente que deseja excluir: ")
                cpf <- Util.lerEntradaString

                let listaDeCliente = ((Data.List.map (wordsWhen(==',') ) (linhasCliente)))

                if not (Util.temCadastro cpf listaDeCliente)
                    then do {Mensagens.usuarioInvalido; excluirCliente menu}     
                else do 
                    let clientesExc = Util.primeiraHorarioCpf (Util.opcaoVaga cpf listaDeCliente)
                    Util.escreveCliente ""
                    
                    appendFile "arquivos/clientes.txt" (clientesExc)
                    Mensagens.clienteExcluido


recomendaAssento :: (IO()) -> IO()
recomendaAssento menu = do

    arquivo <- readFile "arquivos/assentos_disponiveis.txt"

    let lista = (lines arquivo)
    evaluate (force arquivo)
    let assento = head lista

    putStrLn("Lhe recomendamos esse assento:")
    print(assento)


--Listar assentos executivos e econômico disponíveis
getLinesAssentos :: Handle -> IO [String]
getLinesAssentos h = hGetContents h >>= return . lines


listaTodosAssentosDisponiveis:: (IO()) -> IO()
listaTodosAssentosDisponiveis menu = do
                arquivo <- openFile "arquivos/assentos.txt" ReadMode
                linhasAssentos <- getLinesAssentos arquivo
                let listaDeAssentos = ((Data.List.map (split(==',') ) linhasAssentos))
                putStr("\nAtualmente temos os seguintes assentos executivos e econômicos no sistema: \n")
                print(listaDeAssentos)

escolheAssento :: IO()
escolheAssento = do
    putStr"\nSerá necessário realizar o cadastro ou log in no sistema!\n"
    Mensagens.getCpf
    cpf <- Util.lerEntradaString

    arq <- readFile "arquivos/cpv.txt"
    let lista = Data.List.map (Util.split(==',') ) (lines arq)

    arq2 <- readFile "arquivos/clientes.txt"
    let lista2 = Data.List.map (Util.split(==',') ) (lines arq2)

    if Util.temCadastro cpf lista
        then do Mensagens.usuarioAssentoOcupado
    else if Util.temCadastro cpf lista2
        then do
            putStr"\nVocê já está cadastrado, pode continuar.\n"

            putStr"\n"
            Util.escolheAssento cpf
            putStr""
    else do
        putStr"Informe o nome: "
        nome <- Util.lerEntradaString

        let clienteStr = cpf ++ "," ++ nome ++ "," ++ "\n"
        appendFile "arquivos/clientes.txt" clienteStr

        Util.escolheAssento cpf
    putStr""



--TALVEZ isso cause um erro
getlines :: Handle -> IO [String]
getlines h = hGetContents h >>= return . lines

aux :: String -> [String] -> Bool
aux v (x:xs) = v == x

calcularValorPassagem :: IO() -> IO()
calcularValorPassagem menu = do
    Mensagens.getCpf
    cpf <- Util.lerEntradaString

    arq <- readFile "arquivos/clientes.txt"
    let lista = Data.List.map (Util.split(==',') ) (lines arq)

    if Util.temCadastro cpf lista
        then do {calculo cpf; loginCliente menu}
    else do
        {Mensagens.usuarioInvalido; loginCliente menu}

calculo :: String -> IO()
calculo cpf = do
    arq <- readFile "arquivos/horario-cpf.txt"
    let lista = Data.List.map (split(==',') ) (lines arq)
    let servicoFilt = Util.auxRecomendar cpf lista

    if lista == []
        then do Mensagens.usuarioInvalido
    else do

        let getVaga = Util.getIndiceCpv(Util.getVagaCpv cpf lista) ++ "," ++ "\n"
        appendFile "arquivos/assentos.txt" (getVaga)

        Util.escreverCpv (primeiraCpv (Util.opcaoAssento cpf lista))

        Util.escreverHorarioCpf (primeiraCpv (Util.opcaoAssento cpf lista))

        arqClientes <- readFile "arquivos/clientes.txt"
        let lista2 = (Data.List.map (split(==',') ) (lines arqClientes))

        valor <- readFile "arquivos/valorAssento.txt"
        let lista3 = lines valor

        Mensagens.valorPago cpf lista2
        print(valorFinalEst "20" (dizHoraInt (horaCpf cpf lista)) (extraInt servicoFilt) (toInt(lista3 !! 0)))

        putStr""


primeiraCpv :: [[String]] -> String
primeiraCpv [] = ""
primeiraCpv (x:xs) = head x ++ "," ++ (x !! 1) ++ "," ++ (x !! 2) ++ "," ++ (x !! 3) ++ "," ++ (x !! 4) ++ "\n" ++ primeiraCpv xs

valorFinalEst :: String -> Int -> Int -> Int -> Int
valorFinalEst saida entrada extra getValor =  (((toInt saida) - entrada) * getValor) + extra

toInt :: String -> Int
toInt s = read (s) :: Int

dizHoraInt:: [[String]] -> Int
dizHoraInt lista = read (lista !! 0 !! 3) :: Int

horaDeSaida :: IO()
horaDeSaida = do
    putStr("Hora de saída: ")


horaCpf :: String -> [[String]] -> [[String]]
horaCpf _ [] = []
horaCpf v (x:xs) | (auxHoraCpf v x) == False = horaCpf v xs
                   | otherwise = x:horaCpf v xs

extraInt :: [[String]] -> Int
extraInt (x:xs) | (x !! 4) == "s" = 15
                | otherwise = 0

auxHoraCpf :: String -> [String] -> Bool
auxHoraCpf v (x:xs) = (v == x)
