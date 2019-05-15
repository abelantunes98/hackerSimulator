{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import GHC.Read
import Help
import Data.List.Split

-- Tupla para representar um arquivo
data Arquivo =
 Arquivo { nomeArq  ::  !String,
 conteudo  ::  !String
 } deriving (Show, Generic, Read)

-- | Type of each JSON entry in record syntax.
data Diretorio =
 Diretorio { nome  :: !String,
 subdiretorios  ::  ![Diretorio],
 arquivos  ::  ![Arquivo]
 } deriving (Show,Generic, Read)

-- Objeto que representa um mensagem para ser passada ao
-- usuario
data Mensagem =
 Mensagem { idm :: !Int,
 mensagem :: !String
 } deriving (Show, Generic, Read)

-- Instances to convert our type to/from JSON.

instance FromJSON Diretorio
instance FromJSON Arquivo
instance FromJSON Mensagem

-- | Location of the local copy, in case you have it,
--   of the JSON file.
jsonFile :: FilePath
jsonFile = "servidores.json"

-- Location of JSON Mensagens
jsonFileM :: FilePath
jsonFileM = "mensagens.json"

-- Json com Diretorios e arquivos
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

-- Json com mensagens
getJSONM :: IO B.ByteString
getJSONM = B.readFile jsonFileM

-- Retorna um Either (Semelhante a promisse de JavaScript)
-- A partir da leitura do JSON
retornaEither :: IO (Either String [Diretorio])
retornaEither = (eitherDecode <$> getJSON) :: IO (Either String [Diretorio])

-- Retorna um Either para mensagens
retornaEitherM :: IO (Either String [Mensagem])
retornaEitherM = (eitherDecode <$> getJSONM) :: IO (Either String [Mensagem])

-- Retorna um array de Diretorios
-- Recebe como parametro a promisse e olha se ela foi executada com exito
-- quando executada com exito retorna estah direita ou Right
-- Se estiver em Left, estah com erro
lerJSON :: Either String [Diretorio] -> [Diretorio]
lerJSON entrada = 

 case entrada of
  Right ps -> ps 
  Left err -> []

lerJSONM :: Either String [Mensagem] -> [Mensagem]
lerJSONM entrada =
 case entrada of
  Right ps -> ps
  Left err -> []

-- Recebe o Nome de um servidor e a partir da leitura do Json
-- Caso esse servidor exista no sistema, retorna o conteudo desse
-- servidor. Nesse caso funciona para os 5 servidores existentes
-- Caso seja adicionado outros, deve modificar a funcao
-- pois ela itera por um numero especifico de elementos em um array
retornaServidor :: [Diretorio] -> String -> Diretorio
retornaServidor [a, b, c, d, e] nome 
 | nome == "135.110.60.200" = a
 | nome == "112.84.211.1240" = b
 | nome == "150.189.56.65" = c
 | nome == "220.99.134.37" = d
 | otherwise = e

-- Recebe um Diretorio e retorna o Array de subdiretorios desse Diretorio
retornaSubdiretorios :: Diretorio -> [Diretorio]
retornaSubdiretorios dir = subdiretorios dir

-- Recebe um Diretorio e retorna o Array de Arquivos dele
retornaArquivos :: Diretorio -> [Arquivo]
retornaArquivos dir = arquivos dir

-- Recebe um array e retorna o numero de elementos que ele possui
len :: [a] -> Int
len [] = 0
len (h:t) = 1 + len t

-- Recebe o nome de um subdiretorio desejado e  o array de
-- subdiretorios do diretorio atual se o subdiretorio 
-- estiver presente no diretorio atual, retorna ele
-- Esse array de Diretorios pode ser obtido pelo retorno da funcao
-- retornaSubdiretorios -- Se nao houver o subdiretorio retorna um diretorio vazio
retornaSubdiretorio :: [Diretorio] -> String -> Int -> Diretorio
retornaSubdiretorio array nomeEsperado indiceAtual
 | indiceAtual < len array && ((nome elemento) == nomeEsperado) = elemento
 | indiceAtual < len array = retornaSubdiretorio array nomeEsperado (indiceAtual + 1)
 | otherwise = Diretorio "" [] []  
 where elemento = array !! indiceAtual

-- Recebe o nome de um arquivo e o array de arquivos do diretorio atual, se o arquivo estiver presente
-- no diretorio, retorna ele. Esse array de arquivos pode ser obtido pelo retorno da funcao
-- retornaArquivos -- Se nao ouver o arquivo retorna um arquivo vazio
retornaArquivo :: [Arquivo] -> String -> Int -> Arquivo
retornaArquivo array nomeEsperado indiceAtual
 | indiceAtual < len array && ((nomeArq elemento) == nomeEsperado) = elemento
 | indiceAtual < len array = retornaArquivo array nomeEsperado (indiceAtual + 1) 
 | otherwise = Arquivo "" ""
 where elemento = array !! indiceAtual

retornaMensagem :: [Mensagem] -> Int -> Mensagem
retornaMensagem [] idIn = (Mensagem 999 "")
retornaMensagem (h:t) idIn
 | idm h == idIn = h
 | otherwise = retornaMensagem t idIn

retornaConteudoMensagem :: Mensagem -> String
retornaConteudoMensagem msg
 | idm msg == 999 = "Mensagem Inexiste"
 | otherwise = mensagem msg

-- Funcao que retorna o diretorio atual a partir da lista que guarda o caminho necessario
-- para chegar ate ele.
-- O uso de let nessa funcao foi apenas por questao de organizacao, para facilitar o entendimento
-- ela ira acessar os diretorios recursivamente ateh chegar no ultimo nome de diretorio da lista
-- que serah o que o usuario estarah no momento

retornaDiretorioAtual :: Diretorio -> [Diretorio] -> [String] -> Int -> Int -> Diretorio 
retornaDiretorioAtual dir subdirs nomes 0 num
 
 | num > 1 = do
  let servidor = retornaServidor subdirs (nomes !! 0)
  let subDirsServ = retornaSubdiretorios servidor
  retornaDiretorioAtual servidor subDirsServ nomes 1 (num - 1)
 
 | otherwise = retornaServidor subdirs (nomes !! 0)

retornaDiretorioAtual dir subdirs nomes indice 1 = do
 retornaSubdiretorio subdirs (nomes !! indice) 0
 
retornaDiretorioAtual dir subdirs nomes indice num = do
 let subdir = retornaSubdiretorio subdirs (nomes !! (indice)) 0 
 retornaDiretorioAtual subdir (retornaSubdiretorios subdir) nomes (indice + 1) (num - 1) 
--
--
-- --------------Funcoes de uso no simulador---------
--
--
-- Funcao que reduz o caminho do diretorio atual
-- Recebe o array de Strings que possui o caminho ateh o
-- diretorio atual e elimina o ultimo elemento, caso ja esteja
-- na raiz, apenas retorna o mesmo caminho da entrada
reduzCaminho :: [String] -> Int -> [String]
reduzCaminho caminho indice
 | (len caminho) == 1 = caminho
 | indice == ((len caminho) - 1) = []
 | otherwise = [caminho !! indice] ++ reduzCaminho caminho (indice + 1)

-- Funcao que aumenta o caminho do diretorio atual
-- Usado no CD, quando acessamos um novo diretirui
aumentaCaminho :: [String] -> String -> [String]
aumentaCaminho caminho dir = caminho ++ [dir]

-- Funcao CD Recebe o nome do diretorio desejado, o caminho do diretorio atual
-- e o diretorio atual, se o diretorio desejavel for alcancavel pelo atual
-- retorna o caminho ate o diretorio atual modificado, caso nao seja um destino
-- valido, retorna o caminho do diretorio atual da mesma forma que recebeu
cd :: String -> Diretorio -> [String] -> [String]
cd "" dir caminho = caminho
cd ".." dir caminho = (reduzCaminho caminho 0)
cd destino dir caminho
 | nome retorno /= "" = aumentaCaminho caminho destino
 | otherwise = ["erro"]-- caminho
 where retorno = (retornaSubdiretorio (retornaSubdiretorios dir) destino 0)

-- Recebe uma lista e um elemento e retorna True caso o elemento nao esteja na
-- lista ou False caso ele esteja. Usada na funcao RM
listaNaoPossui :: (Eq a) => [a] -> a -> Bool
listaNaoPossui [] a = True
listaNaoPossui (h:t) a
 | a == h = False
 | otherwise = listaNaoPossui t a

-- Funcao que verifica se um arquivo foi apagado, a partir do nome do arquivo
-- Uma lista de arquivos ja apagados e o diretorio atual. Retorna True
-- Caso o arquivo nao tenha sido apagado e False caso contrario
verificaNomeArquivo :: String -> [String] -> [(String, String)] -> Bool
verificaNomeArquivo nome dirAtual apagados
 | listaNaoPossui apagados (servidor, nome) = True
 | otherwise = False
 where servidor = (dirAtual !! 0) 

-- Funcao que retorna o nome dos subdiretorios de um local
-- usada para a funcao ls
retornaNomesDirs :: [Diretorio] -> Int -> [String]
retornaNomesDirs subs indice 
 | len subs == 0 = []
 | indice == ((len subs) - 1) = [nome (subs !! indice)]
 | otherwise = [nome (subs !! indice)] ++ retornaNomesDirs subs (indice + 1)

-- Funcao qoe retorna os nomes dos arquivos de um local, usada
-- na funcao ls
retornaNomesArqs :: [Arquivo] -> Int -> [String]
retornaNomesArqs arqs indice 
 | len arqs == 0 = []
 | indice == ((len arqs) - 1) = [nomeArq (arqs !! indice)]
 | otherwise = [nomeArq (arqs !! indice)] ++ retornaNomesArqs arqs (indice + 1)

-- Funcao que retorna o menor elemento de uma lista
-- usada no insertion sort
menorLista :: (Ord l) => [l] -> l
menorLista [x] = x
menorLista (h:t) 
 | h <= menorLista t = h
 | otherwise = menorLista t

-- Funcao que remove um elemento de uma lista, usada no insertion sort
removeDaLista :: (Ord l) => [l] -> l -> [l]
removeDaLista [] elem= []
removeDaLista (h:t) elem
 | h == elem = t
 |otherwise = [h] ++ removeDaLista t elem 

-- Funcao que ordena uma lista por meio de um insertion sort
-- pode usar para qualquer lista de elementos ordenaveis
ordenaLista :: (Ord a) => [a] -> [a]
ordenaLista [] = []
ordenaLista lista = [menor]  ++ ordenaLista (removeDaLista lista menor)
 where menor = menorLista lista

-- Funcao que organixa o print da funcao ls, dando espacp entre os nomes e
-- pulando linhas
retornaSaidaLs :: [String] -> Int -> String
retornaSaidaLs [] cont = ""
retornaSaidaLs (h:t) cont
 | cont < 8 = h ++ " " ++ retornaSaidaLs t (cont + 1)
 | otherwise = h ++ "\n" ++ retornaSaidaLs t 0

-- Funcao ls - Recebe um dirtorio e retorna seus subdiretorios e seus arquivos 
-- em ordem alfabetica
ls :: Diretorio -> String
ls dir = retornaSaidaLs (ordenaLista ( (retornaNomesArqs (retornaArquivos dir) 0) ++ ( retornaNomesDirs (retornaSubdiretorios dir) 0) )) 0

-- Funcao connect - Recebe um ip e caso esse ip exista no jogo retorna uma nova 
-- lista para representar o diretorio atual, caso nao exista o ip, retorna uma
-- lista vazia
connect :: String -> [String]
connect "135.110.60.200" = ["135.110.60.200", "home"]
connect "112.84.211.1240" = ["112.84.211.1240", "home"]
connect "150.189.56.65" = ["150.189.56.65", "home"]
connect "220.99.134.37" = ["220.99.134.37"]
connect outro = ["erro"]

-- Funcao CAT, recebe um diretorio e o nome de um arquivo, se o arquivo existir
-- retorna seu conteudo, caso contrario, retorna uma mensagem informando que
-- o arquivo nao existe
cat :: Diretorio -> String -> String
cat dir nome
 | nomeArq arquivo == "" = "Arquvo " ++ nome ++ " nao encontrado."
 | otherwise = conteudo arquivo
 where arquivo = retornaArquivo (retornaArquivos dir) nome 0

chamaFuncao :: String -> Diretorio -> String -> [String] -> String -> String
chamaFuncao funcao diretorio arquivo dirAtual arquivosApagados
 | funcao == "ls" = ls diretorio --arquivosApagados
 | funcao == "cat" = cat diretorio arquivo --arquivosApagados
 | funcao == "connect" = do
  let resultadoConnect = connect arquivo
  if (Prelude.head resultadoConnect == "erro") then "mensagem de erro" else "Connecting to " ++ Prelude.head (connect arquivo) ++ "..."
 | funcao == "cd" = do
  let resultadoCd = cd arquivo diretorio dirAtual
  if (Prelude.head resultadoCd == "erro") then "mensagem de erro" else ""
 | funcao == "rm" = "" --colocar coisa do erro aqui
 | funcao == "help" = help
 | funcao == "exit" = "Terminando programa..."
 | otherwise = "Comando nao encontrado" -- TROCAR ESSE TEXTO POR ALGO MAIS BONITINHO

-- Loop principal; recebe um comando, executa ele e depois chama a si mesma com um
-- novo comando.
mainLoop dirAtual arquivosApagados = do
  d <- retornaEither
  let dirVazio = Diretorio "" [] []
  let diretorio = (retornaDiretorioAtual dirVazio (lerJSON d) dirAtual 0 (len dirAtual))
  
  putStr (" " ++ (show dirAtual) ++ " >> ")

  entrada <- getLine
  let splitted = Data.List.Split.splitOn " " entrada
  let nomeFuncao = Prelude.head splitted
  let nomeArquivo = if (len splitted > 1) then Prelude.head (Prelude.tail splitted) else ""

  putStrLn (chamaFuncao nomeFuncao diretorio nomeArquivo dirAtual arquivosApagados)

  let resultadoTroca = (if nomeFuncao == "cd" then (cd nomeArquivo diretorio dirAtual) else (if nomeFuncao == "connect" then (connect nomeArquivo) else dirAtual))
  let novoDirAtual = if (Prelude.head resultadoTroca == "erro") then dirAtual else resultadoTroca

  let novosArquivosApagados = if nomeFuncao == "rm" then rm diretorio nomeArquivo arquivosApagados else arquivosApagados
  
  if entrada == "exit" then (putStrLn ("Tchau")) else mainLoop novoDirAtual novosArquivosApagados -- cada comando aumenta a pilha de recursão! tadinho do stack

main :: IO ()
main = do
  -- d recebe o valor do retorno da funcao retornaEither
  -- Isso é necessario porque antes desse passo o valor nao eh concreto  
  -- (DETALHE) essa atribuicao x <- y soh pode ser efetuada em um bloco (do)
  d <- retornaEither
  m <- retornaEitherM
  
  let dirAtual = ["135.110.60.200"]
  -- Lista com o nome dos arquivos que foram apagado em tuplas com o servidor a 
  -- qual eles pertencem
  let arquivosApagados = [("135.110.60.200", "i"),("","")]

-- Depois que o valor torna-se concreto pode ser passado como parametro na funcao 
-- lerJSON

-- Ip de um servidor para testar o retorno de um diretorio 
-- nome <- getLine
-- Nome do diretorio que quer o retorno
-- nomeDir <- getLine
-- nomeArq <- getLine
  idmsg <- readLn :: IO Int
-- Exemplos de entrada 135.110.60.200 e home // Um por linha
-- print (retornaSubdiretorio ( (retornaSubdiretorios (retornaServidor (lerJSON d) nome)) nome 0))
-- print (cat (retornaSubdiretorio (retornaSubdiretorios (retornaServidor (lerJSON d) nome)) nomeDir 0) nomeArq)
-- print (lerJSONM m)
  putStrLn (retornaConteudoMensagem (retornaMensagem (lerJSONM m) idmsg))
-- print (retornaNomesArqs (retornaArquivos (retornaDiretorioAtual dirVazio subDirs dirAtual 0 2)) 0)
-- putStrLn (retornaSaidaLs ["aaaaaaaaaaa", "aaaaaaaaaaa", "aaaaaaaaaaa","aaaaaaaaaa","aaa","aaa","aaa","aaa","aaa","aaa","aaa","aaa","aaa"] 0) 
-- help
-- print (verificaNomeArquivo "ip" dirAtual arquivosApagados)
-- print "loop principal..."
-- play ""
  print "loop principal..."
  mainLoop dirAtual arquivosApagados