{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import GHC.Read


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

-- Instances to convert our type to/from JSON.

instance FromJSON Diretorio
instance FromJSON Arquivo

-- | Location of the local copy, in case you have it,
--   of the JSON file.
jsonFile :: FilePath
jsonFile = "servidores.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

-- Retorna um Either (Semelhante a promisse de JavaScript)
-- A partir da leitura do JSON
retornaEither :: IO (Either String [Diretorio])
retornaEither = (eitherDecode <$> getJSON) :: IO (Either String [Diretorio])

-- Retorna um array de Diretorios
-- Recebe como parametro a promisse e olha se ela foi executada com exito
-- quando executada com exito retorna estah direita ou Right
-- Se estiver em Left, estah com erro
lerJSON :: Either String [Diretorio] -> [Diretorio]
lerJSON entrada = 

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

main :: IO ()
main = do

-- d recebe o valor do retorno da funcao retornaEither
-- Isso é necessario porque antes desse passo o valor nao eh concreto  
-- (DETALHE) essa atribuicao x <- y soh pode ser efetuada em um bloco (do)
 d <- retornaEither

-- Depois que o valor torna-se concreto pode ser passado como parametro na funcao 
-- lerJSON
 
-- Ip de um servidor para testar o retorno de um diretorio 
 nome <- getLine
-- Nome do diretorio que quer o retorno
 nomeDir <- getLine

-- Exemplos de entrada 135.110.60.200 e home // Um por linha

-- print (retornaSubdiretorio ( (retornaSubdiretorios (retornaServidor (lerJSON d) nome)) nome 0))
 print (retornaSubdiretorio (retornaSubdiretorios (retornaServidor (lerJSON d) nome)) nomeDir 0)
