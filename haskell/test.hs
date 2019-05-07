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

main :: IO ()
main = do

-- d recebe o valor do retorno da funcao retornaEither
-- Isso é necessario porque antes desse passo o valor nao eh concreto  
-- (DETALHE) essa atribuicao x <- y soh pode ser efetuada em um bloco (do)
 d <- retornaEither
 
-- Depois que o valor torna-se concreto pode ser passado como parametro na funcao 
-- lerJSON
 print (lerJSON d)
