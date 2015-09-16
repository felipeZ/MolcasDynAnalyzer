{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, QuasiQuotes #-}

module SlurmParse  where

import Control.Applicative ((<$>),(<*>),(*>),(<*))
import Data.Data
import  Language.Haskell.TH as TH
import Text.Parsec
import Text.Parsec.String (Parser)

-- Internal Modules


-- >>>>>>>>>>>>>>>>>>
data SlurmExpr
         = Const String
          | MetaVar 
           deriving (Show,Typeable,Data)


parseSlurmExp :: (SourceName, Line, Column) -> String -> Q Exp
parseSlurmExp (file, line, col) s =
    case runParser parseScript () "" s of
      Left err  -> fail $ show err
      Right xs   ->  genSlurm xs [| "" |] 
 

parseScript :: Parser [SlurmExpr]
parseScript = many1 (parseText <|> parseMeta)

parseText :: Parser SlurmExpr
parseText = Const <$> (many1 $ noneOf "%")

parseMeta :: Parser SlurmExpr
parseMeta = const MetaVar <$> (char '%' *> letter)


updatePosition file line col = do
   pos <- getPosition
   setPosition $
     (flip setSourceName) file $
     (flip setSourceLine) line $
     (flip setSourceColumn) col $
     pos

genSlurm :: [SlurmExpr] -> Q Exp -> Q Exp
genSlurm [] code = code
genSlurm (MetaVar:  xs) code =  [|\x -> $(genSlurm xs [|$code ++x |]) |]
genSlurm (Const s : xs) code =  genSlurm xs [| $code ++ s  |]
