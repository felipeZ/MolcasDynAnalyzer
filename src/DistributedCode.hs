
module DistributedCode where

import System.Directory (createDirectory)
import System.FilePath.Posix ((</>))
import System.Process (callCommand)
import Text.Printf
-- --------------> TYPES <-------------------

type Tuple = (Int,Int)
type Module = String

-- -------------->  <-------------

compileCode :: Module -> [Tuple] -> IO FilePath
compileCode tup  = do
         home     <- getHomeDirectory
         path     <- createDirectory $ home </> "dist"
         fileMain <- createMain tup path
         setCurrentDirectory path
         callCommand $ "ghc -O2 Main.hs -threaded"
         return path

createMain :: Tuple -> FilePath ->  IO FilePath
createMain tup path = 
 do   let pathMain  = path </> "Main.hs" 
      writeFile pathMain $ header tuple 
      return pathMain       

 where header = printf "\nmodule main where\n\nimport Data.List (isSuffixOf)\nimport System.Environment(getArgs)\nimport ProcessFile\n\n\nmain = do\n   [file] <- getArgs\n   let tup = %s \n   if isSuffixOf \".out\" file  \n      then processout file \n      else isSuffixOf \".xyz\" file then processxyz tup file \n                                  else error \"Unknown FilePath\"\n"



distributesCode :: 
