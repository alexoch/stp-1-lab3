module Main
where 
import Main.Utf8
import GHC.IO.Encoding (getLocaleEncoding)
import Data.List(intercalate)
import Data.Map(Map, fromList, (!))
import Data.List.Split
import Data.ByteString.UTF8(toString)
import Text.Read
import qualified Data.ByteString as BL
import qualified Data.Text.Lazy.Encoding as En
import Parser
main:: IO()

main = withUtf8 $ do 
        dataFromInput <- getLine
        let dataToSelect = map (\row -> word row ) (getCommandFields "SELECT" $ getParsedString dataFromInput)
        let tableName = word $ head (getCommandFields "FROM" $ getParsedString dataFromInput)
        let tableInfo = head $ filter (\row -> (row ! "name") == tableName) getTables 
        fileData <- BL.readFile (tableName++ (tableInfo ! "ext"))
        let rows = getAllRows fileData
        let headers = getHeaders (tableInfo ! "sep") rows
        let body = getBody (tableInfo ! "sep") rows
        let convertedData = getMaps headers body
        let bodyData = selectAllCols convertedData (if (length dataToSelect) == 1 && (dataToSelect !! 0) == "*" then headers else dataToSelect)
        putStrLn $ foldl (\row acc -> row ++ "\n" ++ acc) (intercalate " | " dataToSelect ++ "\n") (map (\row -> intercalate " | " row) bodyData)
        
getAllRows:: BL.ByteString -> [String]
getAllRows fileData = splitOn "\n" $ convertToString fileData

convertToString :: BL.ByteString -> String
convertToString csvData = toString csvData

getHeaders :: String -> [String] -> [String]
getHeaders delimiter (x:xs) = splitOn delimiter x

getBody :: String -> [String]-> [[String]]
getBody delimiter (x:xs) =  map (\row -> splitOn delimiter row) xs

prepareBodyToMap :: [a] -> [[b]] -> [[(a, b)]]
prepareBodyToMap headers body = map (\row -> zip headers row) body

getMaps :: Ord k => [k] -> [[a]] -> [Map k a]
getMaps headers body = Prelude.map (\row -> fromList row) $ prepareBodyToMap headers body

getCol :: Ord k => Map k a -> k -> a
getCol fileData colName = fileData ! colName

getCols ::  Ord k => Map k b -> [k] -> [b]
getCols row colsName = map (\col -> row ! col) colsName 

selectAllCols :: Ord k => [Map k b] -> [k] -> [[b]]
selectAllCols fileDataInMap colsName  = map (\row -> getCols row colsName) fileDataInMap

getTables = [
           fromList [("name","map_zal-skl9"), ("ext", ".csv"), ("sep", ",")] ,
           fromList [("name","mp-assistants"), ("ext", ".csv"), ("sep", ",")],
           fromList [("name","mp-posts_full"), ("ext", ".csv"), ("sep", ",")],
           fromList [("name","plenary_register_mps-skl9"), ("ext", ".tsv"), ("sep", "\t")]
         ]