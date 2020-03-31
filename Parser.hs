module Parser(getParsedString, getCommandFields, WordToken, word, getWord)
where 
import System.Environment
import System.IO
import Data.List.Split
import Data.List

data Token = Token {
    value :: String
    , tokenType :: String 
} deriving(Show)

data WordToken = WordToken {
    word :: String
    , wordType :: String
} deriving(Show)

getTokens :: String -> [Token]
getTokens str = map (\char -> getToken char) (getCharacters str) 

getToken :: String -> Token
getToken char = case char of 
                " " -> Token " " "WHITESPACE"
                "=" -> Token "=" "EQUAL"
                ")" -> Token ")" "LPAREN"
                "(" -> Token "(" "RPAREN"
                "," -> Token "," "COMMA"
                _ -> Token char "CHAR"

getWords :: [Token] -> [String]
getWords tokens = splitOn "//" (foldl (\acc row -> acc ++ if isSeparator row then "//" else value row) "" tokens)


getWord :: String -> WordToken
getWord word = case word of 
                "select" -> WordToken "select" "SELECT"
                "distinct" -> WordToken "distinct" "DISTINCT"
                "from" -> WordToken "from" "FROM"
                "where" -> WordToken "where" "WHERE"
                "join" -> WordToken "join" "JOIN"
                "group by" -> WordToken "group by" "GROUP BY"
                "order by" -> WordToken "order by" "ORDER BY"
                _ -> WordToken word "WORD"

getWordTokens strArr = map (\row -> getWord row) strArr 

getCharacters :: String -> [String]
getCharacters str = filter (\char -> length char > 0) (splitOn "" str )

                    
isSeparator :: Token -> Bool
isSeparator token = tokenType token == "WHITESPACE" || tokenType token == "COMMA"
                                

getParsedString sql = prepareToFind $ getWordTokens $ getWords (getTokens sql)


getWordTypeIfUnique :: WordToken -> WordToken -> String 
getWordTypeIfUnique current newWord  = if wordType newWord == "WORD" then wordType current else wordType newWord

prepareToFind :: [WordToken] -> [WordToken]
prepareToFind words = foldl (\acc row -> acc ++ [WordToken (word row) (getWordTypeIfUnique (if length acc > 0 then last acc else WordToken "" "WORD") row)]) [] words

getCommandFields cmd words= tail $ filter (\row -> wordType row == cmd && word row /= "") words

