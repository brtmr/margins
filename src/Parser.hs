module Parser where

import Annotate (Annotation(..), Location(..), Identifier(..))

import qualified Data.Attoparsec.Text as AT
import qualified Data.Text as T

separator :: AT.Parser Char
separator = AT.char ','

line :: AT.Parser T.Text
line = do
    l <- AT.takeWhile1 (not . AT.isEndOfLine)
    _ <- AT.endOfLine 
    return $ T.concat [l, T.pack "\n"] 
    

nlines :: Int -> AT.Parser T.Text
nlines n = T.concat <$> AT.count n line
    

annotation :: AT.Parser Annotation
annotation = do 
    start <- AT.decimal
    _     <- separator
    len   <- AT.decimal
    _     <- separator
    identifier <- fmap Id $ AT.takeTill (AT.isEndOfLine)
    _ <- AT.endOfLine 
    text <- nlines len
    return $ Annotation (Lines start len) identifier text


fromText :: T.Text -> Maybe Annotation
fromText txt = 
    case (AT.parse annotation txt) of
        AT.Done _ result  -> Just result 
        _              -> Nothing
