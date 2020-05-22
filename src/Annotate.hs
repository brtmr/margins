module Annotate where

import qualified Data.Text as T

data Location = Lines {
      _start :: Int
    , _len   :: Int   } 
        deriving (Eq)

instance Show Location where

    show l = 
        (show $ _start l) ++ "," ++ (show $ _len l)

newtype Identifier = Id T.Text deriving (Show, Eq)
plain :: Identifier -> T.Text
plain (Id s) = s

data Annotation = Annotation {
      _loc :: Location
    , _id :: Identifier
    , _text :: T.Text 
    } 
       deriving (Show, Eq)

data Document = Document {
      _src :: T.Text
    , _margins :: [Annotation]
} 
    deriving (Show, Eq) 

data MarginAction = 
    Add Annotation
    | Modify (Annotation -> Maybe Annotation)
    | Remove Identifier 


-- Annotations are stored in a file seperate from the original source file we
-- are annotating.
-- Each Margin is preceeded by a line storing its metadata.

header :: Annotation -> T.Text
header annot = T.concat [
    T.pack $ show $ _loc annot,
    T.pack ",",
    plain $ _id annot,
    T.pack "\n"
    ]

astext :: Annotation -> T.Text
astext annot = T.concat [
    header annot, _text annot
    ]

