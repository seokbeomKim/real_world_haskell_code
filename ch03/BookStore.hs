-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
              deriving (Show)
data MagazineInfo = Magazine Int String [String]
                  deriving (Show)

myInfo = Book 293849322 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

-- We will introduce the CustomerID type shortly. Following definition
-- says that the type named BookReview has a value constructor that is
-- also named BookReview
data BookReview = BookReview BookInfo CustomerID String

-- Type Synonyms
type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

nicerID (Book id _ _) = id
nicerTitle (Book _ title _) = title
nicerAuthors (Book _ _ authors) = authors
