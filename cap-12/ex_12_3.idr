import Data.Primitives.Views
import System

%default total

-- ex 2

data Command : Type -> Type where
	PutStr  : String -> Command ()
	GetLine : Command String
	ReadFile : String -> Command (Either FileError String)
	WriteFile : String -> String -> Command (Either FileError ())
	Pure : ty -> Command ty
	Bind : Command a -> (a -> Command b) -> Command b

mutual
  Functor Command where
    map f x = do val <- x
                 pure $ f val

  Applicative Command where
    pure = Pure
    f <*> a = do f' <- f
                 a' <- a
                 pure $ f' a'

  Monad Command where
    (>>=) = Bind

-- ex 3 --

record Votes where
  constructor MkVotes
  upvotes : Integer
  downvotes : Integer

record Article where
  constructor MkArticle
  title : String
  url : String
  score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

getScore : Article -> Integer
getScore article = let score' = score article
                   in upvotes score' - downvotes score'

badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)

test1 : getScore Main.goodSite = 94
test1 = Refl

test2 : getScore Main.badSite = -42
test2 = Refl

-- ex 4 --

addUpvote : Article -> Article
addUpvote article = record { score->upvotes $= (+1) } article

addDownvote : Article -> Article
addDownvote article = record { score->downvotes $= (+1) } article

test3 : getScore (addUpvote Main.goodSite) = 95
test3 = Refl
