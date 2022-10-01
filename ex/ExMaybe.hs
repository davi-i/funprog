module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

applyMaybe :: (a -> b) -> Maybe a -> Maybe b
applyMaybe _ Nothing = Nothing
applyMaybe f (Just x) = Just $ f x

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:mxs) = catMaybes mxs
catMaybes ((Just x):mxs) = x : catMaybes mxs

fromJust :: Maybe a -> a
fromJust Nothing = error "not a just"
fromJust (Just x) = x

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f 

maybe :: b -> (a -> b) -> Maybe a -> b
maybe def f = fromMaybe def . applyMaybe f

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith [] xs = xs
tryToModifyWith _ [] = []
tryToModifyWith (Nothing:mfs) (x:xs) = x : tryToModifyWith mfs xs
tryToModifyWith ((Just f):mfs) (x:xs) = f x : tryToModifyWith mfs xs
