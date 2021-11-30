{-# LANGUAGE NoMonomorphismRestriction #-}

module Util where

import Data.List
import Data.Bifunctor (first, second)
import Control.Error


(.:) = (.) . (.)
infixl 7 .:

fmap2 = fmap . fmap

(<<$>>) = fmap2
infixl 4 <<$>>


removeAt :: Int -> [a] -> Maybe (a, [a])
-- remove the element at index n from a list
-- returns the removed element and the remaining list
-- fails if n >= length xs
removeAt 0 (x : xs) = Just (x, xs)
removeAt _ []       = Nothing
removeAt n (x : xs) = second (x :) <$> removeAt (n - 1) xs

pullElem :: Int -> [a] -> Maybe [a]
-- pull element at index n to the head of the list
-- fails if n >= length xs
pullElem n xs = uncurry (:) <$> removeAt n xs

insertAt :: Int -> a -> [a] -> Maybe [a]
-- insert an element at the nth position from the head
-- fails if n >= length xs
insertAt n x xs = ins n xs
  where
    ins 0 xs        = Just (x : xs)
    ins _ []        = Nothing
    ins n (x' : xs) = (x' :) <$> ins (n - 1) xs

pushElem :: Int -> [a] -> Maybe [a]
-- push the head of the list back n elements
-- pre: n >= 1
-- fails if n >= length xs
pushElem n (x : xs) = insertAt n x xs
pushElem _ _        = Nothing

splitOn :: Eq a => a -> [a] -> Maybe ([a], [a])
-- split on an element of a list
-- the sublists the the left and right are returned
splitOn x' (x : xs)
  | x == x'   = Just ([], xs)
  | otherwise = first (x :) <$> splitOn x' xs
splitOn _ _   = Nothing  -- x' not present

stripPrefixes :: String -> [String] -> Maybe String
-- try to strip each prefix from the string
-- if any match, the remaining string after the prefix is returned
stripPrefixes s = foldl tryStrip Nothing
  where tryStrip (Just s') _ = Just s'
        tryStrip _         p = stripPrefix p s

stripChar :: Char -> String -> (String, Bool)
-- if the string begins with c, strip it
-- otherwise do nothing
-- Bool describes whether the character was found
stripChar c' (c : s) | c == c' = (s, True)
stripChar _  s                 = (s, False)

stripEndChar :: Char -> String -> (String, Bool)
-- same as stripChar but on the final character of the string
stripEndChar _ [] = ([], False)
stripEndChar c s
  | last s == c = (init s, True )
  | otherwise   = (s,      False)

stripBrackets :: String -> Maybe String
-- strip square brackets from each end of a string
stripBrackets s = do
  '[' <- headMay s
  ']' <- lastMay s
  return . tail . init $ s

