module Util where

import Data.Bifunctor (second)


(.:) = (.) . (.)
infixl 7 .:


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

