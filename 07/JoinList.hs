{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import           Data.Monoid ((<>))
import           Sized

data JoinList m a = Empty
        | Single m a
        | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single n _)   = n
tag (Append n _ _) = n

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

idx :: Sized a => a -> Int
idx = getSize . size

getListSize :: (Sized b, Monoid b) => JoinList b a -> Size
getListSize = size . tag

indexJ :: (Sized b, Monoid b) =>
        Int -> JoinList b a -> Maybe a
indexJ i jl
        | i < 0 || Size i > getListSize jl = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Append _ jll jlr)
        | Size i < getListSize jll = indexJ i jll
        | otherwise = indexJ (i - idx (tag jll)) jlr
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) =>
        Int -> JoinList b a -> JoinList b a
dropJ n jl
        | Size n >= getListSize jl = Empty
        | n <= 0 = jl
dropJ n (Append _ jll jlr)
        | Size n < getListSize jll = dropJ n jll +++ jlr
        | otherwise = dropJ (n - idx (tag jll)) jlr
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) =>
        Int -> JoinList b a -> JoinList b a
takeJ n jl
        | n <= 0 = Empty
        | Size n >= getListSize jl = jl
takeJ n (Append _ jll jlr)
        | Size n < getListSize jll = takeJ n jll
        | otherwise = jll +++ takeJ (n - idx (tag jll)) jlr
takeJ _ _ = Empty
