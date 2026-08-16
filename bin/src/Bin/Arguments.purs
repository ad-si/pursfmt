module Bin.Arguments
  ( normalizeLegacyArgs
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

normalizeLegacyArgs :: Array String -> Array String
normalizeLegacyArgs = go
  where
  go args = case Array.uncons args of
    Nothing ->
      []
    Just { head, tail }
      | isSingleLineLetInFlagWithoutMode head ->
          case Array.uncons tail of
            Just { head: value, tail: rest }
              | isSingleLineLetInMode value ->
                  Array.cons head (Array.cons value (go rest))
            _ ->
              Array.cons head (Array.cons "always" (go tail))
      | otherwise ->
          Array.cons head (go tail)

  isSingleLineLetInFlagWithoutMode = case _ of
    "--single-line-let-in" -> true
    "-slli" -> true
    _ -> false

  isSingleLineLetInMode = case _ of
    "never" -> true
    "smart" -> true
    "always" -> true
    _ -> false
