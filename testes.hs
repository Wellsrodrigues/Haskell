
module Data.Tree.Pretty
       ( -- * Drawing trees
         drawVerticalTree
       , drawVerticalTreeWith
         -- * Drawing forests
       , drawVerticalForest
       , drawVerticalForestWith
         -- * Widths of gaps between trees.
       , Width
       , defaultGap
         -- * Helper functions
       , treeToBox
       ) where

import Data.Tree
import Data.List(intersperse)
--import Text.PrettyPrint.Boxes
import Control.Monad(ap, liftM2)

drawVerticalTree :: Tree String -> String
drawVerticalTree = drawVerticalTreeWith defaultGap

tree :: Tree String
tree = Node "hello" [ Node "foo" []
                     , Node "bars" [ Node "oi!" []
                                   , Node "baz" [ Node "a" [ Node "b" []
                                                           , Node "c" []]
                                                , Node "d" [ Node "e" []]]]
                     , Node "foobar" []]