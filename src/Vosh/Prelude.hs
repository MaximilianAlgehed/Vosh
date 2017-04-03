module Vosh.Prelude ( ($)
                    , (.)
                    , flip
                    , pure
                    , (<$>)
                    , (<*>)
                    , min
                    , max
                    , odd
                    , even
                    , div
                    , Int
                    , Show
                    , show
                    , zip
                    , zipWith
                    , foldl
                    , IO
                    , print
                    , interleave
                    , reverse
                    ) where

import Prelude

-- | Interleave two lists
interleave :: [a] -> [a] -> [a]
interleave [] xs = xs
interleave xs [] = xs
interleave (x:xs) ys = x : interleave ys xs
