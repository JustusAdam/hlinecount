{-# LANGUAGE UnicodeSyntax #-}
module LineCount.Filter ( isAllowed ) where


import           LineCount.Base
import           LineCount.Filter.Base
import           LineCount.Filter.Values
import           LineCount.Profile


isAllowed ∷ MainOptions → [Profile] → FilePath → Bool
isAllowed = unFilter fileFilter
