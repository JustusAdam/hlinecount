module LineCount.Filter ( isAllowed ) where


import LineCount.Profile
import LineCount.Filter.Values
import LineCount.Filter.Base
import LineCount.Base


isAllowed :: MainOptions -> [Profile] -> FilePath -> Bool
isAllowed = unFilter fileFilter
