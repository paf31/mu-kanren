module Kanren.Stream where
    
import Kanren.State    
    
data Stream
  = StrNil
  | StrDelay (Unit -> Stream)
  | StrCons State Stream

strNull :: Stream -> Boolean
strNull StrNil = true
strNull _     = false

mzero :: Stream
mzero = StrNil