-- | Export .org file from an iCalendar file.

import ICal.Org
import System.Environment

-- | Export .org file from an iCalendar file.
main :: IO ()
main =
  do args <- getArgs
     case args of
       [ics,org] -> exportFromToFile ics org
       _ -> error "expected <input.ics> <output.org>"
