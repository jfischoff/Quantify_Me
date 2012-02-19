{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Quote where
import Types

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.Parsec


parseExpr (file, line, col) s =
    case runParser parse_input () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
 
incoming  :: QuasiQuoter
incoming  =  QuasiQuoter quoteStateExp undefined undefined undefined

quoteStateExp :: String -> TH.ExpQ
quoteStateExp s =  do  loc <- TH.location
                       let pos =  (TH.loc_filename loc,
                                     fst (TH.loc_start loc),
                                     snd (TH.loc_start loc))
                       state <- parseExpr pos s
                       dataToExpQ (const Nothing) state    




