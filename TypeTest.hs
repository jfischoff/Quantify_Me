{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module TypeTest where
import Types
import Quote



test_input_0 = [incoming| put 1 device location heartrate {1.0 1.0,2.0 3.0,3.0 4.0} |]

test_input_1 =  [incoming| get 1 1 1.0 2.0 |]