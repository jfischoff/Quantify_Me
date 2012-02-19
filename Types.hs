{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}
module Types where
import Control.Monad.RWS
import Control.Monad.Error
import qualified Data.HashMap.Strict as H
import Data.IORef
import System.Exit
import Text.Parsec hiding (State)
import Text.Parsec.Token
import Data.Data
import Text.Parsec.Language



type Id = Int

type UserId   = Id
type Device   = String
type Location = String
type Value    = Double
type Time     = Double  -- MSecSinceEpoch
type TimeRange = (Time, Time) 

data Origin = Origin Device Location
    deriving(Eq, Show, Read, Data, Typeable)

data Query = TimeRangeQuery UserId TimeRange
    deriving(Eq, Show, Read, Data, Typeable)
 
data Request = GetRequest Query
             | PutRequest [OriginTimeSeries]
    deriving(Eq, Show, Read, Data, Typeable)

data IncomingPayload = IncomingPayload 
    {
        requesting_user :: UserId,
        request         ::  Request
    }
    deriving(Eq, Show, Read, Data, Typeable)
    
type OriginTimeSeries = (Origin, TypedTimeSeries)

type TimeSeries = [(Time, Value)]

data TypedTimeSeries = TypedTimeSeries SignalType TimeSeries
    deriving(Eq, Show, Read, Data, Typeable)

type TypedTimeSeriesCollection = [TypedTimeSeries]
    
data UnitType = Hertz
              | Pascals
    deriving(Eq, Show, Read, Data, Typeable)              
    
data SignalType = Heartrate      
                | BloodPressure 
    deriving(Eq, Show, Read, Data, Typeable)
              
type User = (UserId, [OriginTimeSeries])             

type State = H.HashMap UserId [OriginTimeSeries]

type EvalStateT m a = ErrorT String (RWST () () State m) a 

type EvalState a = EvalStateT IO a

data Response = GetResponse [OriginTimeSeries]
              | PutResponse
    deriving(Eq, Show, Read, Data, Typeable)

data OutgoingPayload = OutgoingPayload UserId Response 
    deriving(Eq, Show, Read, Data, Typeable)

eval :: IncomingPayload -> EvalState OutgoingPayload
eval (IncomingPayload userId request) = eval' userId request

--un_maybe Nothing = throwError 
--    un_maybe `fmap` 
eval' :: UserId -> Request -> EvalState OutgoingPayload
eval' requester_id (GetRequest (TimeRangeQuery target_user_id time_range)) = do
    (Just user_data) <- lift $ gets (H.lookup target_user_id)
    return $ OutgoingPayload requester_id $ GetResponse $ 
                get_timeseries_by_range time_range user_data
    
eval' userId (PutRequest dat)   = do
      lift $ modify (H.insertWith (++) userId dat)
      return $ OutgoingPayload userId PutResponse

get_timeseries_by_range time_range user_data = undefined

------------------------------------------------------------------------------------------------


run_parser :: String -> IncomingPayload
run_parser s = (\(Right x) -> x) $ runParser parse_input () "" s
--run_parser = undefined

run_evaluator state incoming_payload = (runRWST (runErrorT (eval incoming_payload)) () state) 
--run_evaluator = undefined

--isRight = undefined
--fromRight = undefined

main = do
  let app_state = H.empty 
  app_state_ref <- newIORef app_state

  loop app_state_ref

loop app_state_ref = do
    state <- readIORef app_state_ref

    line <- getLine

    abort_if line "q"

    let parse_output = run_parser line
    print parse_output

    --if isRight parse_output
    --  then do
    --let input = fromRight parse_output

    (output, new_state, ()) <- run_evaluator state parse_output

    --print output
    print new_state

    writeIORef app_state_ref new_state

    loop app_state_ref

    --  else loop app_state_ref
    

abort_if line str = do
  if line == str
     then exitSuccess
     else return ()   
     
     
---------------------------------------------------------------------------------------------
--Parser 
  
parse_input = try p_get <|> try p_put

p_get = do 
    spaces
    string "get"
    spaces
    requesting_user_id <- p_user_id
    spaces 
    user_id <- p_user_id
    spaces
    start <- p_time
    spaces
    end <- p_time 
    return $ IncomingPayload requesting_user_id $ GetRequest $ TimeRangeQuery user_id (start, end)

p_put = do
    spaces
    string "put"
    spaces
    user_id <- p_user_id
    spaces
    dat <- sepBy1 p_location_time_series (try $ string ",")
    return $ IncomingPayload user_id $ PutRequest dat
    
p_user_id =  fromIntegral `fmap` (integer haskell)

p_location_time_series = do
    origin <- p_origin
    spaces
    time_series <- p_typed_time_series
    return (origin, time_series)
    
p_origin = do 
    device   <- p_device
    spaces
    location <- p_location
    return $ Origin device location

p_device = (identifier haskell)
p_location = (identifier haskell)
    
p_typed_time_series = do
    typ <- p_type
    spaces
    timeseries <- p_timeseries
    return $ TypedTimeSeries typ timeseries
    
p_type = p_heartrate <|> p_bloodpressure

p_heartrate = do
    string "heartrate" 
    return Heartrate 
    
p_bloodpressure = do
    string "blood pressure"
    return BloodPressure
    
p_timeseries = (braces haskell) $ sepBy1 time_datum (try $ string ",")

time_datum = do
    time  <- p_time
    spaces
    value <- (float haskell)
    return (time, value)
    
p_time = (float haskell)
    


     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
                  
                
                

                