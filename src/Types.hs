{-# LANGUAGE DerivingStrategies #-}
-- |

module Types where

import           Network.Http.Client            ( Hostname
                                                , Port
                                                )
import qualified Data.ByteString               as S
import qualified Data.ByteString.Char8         as C
import           Data.Coerce                    ( coerce )
import           Language                       ( Language
                                                , Lexer
                                                )

data Input
  = FileInput FilePath
  | StdInput
  deriving (Show)

data Uppity =
  Uppity
    { inputs :: [Input]
    , service :: ServiceName
    , language :: Language
    , expiretime :: ExpireTime
    , listServices :: Bool
    , serviceLangs :: [Lexer]
    , debug :: Mode
    }

data Mode
  = Debug
  | Normal
  deriving (Eq)

data Service =
  Service
    { s_endpoint :: S.ByteString
    , s_name :: String
    , s_nvs :: NameValues
    , s_port :: Port
    , s_host :: Hostname
    }
  deriving (Show)

data ServiceName = DpasteCom | DpasteOrg

type Content = C.ByteString

type Format = C.ByteString

type Expires = C.ByteString

type Name = C.ByteString

type Value = C.ByteString

newtype NameValues =
  NameValues [(Name, Value)]
  deriving (Show, Read)

toBody :: NameValues -> [(C.ByteString, C.ByteString)]
toBody = coerce

data ExpireTime
  = ExpDefault
  | ExpHour
  | ExpDay
  | ExpWeek
  | ExpMonth
  | ExpYear
  | ExpOneTime
  | ExpNever
  | ExpDays Int
  deriving (Show, Read)
