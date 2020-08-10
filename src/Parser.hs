{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Parser where
import           Options.Applicative            ( (<**>)
                                                , (<|>)
                                                , Parser
                                                , ParserInfo
                                                , ReadM
                                                , argument
                                                , eitherReader
                                                , flag
                                                , flag'
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , many
                                                , metavar
                                                , option
                                                , progDesc
                                                , short
                                                , some
                                                , str
                                                , strOption
                                                , switch
                                                , value
                                                )
-- import Data.Functor((<&>))

import qualified Text.Parsec                   as P
import           Types                          ( Uppity(Uppity)
                                                , Input(StdInput, FileInput)
                                                , Mode(Normal, Debug)
                                                , ExpireTime
                                                  ( ExpDays
                                                  , ExpNever
                                                  , ExpDay
                                                  , ExpWeek
                                                  , ExpMonth
                                                  , ExpOneTime
                                                  , ExpDefault
                                                  )
                                                , ServiceName
                                                  ( DpasteOrg
                                                  , DpasteCom
                                                  )
                                                )
import           Language                       ( Language
                                                , parseLanguages
                                                , Language(LangDefault)
                                                , parseLanguage
                                                , langToLexer
                                                , Lexer
                                                , unLexer
                                                )
import           LanguageDpasteOrg              ( dpasteOrgLanguages )
import           LanguageDpasteCom              ( dpasteComLanguages )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.ByteString.Char8         as C
import           Data.Either                    ( rights )

fileInput :: Parser [Input]
fileInput = some
  (FileInput <$> strOption
    (long "file" <> short 'f' <> metavar "FILENAME" <> help "Input file")
  )

filesInputs :: Parser [Input]
filesInputs = many (FileInput <$> argument str (metavar "FILES..."))

stdInput :: Parser [Input]
stdInput = some (flag' StdInput (long "stdin" <> help "Paste from stdin"))

input :: Parser [Input]
input = fileInput <|> stdInput <|> filesInputs

getInputString :: Input -> IO String
getInputString (FileInput fp) = readFile fp
getInputString StdInput       = getContents

parseMode :: Parser Mode
parseMode =
  flag Normal Debug (long "debug" <> short 'd' <> help "Enable debug mode")

parseLanguage :: Parser Language
parseLanguage = option
  parseLanguages
  (  long "language"
  <> short 'l'
  <> help "language for syntax highlighting"
  <> metavar "LANGUAGE"
  <> value LangDefault
  )

parseExpDays :: String -> Either String ExpireTime
parseExpDays s = case P.parse (P.many1 P.digit) "" s of
  Left  _  -> Left "Invalid expire time"
  Right xs -> case read xs of
    x -> if x <= 365 && x > 0
      then Right $ ExpDays x
      else Left "Invalid expire time"

parseExpireTime :: ReadM ExpireTime
parseExpireTime = eitherReader $ \case
  "never"                      -> Right ExpNever
  "1-day"                      -> Right ExpDay
  "1-week"                     -> Right ExpWeek
  "1-month"                    -> Right ExpMonth
  "onetime"                    -> Right ExpOneTime
  "default"                    -> Right ExpDefault
  [d, '-', 'd', 'a', 'y', 's'] -> parseExpDays [d]
  [d, d1, '-', 'd', 'a', 'y', 's'] -> parseExpDays [d, d1]
  [d, d1, d2, '-', 'd', 'a', 'y', 's'] -> parseExpDays [d, d1, d2]
  _                            -> Left "Invalid expire time"

parseExpires :: Parser ExpireTime
parseExpires = option
  parseExpireTime
  (  long "expires"
  <> short 'e'
  <> help "How long before paste expires"
  <> metavar "EXPIRES"
  <> value ExpDefault
  )

parseLanguageService :: ReadM [Lexer]
parseLanguageService = eitherReader $ \case
  "dpaste.org" -> Right $ langsForService dpasteOrgLanguages
  "dpaste.com" -> Right $ langsForService dpasteComLanguages
  _            -> Left "Invalid service name"

parserService :: Parser ServiceName
parserService = option
  parseService
  (  long "service"
  <> short 's'
  <> help "Paste service to use"
  <> metavar "SERVICE"
  <> value DpasteCom
  )

parseService :: ReadM ServiceName
parseService = eitherReader $ \case
  "dpaste.com" -> Right DpasteCom
  "dpaste.org" -> Right DpasteOrg
  _            -> Left "Invalid service name"

parseListLanguages :: Parser [Lexer]
parseListLanguages = option
  parseLanguageService
  (  long "languages"
  <> short 'L'
  <> help "list supported languages for SERVICE"
  <> metavar "SERVICE"
  <> value []
  )

parseListExpires :: ReadM [String]
parseListExpires = eitherReader $ \case
  "dpaste.com" ->
    Right ["1-day", "1-week", "1-month", "1-year", "[1-365]-days"]
  "dpaste.org" -> Right ["onetime", "1-hour", "1-day", "1-week"]
  _            -> Left "Invalid service name"

parserListExpires :: Parser [String]
parserListExpires = option
  parseListExpires
  (  long "expiretimes"
  <> short 'E'
  <> help "list supported expiretimes for SERVICE"
  <> metavar "SERVICE"
  <> value []
  )

parserListServices :: Parser Bool
parserListServices =
  switch (long "services" <> short 'S' <> help "list supported services")

uppity :: Parser Uppity
uppity =
  Uppity
    <$> input
    <*> parserService
    <*> Parser.parseLanguage
    <*> parseExpires
    <*> parserListExpires
    <*> parserListServices
    <*> parseListLanguages
    <*> parseMode

opts :: ParserInfo Uppity
opts = info
  (uppity <**> helper)
  (fullDesc <> progDesc "The (mostly) absurd paste client AKA apc" <> header
    "absurd-paste-client"
  )

allLangs :: [Language]
allLangs = [(minBound :: Language) .. (maxBound :: Language)]

allLangsNoDefault :: [Language]
allLangsNoDefault = filter (/= LangDefault) allLangs

listLangsForService :: [Language] -> (Language -> Maybe Lexer) -> [Lexer]
listLangsForService = flip mapMaybe

langsForService :: (Language -> Maybe Lexer) -> [Lexer]
langsForService =
  map langToLexer
    . rights
    . map (Language.parseLanguage . C.unpack . unLexer)
    . listLangsForService allLangsNoDefault
