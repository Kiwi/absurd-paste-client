{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

-- Main.imports
import qualified Data.ByteString               as S
import qualified Data.ByteString.Char8         as C

import           Control.Monad                  ( forM
                                                , when
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Either                    ( rights )
import           Network.Http.Client            ( Hostname
                                                , Method(POST)
                                                , Port
                                                , baselineContextSSL
                                                , buildRequest1
                                                , encodedFormBody
                                                , getStatusMessage
                                                , http
                                                , openConnectionSSL
                                                , receiveResponse
                                                , sendRequest
                                                , setAccept
                                                , setContentType
                                                , setHostname
                                                , withConnection
                                                )
import           Options.Applicative            ( (<**>)
                                                , (<|>)
                                                , Parser
                                                , ParserInfo
                                                , ReadM
                                                , argument
                                                , eitherReader
                                                , execParser
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
                                                , value
                                                )

import qualified System.IO.Streams             as Streams

data Uppity =
  Uppity
    { debug :: Mode
    , language :: Language
    , expiretime :: ExpireTime
    , inputs :: [Input]
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

data Input
  = FileInput FilePath
  | StdInput
  deriving (Show)

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

parseLanguages :: ReadM Language
parseLanguages = eitherReader $ \case
  "text"        -> Right LangText
  "markdown"    -> Right LangMarkdown
  "rst"         -> Right LangRst
  "code"        -> Right LangCode
  "abap"        -> Right LangAbap
  "apacheconf"  -> Right LangApacheconf
  "applescript" -> Right LangApplescript
  "as"          -> Right LangAs
  "bash"        -> Right LangBash
  "bbcode"      -> Right LangBbcode
  "c"           -> Right LangC
  "cpp"         -> Right LangCpp
  "clojure"     -> Right LangClojure
  "cobol"       -> Right LangCobol
  "css"         -> Right LangCss
  "cuda"        -> Right LangCuda
  "dart"        -> Right LangDart
  "delphi"      -> Right LangDelphi
  "diff"        -> Right LangDiff
  "django"      -> Right LangDjango
  "erlang"      -> Right LangErlang
  "fortran"     -> Right LangFortran
  "go"          -> Right LangGo
  "groovy"      -> Right LangGroovy
  "haml"        -> Right LangHaml
  "haskell"     -> Right LangHaskell
  "html"        -> Right LangHtml
  "http"        -> Right LangHttp
  "ini"         -> Right LangIni
  "irc"         -> Right LangIrc
  "java"        -> Right LangJava
  "js"          -> Right LangJs
  "json"        -> Right LangJson
  "lua"         -> Right LangLua
  "make"        -> Right LangMake
  "mako"        -> Right LangMako
  "mason"       -> Right LangMason
  "matlab"      -> Right LangMatlab
  "modula2"     -> Right LangModula2
  "monkey"      -> Right LangMonkey
  "mysql"       -> Right LangMysql
  "numpy"       -> Right LangNumpy
  "objc"        -> Right LangObjc
  "ocaml"       -> Right LangOcaml
  "perl"        -> Right LangPerl
  "php"         -> Right LangPhp
  "postscript"  -> Right LangPostscript
  "powershell"  -> Right LangPowershell
  "prolog"      -> Right LangProlog
  "properties"  -> Right LangProperties
  "puppet"      -> Right LangPuppet
  "python"      -> Right LangPython
  "r"           -> Right LangR
  "rb"          -> Right LangRb
  "rust"        -> Right LangRust
  "sass"        -> Right LangSass
  "scala"       -> Right LangScala
  "scheme"      -> Right LangScheme
  "scilab"      -> Right LangScilab
  "scss"        -> Right LangScss
  "smalltalk"   -> Right LangSmalltalk
  "smarty"      -> Right LangSmarty
  "solidity"    -> Right LangSolidity
  "sql"         -> Right LangSql
  "tcl"         -> Right LangTcl
  "tcsh"        -> Right LangTcsh
  "tex"         -> Right LangTex
  "vb.net"      -> Right LangVbNet
  "vim"         -> Right LangVim
  "xml"         -> Right LangXml
  "xquery"      -> Right LangXquery
  "xslt"        -> Right LangXslt
  "yaml"        -> Right LangYaml
  _             -> Left "Invalid language"

parseLanguage :: Parser Language
parseLanguage = option
  parseLanguages
  (  long "language"
  <> short 'l'
  <> help "Language for syntax highlighting"
  <> metavar "LANGUAGE"
  <> value LangDefault
  )

parseExpireTime :: ReadM ExpireTime
parseExpireTime = eitherReader $ \case
  "never"   -> Right ExpNever
  "1-day"   -> Right ExpDay
  "1-week"  -> Right ExpWeek
  "1-month" -> Right ExpMonth
  "onetime" -> Right ExpOneTime
  "default" -> Right ExpDefault
  _         -> Left "Invalid expire time"

parseExpires :: Parser ExpireTime
parseExpires = option
  parseExpireTime
  (  long "expires"
  <> short 'e'
  <> help "How long before paste expires"
  <> metavar "EXPIRES"
  <> value ExpDefault
  )

uppity :: Parser Uppity
uppity = Uppity <$> parseMode <*> parseLanguage <*> parseExpires <*> input

main :: IO ()
main = execParser opts >>= run

opts :: ParserInfo Uppity
opts = info
  (uppity <**> helper)
  (fullDesc <> progDesc "the mostly absurd paste client aka apc" <> header
    "absurd-paste-client"
  )

run :: Uppity -> IO ()
run (Uppity mode lang exptime targets) = do
  strs <- forM
    targets
    (\t -> do
      x <- getInputString t
      apcPost (dpasteOrg (C.pack x) lang exptime) mode
    )
  mapM_ S.putStr (rights strs)

type Content = C.ByteString

type Format = C.ByteString

type Lexer = C.ByteString

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
  | ExpOneTime
  | ExpNever
  deriving (Show, Read)

dpasteOrg :: Content -> Language -> ExpireTime -> Service
dpasteOrg c l et = Service { s_endpoint = "/api/"
                           , s_host     = "dpaste.org"
                           , s_port     = 443
                           , s_name     = "dpaste.org"
                           , s_nvs      = dpasteOrgNvs c l et
                           }

dpasteOrgNvs :: Content -> Language -> ExpireTime -> NameValues
dpasteOrgNvs c l et = NameValues
  [ ("content", c)
  , ("format" , "url")
  , ("lexer"  , dpasteOrgLanguages l)
  , ("expires", dpasteOrgExpires et)
  ]

{- Valid values are: 3600, 86400, 604800, onetime -}
dpasteOrgExpires :: ExpireTime -> Expires
dpasteOrgExpires = \case
  ExpHour    -> "3600"
  ExpDay     -> "86400"
  ExpWeek    -> "604800"
  ExpOneTime -> "onetime"
  _          -> "3600"

{-
_text, _markdown, _rst, _code, abap, apacheconf, applescript, as, bash, bbcode
, c, cpp, clojure, cobol, css, cuda, dart, delphi, diff, django, erlang, fortran
, go, groovy, haml, haskell, html, http, ini, irc, java, js, json, lua, make
, mako, mason, matlab, modula2, monkey, mysql, numpy, objc, ocaml, perl, php
, postscript, powershell, prolog, properties, puppet, python, r, rb, rst, rust
, sass, scala, scheme, scilab, scss, smalltalk, smarty, solidity, sql, tcl, tcsh
, tex, vb.net, vim, xml, xquery, xslt, yaml
-}
data Language
  = LangText
  | LangMarkdown
  | LangRst
  | LangCode
  | LangAbap
  | LangApacheconf
  | LangApplescript
  | LangAs
  | LangBash
  | LangBbcode
  | LangC
  | LangCpp
  | LangClojure
  | LangCobol
  | LangCss
  | LangCuda
  | LangDart
  | LangDelphi
  | LangDiff
  | LangDjango
  | LangErlang
  | LangFortran
  | LangGo
  | LangGroovy
  | LangHaml
  | LangHaskell
  | LangHtml
  | LangHttp
  | LangIni
  | LangIrc
  | LangJava
  | LangJs
  | LangJson
  | LangLua
  | LangMake
  | LangMako
  | LangMason
  | LangMatlab
  | LangModula2
  | LangMonkey
  | LangMysql
  | LangNumpy
  | LangObjc
  | LangOcaml
  | LangPerl
  | LangPhp
  | LangPostscript
  | LangPowershell
  | LangProlog
  | LangProperties
  | LangPuppet
  | LangPython
  | LangR
  | LangRb
  | LangRust
  | LangSass
  | LangScala
  | LangScheme
  | LangScilab
  | LangScss
  | LangSmalltalk
  | LangSmarty
  | LangSolidity
  | LangSql
  | LangTcl
  | LangTcsh
  | LangTex
  | LangVbNet
  | LangVim
  | LangXml
  | LangXquery
  | LangXslt
  | LangYaml
  | LangDefault

dpasteOrgLanguages :: Language -> Lexer
dpasteOrgLanguages = \case
  LangText        -> "_text"
  LangMarkdown    -> "_markdown"
  LangRst         -> "_rst"
  LangCode        -> "_code"
  LangAbap        -> "abap"
  LangApacheconf  -> "apacheconf"
  LangApplescript -> "applescript"
  LangAs          -> "as"
  LangBash        -> "bash"
  LangBbcode      -> "bbcode"
  LangC           -> "c"
  LangCpp         -> "cpp"
  LangClojure     -> "clojure"
  LangCobol       -> "cobol"
  LangCss         -> "css"
  LangCuda        -> "cuda"
  LangDart        -> "dart"
  LangDelphi      -> "delphi"
  LangDiff        -> "diff"
  LangDjango      -> "django"
  LangErlang      -> "erlang"
  LangFortran     -> "fortran"
  LangGo          -> "go"
  LangGroovy      -> "groovy"
  LangHaml        -> "haml"
  LangHaskell     -> "haskell"
  LangHtml        -> "html"
  LangHttp        -> "http"
  LangIni         -> "ini"
  LangIrc         -> "irc"
  LangJava        -> "java"
  LangJs          -> "js"
  LangJson        -> "json"
  LangLua         -> "lua"
  LangMake        -> "make"
  LangMako        -> "mako"
  LangMason       -> "mason"
  LangMatlab      -> "matlab"
  LangModula2     -> "modula2"
  LangMonkey      -> "monkey"
  LangMysql       -> "mysql"
  LangNumpy       -> "numpy"
  LangObjc        -> "objc"
  LangOcaml       -> "ocaml"
  LangPerl        -> "perl"
  LangPhp         -> "php"
  LangPostscript  -> "postscript"
  LangPowershell  -> "powershell"
  LangProlog      -> "prolog"
  LangProperties  -> "properties"
  LangPuppet      -> "puppet"
  LangPython      -> "python"
  LangR           -> "r"
  LangRb          -> "rb"
  LangRust        -> "rust"
  LangSass        -> "sass"
  LangScala       -> "scala"
  LangScheme      -> "scheme"
  LangScilab      -> "scilab"
  LangScss        -> "scss"
  LangSmalltalk   -> "smalltalk"
  LangSmarty      -> "smarty"
  LangSolidity    -> "solidity"
  LangSql         -> "sql"
  LangTcl         -> "tcl"
  LangTcsh        -> "tcsh"
  LangTex         -> "tex"
  LangVbNet       -> "vb.net"
  LangVim         -> "vim"
  LangXml         -> "xml"
  LangXquery      -> "xquery"
  LangXslt        -> "xslt"
  LangYaml        -> "yaml"
  _               -> "_text"

apcPost :: Service -> Mode -> IO (Either C.ByteString C.ByteString)
apcPost Service { s_host = host, s_port = port, s_endpoint = endpoint, s_nvs = nvs } mode
  = do
    ctx <- baselineContextSSL
    withConnection
      (openConnectionSSL ctx host port)
      (\c -> do
        let q = buildRequest1 $ do
              http POST endpoint
              setContentType "application/x-www-form-urlencoded"
              setHostname host port
              setAccept "text/html"
        sendRequest c q (encodedFormBody $ toBody nvs)
        receiveResponse
          c
          (\p i -> do
            when (mode == Debug) . C.putStr $ C.pack (show p)
            xm <- Streams.read i
            return $ case xm of
              Just x  -> Right x
              Nothing -> Left $ getStatusMessage p
          )
      )
