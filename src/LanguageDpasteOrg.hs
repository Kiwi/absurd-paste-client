{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module LanguageDpasteOrg where
import           Types                          ( Content
                                                , ExpireTime(..)
                                                , Expires
                                                , Service
                                                  ( Service
                                                  , s_endpoint
                                                  , s_host
                                                  , s_port
                                                  , s_name
                                                  , s_nvs
                                                  )
                                                , NameValues(NameValues)
                                                )
import           Language                       ( Language(..)
                                                , Lexer(Lexer)
                                                , unLexer
                                                )

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
  , ("lexer", maybe "_text" unLexer (dpasteOrgLanguages l))
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

dpasteOrgLanguages :: Language -> Maybe Lexer
dpasteOrgLanguages = \case
  LangText        -> Just $ Lexer "_text"
  LangMarkdown    -> Just $ Lexer "_markdown"
  LangRst         -> Just $ Lexer "_rst"
  LangCode        -> Just $ Lexer "_code"
  LangAbap        -> Just $ Lexer "abap"
  LangApacheconf  -> Just $ Lexer "apacheconf"
  LangApplescript -> Just $ Lexer "applescript"
  LangAs          -> Just $ Lexer "as"
  LangBash        -> Just $ Lexer "bash"
  LangBbcode      -> Just $ Lexer "bbcode"
  LangC           -> Just $ Lexer "c"
  LangCpp         -> Just $ Lexer "cpp"
  LangClojure     -> Just $ Lexer "clojure"
  LangCobol       -> Just $ Lexer "cobol"
  LangCss         -> Just $ Lexer "css"
  LangCuda        -> Just $ Lexer "cuda"
  LangDart        -> Just $ Lexer "dart"
  LangDelphi      -> Just $ Lexer "delphi"
  LangDiff        -> Just $ Lexer "diff"
  LangDjango      -> Just $ Lexer "django"
  LangErlang      -> Just $ Lexer "erlang"
  LangFortran     -> Just $ Lexer "fortran"
  LangGo          -> Just $ Lexer "go"
  LangGroovy      -> Just $ Lexer "groovy"
  LangHaml        -> Just $ Lexer "haml"
  LangHaskell     -> Just $ Lexer "haskell"
  LangHtml        -> Just $ Lexer "html"
  LangHttp        -> Just $ Lexer "http"
  LangIni         -> Just $ Lexer "ini"
  LangIrc         -> Just $ Lexer "irc"
  LangJava        -> Just $ Lexer "java"
  LangJs          -> Just $ Lexer "js"
  LangJson        -> Just $ Lexer "json"
  LangLua         -> Just $ Lexer "lua"
  LangMake        -> Just $ Lexer "make"
  LangMako        -> Just $ Lexer "mako"
  LangMason       -> Just $ Lexer "mason"
  LangMatlab      -> Just $ Lexer "matlab"
  LangModula2     -> Just $ Lexer "modula2"
  LangMonkey      -> Just $ Lexer "monkey"
  LangMysql       -> Just $ Lexer "mysql"
  LangNumpy       -> Just $ Lexer "numpy"
  LangObjectiveC  -> Just $ Lexer "objc"
  LangOcaml       -> Just $ Lexer "ocaml"
  LangPerl        -> Just $ Lexer "perl"
  LangPhp         -> Just $ Lexer "php"
  LangPostscript  -> Just $ Lexer "postscript"
  LangPowershell  -> Just $ Lexer "powershell"
  LangProlog      -> Just $ Lexer "prolog"
  LangProperties  -> Just $ Lexer "properties"
  LangPuppet      -> Just $ Lexer "puppet"
  LangPython      -> Just $ Lexer "python"
  LangR           -> Just $ Lexer "r"
  LangRb          -> Just $ Lexer "rb"
  LangRust        -> Just $ Lexer "rust"
  LangSass        -> Just $ Lexer "sass"
  LangScala       -> Just $ Lexer "scala"
  LangScheme      -> Just $ Lexer "scheme"
  LangScilab      -> Just $ Lexer "scilab"
  LangScss        -> Just $ Lexer "scss"
  LangSmalltalk   -> Just $ Lexer "smalltalk"
  LangSmarty      -> Just $ Lexer "smarty"
  LangSolidity    -> Just $ Lexer "solidity"
  LangSql         -> Just $ Lexer "sql"
  LangTcl         -> Just $ Lexer "tcl"
  LangTcsh        -> Just $ Lexer "tcsh"
  LangTex         -> Just $ Lexer "tex"
  LangVbNet       -> Just $ Lexer "vb.net"
  LangVim         -> Just $ Lexer "vim"
  LangXml         -> Just $ Lexer "xml"
  LangXquery      -> Just $ Lexer "xquery"
  LangXslt        -> Just $ Lexer "xslt"
  LangYaml        -> Just $ Lexer "yaml"
  _               -> Nothing
