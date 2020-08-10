{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
module Language where

import           Options.Applicative            ( ReadM
                                                , eitherReader)

import qualified Data.ByteString.Char8         as C

newtype Lexer =
  Lexer
    { unLexer :: C.ByteString
    }
  deriving newtype (Show, Read)

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
  = LangAbap
  | LangAbnf
  | LangAda
  | LangAdl
  | LangAgda
  | LangAheui
  | LangAhk
  | LangAlloy
  | LangAmpl
  | LangAntlr
  | LangAntlrAs
  | LangAntlrCpp
  | LangAntlrCsharp
  | LangAntlrJava
  | LangAntlrObjc
  | LangAntlrPerl
  | LangAntlrPython
  | LangAntlrRuby
  | LangApacheconf
  | LangApl
  | LangApplescript
  | LangArduino
  | LangAs
  | LangAs3
  | LangAspectj
  | LangAspxCs
  | LangAspxVb
  | LangAsy
  | LangAt
  | LangAugeas
  | LangAutoit
  | LangAwk
  | LangBasemake
  | LangBash
  | LangBat
  | LangBbcbasic
  | LangBbcode
  | LangBc
  | LangBefunge
  | LangBib
  | LangBlitzbasic
  | LangBlitzmax
  | LangBnf
  | LangBoa
  | LangBoo
  | LangBoogie
  | LangBrainfuck
  | LangBst
  | LangBugs
  | LangC
  | LangCa65
  | LangCadl
  | LangCamkes
  | LangCapdl
  | LangCapnp
  | LangCbmbas
  | LangCeylon
  | LangCfc
  | LangCfengine3
  | LangCfm
  | LangCfs
  | LangChai
  | LangChapel
  | LangCharmci
  | LangCheetah
  | LangCirru
  | LangClay
  | LangClean
  | LangClojure
  | LangClojurescript
  | LangCmake
  | LangCObjdump
  | LangCobol
  | LangCobolfree
  | LangCode
  | LangCoffeeScript
  | LangCommonLisp
  | LangComponentPascal
  | LangConsole
  | LangControl
  | LangCoq
  | LangCpp
  | LangCppObjdump
  | LangCpsa
  | LangCr
  | LangCrmsh
  | LangCroc
  | LangCryptol
  | LangCsharp
  | LangCsound
  | LangCsoundDocument
  | LangCsoundScore
  | LangCss
  | LangCssDjango
  | LangCssErb
  | LangCssGenshiText
  | LangCssLasso
  | LangCssMako
  | LangCssMozPreProc
  | LangCssMyghty
  | LangCssPhp
  | LangCssSmarty
  | LangCucumber
  | LangCuda
  | LangCypher
  | LangCython
  | LangD
  | LangDart
  | LangDasm16
  | LangDefault
  | LangDelphi
  | LangDg
  | LangDiff
  | LangDjango
  | LangDObjdump
  | LangDocker
  | LangDoscon
  | LangDpatch
  | LangDtd
  | LangDuel
  | LangDylan
  | LangDylanConsole
  | LangDylanLid
  | LangEarlGrey
  | LangEasytrieve
  | LangEbnf
  | LangEc
  | LangEcl
  | LangEiffel
  | LangElixir
  | LangElm
  | LangEmacs
  | LangEmail
  | LangErb
  | LangErl
  | LangErlang
  | LangEvoque
  | LangExtempore
  | LangEzhil
  | LangFactor
  | LangFan
  | LangFancy
  | LangFelix
  | LangFennel
  | LangFish
  | LangFlatline
  | LangFloscript
  | LangForth
  | LangFortran
  | LangFortranfixed
  | LangFoxpro
  | LangFreefem
  | LangFsharp
  | LangGap
  | LangGas
  | LangGenshi
  | LangGenshiText
  | LangGlsl
  | LangGnuplot
  | LangGo
  | LangGolo
  | LangGooddataCl
  | LangGosu
  | LangGroff
  | LangGroovy
  | LangGst
  | LangHaml
  | LangHandlebars
  | LangHaskell
  | LangHaxeml
  | LangHexdump
  | LangHlsl
  | LangHsail
  | LangHspec
  | LangHtml
  | LangHtmlCheetah
  | LangHtmlDjango
  | LangHtmlEvoque
  | LangHtmlGenshi
  | LangHtmlHandlebars
  | LangHtmlLasso
  | LangHtmlMako
  | LangHtmlMyghty
  | LangHtmlNg2
  | LangHtmlPhp
  | LangHtmlSmarty
  | LangHtmlTwig
  | LangHtmlVelocity
  | LangHttp
  | LangHx
  | LangHybris
  | LangHylang
  | LangI6t
  | LangIcon
  | LangIdl
  | LangIdris
  | LangIex
  | LangIgor
  | LangInform6
  | LangInform7
  | LangIni
  | LangIo
  | LangIoke
  | LangIrc
  | LangIsabelle
  | LangJ
  | LangJags
  | LangJasmin
  | LangJava
  | LangJavascriptMozPreProc
  | LangJcl
  | LangJlcon
  | LangJs
  | LangJsCheetah
  | LangJsDjango
  | LangJsErb
  | LangJsGenshiText
  | LangJsgf
  | LangJsLasso
  | LangJsMako
  | LangJsMyghty
  | LangJson
  | LangJsonld
  | LangJsonObject
  | LangJsp
  | LangJsPhp
  | LangJsSmarty
  | LangJulia
  | LangJuttle
  | LangKal
  | LangKconfig
  | LangKmsg
  | LangKoka
  | LangKotlin
  | LangLagda
  | LangLasso
  | LangLcry
  | LangLean
  | LangLess
  | LangLhs
  | LangLidr
  | LangLighty
  | LangLimbo
  | LangLiquid
  | LangLiveScript
  | LangLlvm
  | LangLlvmMir
  | LangLlvmMirBody
  | LangLogos
  | LangLogtalk
  | LangLsl
  | LangLua
  | LangMake
  | LangMako
  | LangMaql
  | LangMarkdown
  | LangMask
  | LangMason
  | LangMathematica
  | LangMatlab
  | LangMatlabsession
  | LangMime
  | LangMinid
  | LangModelica
  | LangModula2
  | LangMonkey
  | LangMonte
  | LangMoocode
  | LangMoon
  | LangMosel
  | LangMozHashPreProc
  | LangMozPercentPreProc
  | LangMql
  | LangMs
  | LangMscgen
  | LangMupad
  | LangMxml
  | LangMyghty
  | LangMysql
  | LangNasm
  | LangNcl
  | LangNemerle
  | LangNesc
  | LangNewlisp
  | LangNewspeak
  | LangNg2
  | LangNginx
  | LangNim
  | LangNit
  | LangNixos
  | LangNotmuch
  | LangNsis
  | LangNumpy
  | LangNusmv
  | LangObjdump
  | LangObjdumpNasm
  | LangObjectiveC
  | LangObjectiveCpp
  | LangObjectiveJ
  | LangOcaml
  | LangOctave
  | LangOdin
  | LangOoc
  | LangOpa
  | LangOpenedge
  | LangPacmanconf
  | LangPan
  | LangParasail
  | LangPawn
  | LangPeg
  | LangPerl
  | LangPerl6
  | LangPhp
  | LangPig
  | LangPike
  | LangPkgconfig
  | LangPlpgsql
  | LangPony
  | LangPostgresql
  | LangPostscript
  | LangPot
  | LangPov
  | LangPowershell
  | LangPraat
  | LangProlog
  | LangProperties
  | LangProtobuf
  | LangPs1con
  | LangPsql
  | LangPug
  | LangPuppet
  | LangPy2tb
  | LangPycon
  | LangPypylog
  | LangPytb
  | LangPython
  | LangPython2
  | LangQbasic
  | LangQml
  | LangQvto
  | LangR
  | LangRacket
  | LangRagel
  | LangRagelC
  | LangRagelCpp
  | LangRagelD
  | LangRagelEm
  | LangRagelJava
  | LangRagelObjc
  | LangRagelRuby
  | LangRaw
  | LangRb
  | LangRbcon
  | LangRconsole
  | LangRd
  | LangReason
  | LangRebol
  | LangRed
  | LangRedcode
  | LangRegistry
  | LangResource
  | LangRexx
  | LangRhtml
  | LangRide
  | LangRnc
  | LangRoboconfGraph
  | LangRoboconfInstances
  | LangRobotFramework
  | LangRql
  | LangRsl
  | LangRst
  | LangRts
  | LangRust
  | LangSarl
  | LangSas
  | LangSass
  | LangSc
  | LangScala
  | LangScaml
  | LangScdoc
  | LangScheme
  | LangScilab
  | LangScss
  | LangSgf
  | LangShen
  | LangShexc
  | LangSieve
  | LangSilver
  | LangSlash
  | LangSlim
  | LangSlurm
  | LangSmali
  | LangSmalltalk
  | LangSmarty
  | LangSml
  | LangSnobol
  | LangSnowball
  | LangSolidity
  | LangSourceslist
  | LangSp
  | LangSparql
  | LangSpec
  | LangSplus
  | LangSql
  | LangSqlite3
  | LangSquidconf
  | LangSsp
  | LangStan
  | LangStata
  | LangSwift
  | LangSwig
  | LangSystemverilog
  | LangTads3
  | LangTap
  | LangTasm
  | LangTcl
  | LangTcsh
  | LangTcshcon
  | LangTea
  | LangTermcap
  | LangTerminfo
  | LangTerraform
  | LangTex
  | LangText
  | LangThrift
  | LangTodotxt
  | LangToml
  | LangTracWiki
  | LangTreetop
  | LangTs
  | LangTsql
  | LangTtl
  | LangTurtle
  | LangTwig
  | LangTyposcript
  | LangTyposcriptcssdata
  | LangTyposcripthtmldata
  | LangUcode
  | LangUnicon
  | LangUrbiscript
  | LangUsd
  | LangVala
  | LangVbNet
  | LangVbscript
  | LangVcl
  | LangVclsnippets
  | LangVctreestatus
  | LangVelocity
  | LangVerilog
  | LangVgl
  | LangVhdl
  | LangVim
  | LangWdiff
  | LangWebidl
  | LangWhiley
  | LangX10
  | LangXml
  | LangXmlCheetah
  | LangXmlDjango
  | LangXmlErb
  | LangXmlEvoque
  | LangXmlLasso
  | LangXmlMako
  | LangXmlMyghty
  | LangXmlPhp
  | LangXmlSmarty
  | LangXmlVelocity
  | LangXorgConf
  | LangXquery
  | LangXslt
  | LangXtend
  | LangXulMozPreProc
  | LangYaml
  | LangYamlJinja
  | LangZeek
  | LangZephir
  | LangZig
  deriving (Enum, Bounded, Show, Read, Eq)

parseLanguages :: ReadM Language
parseLanguages = eitherReader parseLanguage
-- parseLanguages :: ReadM Language
-- parseLanguages = eitherReader $ \case
parseLanguage :: String -> Either String Language
parseLanguage = \case
  "abap"                  -> Right LangAbap
  "abnf"                  -> Right LangAbnf
  "ada"                   -> Right LangAda
  "adl"                   -> Right LangAdl
  "agda"                  -> Right LangAgda
  "aheui"                 -> Right LangAheui
  "ahk"                   -> Right LangAhk
  "alloy"                 -> Right LangAlloy
  "ampl"                  -> Right LangAmpl
  "antlr-as"              -> Right LangAntlrAs
  "antlr-cpp"             -> Right LangAntlrCpp
  "antlr-csharp"          -> Right LangAntlrCsharp
  "antlr-java"            -> Right LangAntlrJava
  "antlr-objc"            -> Right LangAntlrObjc
  "antlr-perl"            -> Right LangAntlrPerl
  "antlr-python"          -> Right LangAntlrPython
  "antlr"                 -> Right LangAntlr
  "antlr-ruby"            -> Right LangAntlrRuby
  "apacheconf"            -> Right LangApacheconf
  "apl"                   -> Right LangApl
  "applescript"           -> Right LangApplescript
  "arduino"               -> Right LangArduino
  "as3"                   -> Right LangAs3
  "aspectj"               -> Right LangAspectj
  "aspx-cs"               -> Right LangAspxCs
  "aspx-vb"               -> Right LangAspxVb
  "as"                    -> Right LangAs
  "asy"                   -> Right LangAsy
  "at"                    -> Right LangAt
  "augeas"                -> Right LangAugeas
  "autoit"                -> Right LangAutoit
  "awk"                   -> Right LangAwk
  "basemake"              -> Right LangBasemake
  "bash"                  -> Right LangBash
  "bat"                   -> Right LangBat
  "bbcbasic"              -> Right LangBbcbasic
  "bbcode"                -> Right LangBbcode
  "bc"                    -> Right LangBc
  "befunge"               -> Right LangBefunge
  "bib"                   -> Right LangBib
  "blitzbasic"            -> Right LangBlitzbasic
  "blitzmax"              -> Right LangBlitzmax
  "bnf"                   -> Right LangBnf
  "boa"                   -> Right LangBoa
  "boogie"                -> Right LangBoogie
  "boo"                   -> Right LangBoo
  "brainfuck"             -> Right LangBrainfuck
  "bst"                   -> Right LangBst
  "bugs"                  -> Right LangBugs
  "ca65"                  -> Right LangCa65
  "cadl"                  -> Right LangCadl
  "camkes"                -> Right LangCamkes
  "capdl"                 -> Right LangCapdl
  "capnp"                 -> Right LangCapnp
  "cbmbas"                -> Right LangCbmbas
  "ceylon"                -> Right LangCeylon
  "cfc"                   -> Right LangCfc
  "cfengine3"             -> Right LangCfengine3
  "cfm"                   -> Right LangCfm
  "cfs"                   -> Right LangCfs
  "chai"                  -> Right LangChai
  "chapel"                -> Right LangChapel
  "charmci"               -> Right LangCharmci
  "cheetah"               -> Right LangCheetah
  "cirru"                 -> Right LangCirru
  "clay"                  -> Right LangClay
  "clean"                 -> Right LangClean
  "clojure"               -> Right LangClojure
  "clojurescript"         -> Right LangClojurescript
  "cmake"                 -> Right LangCmake
  "c-objdump"             -> Right LangCObjdump
  "cobolfree"             -> Right LangCobolfree
  "cobol"                 -> Right LangCobol
  "code"                  -> Right LangCode
  "_code"                 -> Right LangCode
  "coffee-script"         -> Right LangCoffeeScript
  "common-lisp"           -> Right LangCommonLisp
  "componentpascal"       -> Right LangComponentPascal
  "console"               -> Right LangConsole
  "control"               -> Right LangControl
  "coq"                   -> Right LangCoq
  "cpp-objdump"           -> Right LangCppObjdump
  "cpp"                   -> Right LangCpp
  "cpsa"                  -> Right LangCpsa
  "c"                     -> Right LangC
  "crmsh"                 -> Right LangCrmsh
  "croc"                  -> Right LangCroc
  "cr"                    -> Right LangCr
  "cryptol"               -> Right LangCryptol
  "csharp"                -> Right LangCsharp
  "csound-document"       -> Right LangCsoundDocument
  "csound"                -> Right LangCsound
  "csound-score"          -> Right LangCsoundScore
  "css+django"            -> Right LangCssDjango
  "css+erb"               -> Right LangCssErb
  "css+genshitext"        -> Right LangCssGenshiText
  "css+lasso"             -> Right LangCssLasso
  "css+mako"              -> Right LangCssMako
  "css+mozpreproc"        -> Right LangCssMozPreProc
  "css+myghty"            -> Right LangCssMyghty
  "css+php"               -> Right LangCssPhp
  "css"                   -> Right LangCss
  "css+smarty"            -> Right LangCssSmarty
  "cucumber"              -> Right LangCucumber
  "cuda"                  -> Right LangCuda
  "cypher"                -> Right LangCypher
  "cython"                -> Right LangCython
  "dart"                  -> Right LangDart
  "dasm16"                -> Right LangDasm16
  "delphi"                -> Right LangDelphi
  "dg"                    -> Right LangDg
  "diff"                  -> Right LangDiff
  "django"                -> Right LangDjango
  "d-objdump"             -> Right LangDObjdump
  "docker"                -> Right LangDocker
  "doscon"                -> Right LangDoscon
  "dpatch"                -> Right LangDpatch
  "d"                     -> Right LangD
  "dtd"                   -> Right LangDtd
  "duel"                  -> Right LangDuel
  "dylan-console"         -> Right LangDylanConsole
  "dylan-lid"             -> Right LangDylanLid
  "dylan"                 -> Right LangDylan
  "earl-grey"             -> Right LangEarlGrey
  "easytrieve"            -> Right LangEasytrieve
  "ebnf"                  -> Right LangEbnf
  "ecl"                   -> Right LangEcl
  "ec"                    -> Right LangEc
  "eiffel"                -> Right LangEiffel
  "elixir"                -> Right LangElixir
  "elm"                   -> Right LangElm
  "emacs"                 -> Right LangEmacs
  "email"                 -> Right LangEmail
  "erb"                   -> Right LangErb
  "erlang"                -> Right LangErlang
  "erl"                   -> Right LangErl
  "evoque"                -> Right LangEvoque
  "extempore"             -> Right LangExtempore
  "ezhil"                 -> Right LangEzhil
  "factor"                -> Right LangFactor
  "fancy"                 -> Right LangFancy
  "fan"                   -> Right LangFan
  "felix"                 -> Right LangFelix
  "fennel"                -> Right LangFennel
  "fish"                  -> Right LangFish
  "flatline"              -> Right LangFlatline
  "floscript"             -> Right LangFloscript
  "forth"                 -> Right LangForth
  "fortranfixed"          -> Right LangFortranfixed
  "fortran"               -> Right LangFortran
  "foxpro"                -> Right LangFoxpro
  "freefem"               -> Right LangFreefem
  "fsharp"                -> Right LangFsharp
  "gap"                   -> Right LangGap
  "gas"                   -> Right LangGas
  "genshi"                -> Right LangGenshi
  "genshitext"            -> Right LangGenshiText
  "glsl"                  -> Right LangGlsl
  "gnuplot"               -> Right LangGnuplot
  "golo"                  -> Right LangGolo
  "gooddata-cl"           -> Right LangGooddataCl
  "go"                    -> Right LangGo
  "gosu"                  -> Right LangGosu
  "groff"                 -> Right LangGroff
  "groovy"                -> Right LangGroovy
  "gst"                   -> Right LangGst
  "haml"                  -> Right LangHaml
  "handlebars"            -> Right LangHandlebars
  "haskell"               -> Right LangHaskell
  "haxeml"                -> Right LangHaxeml
  "hexdump"               -> Right LangHexdump
  "hlsl"                  -> Right LangHlsl
  "hsail"                 -> Right LangHsail
  "hspec"                 -> Right LangHspec
  "html+cheetah"          -> Right LangHtmlCheetah
  "html+django"           -> Right LangHtmlDjango
  "html+evoque"           -> Right LangHtmlEvoque
  "html+genshi"           -> Right LangHtmlGenshi
  "html+handlebars"       -> Right LangHtmlHandlebars
  "html+lasso"            -> Right LangHtmlLasso
  "html+mako"             -> Right LangHtmlMako
  "html+myghty"           -> Right LangHtmlMyghty
  "html+ng2"              -> Right LangHtmlNg2
  "html+php"              -> Right LangHtmlPhp
  "html"                  -> Right LangHtml
  "html+smarty"           -> Right LangHtmlSmarty
  "html+twig"             -> Right LangHtmlTwig
  "html+velocity"         -> Right LangHtmlVelocity
  "http"                  -> Right LangHttp
  "hx"                    -> Right LangHx
  "hybris"                -> Right LangHybris
  "hylang"                -> Right LangHylang
  "i6t"                   -> Right LangI6t
  "icon"                  -> Right LangIcon
  "idl"                   -> Right LangIdl
  "idris"                 -> Right LangIdris
  "iex"                   -> Right LangIex
  "igor"                  -> Right LangIgor
  "inform6"               -> Right LangInform6
  "inform7"               -> Right LangInform7
  "ini"                   -> Right LangIni
  "ioke"                  -> Right LangIoke
  "io"                    -> Right LangIo
  "irc"                   -> Right LangIrc
  "isabelle"              -> Right LangIsabelle
  "jags"                  -> Right LangJags
  "jasmin"                -> Right LangJasmin
  "java"                  -> Right LangJava
  "javascript+mozpreproc" -> Right LangJavascriptMozPreProc
  "jcl"                   -> Right LangJcl
  "jlcon"                 -> Right LangJlcon
  "j"                     -> Right LangJ
  "js+cheetah"            -> Right LangJsCheetah
  "js+django"             -> Right LangJsDjango
  "js+erb"                -> Right LangJsErb
  "js+genshitext"         -> Right LangJsGenshiText
  "jsgf"                  -> Right LangJsgf
  "js+lasso"              -> Right LangJsLasso
  "js+mako"               -> Right LangJsMako
  "js+myghty"             -> Right LangJsMyghty
  "jsonld"                -> Right LangJsonld
  "json-object"           -> Right LangJsonObject
  "json"                  -> Right LangJson
  "js+php"                -> Right LangJsPhp
  "jsp"                   -> Right LangJsp
  "js"                    -> Right LangJs
  "js+smarty"             -> Right LangJsSmarty
  "julia"                 -> Right LangJulia
  "juttle"                -> Right LangJuttle
  "kal"                   -> Right LangKal
  "kconfig"               -> Right LangKconfig
  "kmsg"                  -> Right LangKmsg
  "koka"                  -> Right LangKoka
  "kotlin"                -> Right LangKotlin
  "lagda"                 -> Right LangLagda
  "lasso"                 -> Right LangLasso
  "lcry"                  -> Right LangLcry
  "lean"                  -> Right LangLean
  "less"                  -> Right LangLess
  "lhs"                   -> Right LangLhs
  "lidr"                  -> Right LangLidr
  "lighty"                -> Right LangLighty
  "limbo"                 -> Right LangLimbo
  "liquid"                -> Right LangLiquid
  "live-script"           -> Right LangLiveScript
  "llvm-mir-body"         -> Right LangLlvmMirBody
  "llvm-mir"              -> Right LangLlvmMir
  "llvm"                  -> Right LangLlvm
  "logos"                 -> Right LangLogos
  "logtalk"               -> Right LangLogtalk
  "lsl"                   -> Right LangLsl
  "lua"                   -> Right LangLua
  "make"                  -> Right LangMake
  "mako"                  -> Right LangMako
  "maql"                  -> Right LangMaql
  "markdown"              -> Right LangMarkdown
  "_markdown"             -> Right LangMarkdown
  "mask"                  -> Right LangMask
  "mason"                 -> Right LangMason
  "mathematica"           -> Right LangMathematica
  "matlab"                -> Right LangMatlab
  "matlabsession"         -> Right LangMatlabsession
  "md"                    -> Right LangMarkdown
  "mime"                  -> Right LangMime
  "minid"                 -> Right LangMinid
  "modelica"              -> Right LangModelica
  "modula2"               -> Right LangModula2
  "monkey"                -> Right LangMonkey
  "monte"                 -> Right LangMonte
  "moocode"               -> Right LangMoocode
  "moon"                  -> Right LangMoon
  "mosel"                 -> Right LangMosel
  "mozhashpreproc"        -> Right LangMozHashPreProc
  "mozpercentpreproc"     -> Right LangMozPercentPreProc
  "mql"                   -> Right LangMql
  "mscgen"                -> Right LangMscgen
  "ms"                    -> Right LangMs
  "mupad"                 -> Right LangMupad
  "mxml"                  -> Right LangMxml
  "myghty"                -> Right LangMyghty
  "mysql"                 -> Right LangMysql
  "nasm"                  -> Right LangNasm
  "ncl"                   -> Right LangNcl
  "nemerle"               -> Right LangNemerle
  "nesc"                  -> Right LangNesc
  "newlisp"               -> Right LangNewlisp
  "newspeak"              -> Right LangNewspeak
  "ng2"                   -> Right LangNg2
  "nginx"                 -> Right LangNginx
  "nim"                   -> Right LangNim
  "nit"                   -> Right LangNit
  "nixos"                 -> Right LangNixos
  "notmuch"               -> Right LangNotmuch
  "nsis"                  -> Right LangNsis
  "numpy"                 -> Right LangNumpy
  "nusmv"                 -> Right LangNusmv
  "objdump-nasm"          -> Right LangObjdumpNasm
  "objdump"               -> Right LangObjdump
  "objective-c"           -> Right LangObjectiveC
  "objective-c++"         -> Right LangObjectiveCpp
  "objective-j"           -> Right LangObjectiveJ
  "ocaml"                 -> Right LangOcaml
  "octave"                -> Right LangOctave
  "odin"                  -> Right LangOdin
  "ooc"                   -> Right LangOoc
  "opa"                   -> Right LangOpa
  "openedge"              -> Right LangOpenedge
  "pacmanconf"            -> Right LangPacmanconf
  "pan"                   -> Right LangPan
  "parasail"              -> Right LangParasail
  "pawn"                  -> Right LangPawn
  "peg"                   -> Right LangPeg
  "perl6"                 -> Right LangPerl6
  "perl"                  -> Right LangPerl
  "php"                   -> Right LangPhp
  "pig"                   -> Right LangPig
  "pike"                  -> Right LangPike
  "pkgconfig"             -> Right LangPkgconfig
  "plpgsql"               -> Right LangPlpgsql
  "pony"                  -> Right LangPony
  "postgresql"            -> Right LangPostgresql
  "postscript"            -> Right LangPostscript
  "pot"                   -> Right LangPot
  "pov"                   -> Right LangPov
  "powershell"            -> Right LangPowershell
  "praat"                 -> Right LangPraat
  "prolog"                -> Right LangProlog
  "properties"            -> Right LangProperties
  "protobuf"              -> Right LangProtobuf
  "ps1con"                -> Right LangPs1con
  "psql"                  -> Right LangPsql
  "pug"                   -> Right LangPug
  "puppet"                -> Right LangPuppet
  "py2tb"                 -> Right LangPy2tb
  "pycon"                 -> Right LangPycon
  "pypylog"               -> Right LangPypylog
  "pytb"                  -> Right LangPytb
  "python2"               -> Right LangPython2
  "python"                -> Right LangPython
  "qbasic"                -> Right LangQbasic
  "qml"                   -> Right LangQml
  "qvto"                  -> Right LangQvto
  "racket"                -> Right LangRacket
  "ragel-cpp"             -> Right LangRagelCpp
  "ragel-c"               -> Right LangRagelC
  "ragel-d"               -> Right LangRagelD
  "ragel-em"              -> Right LangRagelEm
  "ragel-java"            -> Right LangRagelJava
  "ragel-objc"            -> Right LangRagelObjc
  "ragel"                 -> Right LangRagel
  "ragel-ruby"            -> Right LangRagelRuby
  "raw"                   -> Right LangRaw
  "rbcon"                 -> Right LangRbcon
  "rb"                    -> Right LangRb
  "rconsole"              -> Right LangRconsole
  "rd"                    -> Right LangRd
  "reason"                -> Right LangReason
  "rebol"                 -> Right LangRebol
  "redcode"               -> Right LangRedcode
  "red"                   -> Right LangRed
  "registry"              -> Right LangRegistry
  "resource"              -> Right LangResource
  "rexx"                  -> Right LangRexx
  "rhtml"                 -> Right LangRhtml
  "ride"                  -> Right LangRide
  "rnc"                   -> Right LangRnc
  "roboconf-graph"        -> Right LangRoboconfGraph
  "roboconf-instances"    -> Right LangRoboconfInstances
  "robotframework"        -> Right LangRobotFramework
  "rql"                   -> Right LangRql
  "r"                     -> Right LangR
  "rsl"                   -> Right LangRsl
  "rst"                   -> Right LangRst
  "_rst"                  -> Right LangRst
  "rts"                   -> Right LangRts
  "rust"                  -> Right LangRust
  "sarl"                  -> Right LangSarl
  "sas"                   -> Right LangSas
  "sass"                  -> Right LangSass
  "scala"                 -> Right LangScala
  "scaml"                 -> Right LangScaml
  "scdoc"                 -> Right LangScdoc
  "scheme"                -> Right LangScheme
  "scilab"                -> Right LangScilab
  "sc"                    -> Right LangSc
  "scss"                  -> Right LangScss
  "sgf"                   -> Right LangSgf
  "shen"                  -> Right LangShen
  "shexc"                 -> Right LangShexc
  "sieve"                 -> Right LangSieve
  "silver"                -> Right LangSilver
  "slash"                 -> Right LangSlash
  "slim"                  -> Right LangSlim
  "slurm"                 -> Right LangSlurm
  "smali"                 -> Right LangSmali
  "smalltalk"             -> Right LangSmalltalk
  "smarty"                -> Right LangSmarty
  "sml"                   -> Right LangSml
  "snobol"                -> Right LangSnobol
  "snowball"              -> Right LangSnowball
  "solidity"              -> Right LangSolidity
  "sourceslist"           -> Right LangSourceslist
  "sparql"                -> Right LangSparql
  "spec"                  -> Right LangSpec
  "splus"                 -> Right LangSplus
  "sp"                    -> Right LangSp
  "sqlite3"               -> Right LangSqlite3
  "sql"                   -> Right LangSql
  "squidconf"             -> Right LangSquidconf
  "ssp"                   -> Right LangSsp
  "stan"                  -> Right LangStan
  "stata"                 -> Right LangStata
  "swift"                 -> Right LangSwift
  "swig"                  -> Right LangSwig
  "systemverilog"         -> Right LangSystemverilog
  "tads3"                 -> Right LangTads3
  "tap"                   -> Right LangTap
  "tasm"                  -> Right LangTasm
  "tcl"                   -> Right LangTcl
  "tcshcon"               -> Right LangTcshcon
  "tcsh"                  -> Right LangTcsh
  "tea"                   -> Right LangTea
  "termcap"               -> Right LangTermcap
  "terminfo"              -> Right LangTerminfo
  "terraform"             -> Right LangTerraform
  "tex"                   -> Right LangTex
  "text"                  -> Right LangText
  "_text"                 -> Right LangText
  "thrift"                -> Right LangThrift
  "todotxt"               -> Right LangTodotxt
  "toml"                  -> Right LangToml
  "trac-wiki"             -> Right LangTracWiki
  "treetop"               -> Right LangTreetop
  "tsql"                  -> Right LangTsql
  "ts"                    -> Right LangTs
  "ttl"                   -> Right LangTtl
  "turtle"                -> Right LangTurtle
  "twig"                  -> Right LangTwig
  "typoscriptcssdata"     -> Right LangTyposcriptcssdata
  "typoscripthtmldata"    -> Right LangTyposcripthtmldata
  "typoscript"            -> Right LangTyposcript
  "ucode"                 -> Right LangUcode
  "unicon"                -> Right LangUnicon
  "urbiscript"            -> Right LangUrbiscript
  "usd"                   -> Right LangUsd
  "vala"                  -> Right LangVala
  "vb.net"                -> Right LangVbNet
  "vbscript"              -> Right LangVbscript
  "vcl"                   -> Right LangVcl
  "vclsnippets"           -> Right LangVclsnippets
  "vctreestatus"          -> Right LangVctreestatus
  "velocity"              -> Right LangVelocity
  "verilog"               -> Right LangVerilog
  "vgl"                   -> Right LangVgl
  "vhdl"                  -> Right LangVhdl
  "vim"                   -> Right LangVim
  "wdiff"                 -> Right LangWdiff
  "webidl"                -> Right LangWebidl
  "whiley"                -> Right LangWhiley
  "x10"                   -> Right LangX10
  "xml+cheetah"           -> Right LangXmlCheetah
  "xml+django"            -> Right LangXmlDjango
  "xml+erb"               -> Right LangXmlErb
  "xml+evoque"            -> Right LangXmlEvoque
  "xml+lasso"             -> Right LangXmlLasso
  "xml+mako"              -> Right LangXmlMako
  "xml+myghty"            -> Right LangXmlMyghty
  "xml+php"               -> Right LangXmlPhp
  "xml"                   -> Right LangXml
  "xml+smarty"            -> Right LangXmlSmarty
  "xml+velocity"          -> Right LangXmlVelocity
  "xorg.conf"             -> Right LangXorgConf
  "xquery"                -> Right LangXquery
  "xslt"                  -> Right LangXslt
  "xtend"                 -> Right LangXtend
  "xul+mozpreproc"        -> Right LangXulMozPreProc
  "yaml+jinja"            -> Right LangYamlJinja
  "yaml"                  -> Right LangYaml
  "zeek"                  -> Right LangZeek
  "zephir"                -> Right LangZephir
  "zig"                   -> Right LangZig
  _                       -> Left "Invalid language"

langToLexer :: Language -> Lexer
langToLexer = Lexer . \case
  LangAbap                 -> "abap"
  LangAbnf                 -> "abnf"
  LangAda                  -> "ada"
  LangAdl                  -> "adl"
  LangAgda                 -> "agda"
  LangAheui                -> "aheui"
  LangAhk                  -> "ahk"
  LangAlloy                -> "alloy"
  LangAmpl                 -> "ampl"
  LangAntlrAs              -> "antlr-as"
  LangAntlrCpp             -> "antlr-cpp"
  LangAntlrCsharp          -> "antlr-csharp"
  LangAntlrJava            -> "antlr-java"
  LangAntlrObjc            -> "antlr-objc"
  LangAntlrPerl            -> "antlr-perl"
  LangAntlrPython          -> "antlr-python"
  LangAntlr                -> "antlr"
  LangAntlrRuby            -> "antlr-ruby"
  LangApacheconf           -> "apacheconf"
  LangApl                  -> "apl"
  LangApplescript          -> "applescript"
  LangArduino              -> "arduino"
  LangAs3                  -> "as3"
  LangAspectj              -> "aspectj"
  LangAspxCs               -> "aspx-cs"
  LangAspxVb               -> "aspx-vb"
  LangAs                   -> "as"
  LangAsy                  -> "asy"
  LangAt                   -> "at"
  LangAugeas               -> "augeas"
  LangAutoit               -> "autoit"
  LangAwk                  -> "awk"
  LangBasemake             -> "basemake"
  LangBash                 -> "bash"
  LangBat                  -> "bat"
  LangBbcbasic             -> "bbcbasic"
  LangBbcode               -> "bbcode"
  LangBc                   -> "bc"
  LangBefunge              -> "befunge"
  LangBib                  -> "bib"
  LangBlitzbasic           -> "blitzbasic"
  LangBlitzmax             -> "blitzmax"
  LangBnf                  -> "bnf"
  LangBoa                  -> "boa"
  LangBoogie               -> "boogie"
  LangBoo                  -> "boo"
  LangBrainfuck            -> "brainfuck"
  LangBst                  -> "bst"
  LangBugs                 -> "bugs"
  LangCa65                 -> "ca65"
  LangCadl                 -> "cadl"
  LangCamkes               -> "camkes"
  LangCapdl                -> "capdl"
  LangCapnp                -> "capnp"
  LangCbmbas               -> "cbmbas"
  LangCeylon               -> "ceylon"
  LangCfc                  -> "cfc"
  LangCfengine3            -> "cfengine3"
  LangCfm                  -> "cfm"
  LangCfs                  -> "cfs"
  LangChai                 -> "chai"
  LangChapel               -> "chapel"
  LangCharmci              -> "charmci"
  LangCheetah              -> "cheetah"
  LangCirru                -> "cirru"
  LangClay                 -> "clay"
  LangClean                -> "clean"
  LangClojure              -> "clojure"
  LangClojurescript        -> "clojurescript"
  LangCmake                -> "cmake"
  LangCObjdump             -> "c-objdump"
  LangCobolfree            -> "cobolfree"
  LangCobol                -> "cobol"
  LangCode                 -> "code"
  LangCoffeeScript         -> "coffee-script"
  LangCommonLisp           -> "common-lisp"
  LangComponentPascal      -> "componentpascal"
  LangConsole              -> "console"
  LangControl              -> "control"
  LangCoq                  -> "coq"
  LangCppObjdump           -> "cpp-objdump"
  LangCpp                  -> "cpp"
  LangCpsa                 -> "cpsa"
  LangC                    -> "c"
  LangCrmsh                -> "crmsh"
  LangCroc                 -> "croc"
  LangCr                   -> "cr"
  LangCryptol              -> "cryptol"
  LangCsharp               -> "csharp"
  LangCsoundDocument       -> "csound-document"
  LangCsound               -> "csound"
  LangCsoundScore          -> "csound-score"
  LangCssDjango            -> "css+django"
  LangCssErb               -> "css+erb"
  LangCssGenshiText        -> "css+genshitext"
  LangCssLasso             -> "css+lasso"
  LangCssMako              -> "css+mako"
  LangCssMozPreProc        -> "css+mozpreproc"
  LangCssMyghty            -> "css+myghty"
  LangCssPhp               -> "css+php"
  LangCss                  -> "css"
  LangCssSmarty            -> "css+smarty"
  LangCucumber             -> "cucumber"
  LangCuda                 -> "cuda"
  LangCypher               -> "cypher"
  LangCython               -> "cython"
  LangDart                 -> "dart"
  LangDasm16               -> "dasm16"
  LangDelphi               -> "delphi"
  LangDg                   -> "dg"
  LangDiff                 -> "diff"
  LangDjango               -> "django"
  LangDObjdump             -> "d-objdump"
  LangDocker               -> "docker"
  LangDoscon               -> "doscon"
  LangDpatch               -> "dpatch"
  LangD                    -> "d"
  LangDtd                  -> "dtd"
  LangDuel                 -> "duel"
  LangDylanConsole         -> "dylan-console"
  LangDylanLid             -> "dylan-lid"
  LangDylan                -> "dylan"
  LangEarlGrey             -> "earl-grey"
  LangEasytrieve           -> "easytrieve"
  LangEbnf                 -> "ebnf"
  LangEcl                  -> "ecl"
  LangEc                   -> "ec"
  LangEiffel               -> "eiffel"
  LangElixir               -> "elixir"
  LangElm                  -> "elm"
  LangEmacs                -> "emacs"
  LangEmail                -> "email"
  LangErb                  -> "erb"
  LangErlang               -> "erlang"
  LangErl                  -> "erl"
  LangEvoque               -> "evoque"
  LangExtempore            -> "extempore"
  LangEzhil                -> "ezhil"
  LangFactor               -> "factor"
  LangFancy                -> "fancy"
  LangFan                  -> "fan"
  LangFelix                -> "felix"
  LangFennel               -> "fennel"
  LangFish                 -> "fish"
  LangFlatline             -> "flatline"
  LangFloscript            -> "floscript"
  LangForth                -> "forth"
  LangFortranfixed         -> "fortranfixed"
  LangFortran              -> "fortran"
  LangFoxpro               -> "foxpro"
  LangFreefem              -> "freefem"
  LangFsharp               -> "fsharp"
  LangGap                  -> "gap"
  LangGas                  -> "gas"
  LangGenshi               -> "genshi"
  LangGenshiText           -> "genshitext"
  LangGlsl                 -> "glsl"
  LangGnuplot              -> "gnuplot"
  LangGolo                 -> "golo"
  LangGooddataCl           -> "gooddata-cl"
  LangGo                   -> "go"
  LangGosu                 -> "gosu"
  LangGroff                -> "groff"
  LangGroovy               -> "groovy"
  LangGst                  -> "gst"
  LangHaml                 -> "haml"
  LangHandlebars           -> "handlebars"
  LangHaskell              -> "haskell"
  LangHaxeml               -> "haxeml"
  LangHexdump              -> "hexdump"
  LangHlsl                 -> "hlsl"
  LangHsail                -> "hsail"
  LangHspec                -> "hspec"
  LangHtmlCheetah          -> "html+cheetah"
  LangHtmlDjango           -> "html+django"
  LangHtmlEvoque           -> "html+evoque"
  LangHtmlGenshi           -> "html+genshi"
  LangHtmlHandlebars       -> "html+handlebars"
  LangHtmlLasso            -> "html+lasso"
  LangHtmlMako             -> "html+mako"
  LangHtmlMyghty           -> "html+myghty"
  LangHtmlNg2              -> "html+ng2"
  LangHtmlPhp              -> "html+php"
  LangHtml                 -> "html"
  LangHtmlSmarty           -> "html+smarty"
  LangHtmlTwig             -> "html+twig"
  LangHtmlVelocity         -> "html+velocity"
  LangHttp                 -> "http"
  LangHx                   -> "hx"
  LangHybris               -> "hybris"
  LangHylang               -> "hylang"
  LangI6t                  -> "i6t"
  LangIcon                 -> "icon"
  LangIdl                  -> "idl"
  LangIdris                -> "idris"
  LangIex                  -> "iex"
  LangIgor                 -> "igor"
  LangInform6              -> "inform6"
  LangInform7              -> "inform7"
  LangIni                  -> "ini"
  LangIoke                 -> "ioke"
  LangIo                   -> "io"
  LangIrc                  -> "irc"
  LangIsabelle             -> "isabelle"
  LangJags                 -> "jags"
  LangJasmin               -> "jasmin"
  LangJava                 -> "java"
  LangJavascriptMozPreProc -> "javascript+mozpreproc"
  LangJcl                  -> "jcl"
  LangJlcon                -> "jlcon"
  LangJ                    -> "j"
  LangJsCheetah            -> "js+cheetah"
  LangJsDjango             -> "js+django"
  LangJsErb                -> "js+erb"
  LangJsGenshiText         -> "js+genshitext"
  LangJsgf                 -> "jsgf"
  LangJsLasso              -> "js+lasso"
  LangJsMako               -> "js+mako"
  LangJsMyghty             -> "js+myghty"
  LangJsonld               -> "jsonld"
  LangJsonObject           -> "json-object"
  LangJson                 -> "json"
  LangJsPhp                -> "js+php"
  LangJsp                  -> "jsp"
  LangJs                   -> "js"
  LangJsSmarty             -> "js+smarty"
  LangJulia                -> "julia"
  LangJuttle               -> "juttle"
  LangKal                  -> "kal"
  LangKconfig              -> "kconfig"
  LangKmsg                 -> "kmsg"
  LangKoka                 -> "koka"
  LangKotlin               -> "kotlin"
  LangLagda                -> "lagda"
  LangLasso                -> "lasso"
  LangLcry                 -> "lcry"
  LangLean                 -> "lean"
  LangLess                 -> "less"
  LangLhs                  -> "lhs"
  LangLidr                 -> "lidr"
  LangLighty               -> "lighty"
  LangLimbo                -> "limbo"
  LangLiquid               -> "liquid"
  LangLiveScript           -> "live-script"
  LangLlvmMirBody          -> "llvm-mir-body"
  LangLlvmMir              -> "llvm-mir"
  LangLlvm                 -> "llvm"
  LangLogos                -> "logos"
  LangLogtalk              -> "logtalk"
  LangLsl                  -> "lsl"
  LangLua                  -> "lua"
  LangMake                 -> "make"
  LangMako                 -> "mako"
  LangMaql                 -> "maql"
  LangMask                 -> "mask"
  LangMason                -> "mason"
  LangMathematica          -> "mathematica"
  LangMatlab               -> "matlab"
  LangMatlabsession        -> "matlabsession"
  LangMarkdown             -> "markdown"
  LangMime                 -> "mime"
  LangMinid                -> "minid"
  LangModelica             -> "modelica"
  LangModula2              -> "modula2"
  LangMonkey               -> "monkey"
  LangMonte                -> "monte"
  LangMoocode              -> "moocode"
  LangMoon                 -> "moon"
  LangMosel                -> "mosel"
  LangMozHashPreProc       -> "mozhashpreproc"
  LangMozPercentPreProc    -> "mozpercentpreproc"
  LangMql                  -> "mql"
  LangMscgen               -> "mscgen"
  LangMs                   -> "ms"
  LangMupad                -> "mupad"
  LangMxml                 -> "mxml"
  LangMyghty               -> "myghty"
  LangMysql                -> "mysql"
  LangNasm                 -> "nasm"
  LangNcl                  -> "ncl"
  LangNemerle              -> "nemerle"
  LangNesc                 -> "nesc"
  LangNewlisp              -> "newlisp"
  LangNewspeak             -> "newspeak"
  LangNg2                  -> "ng2"
  LangNginx                -> "nginx"
  LangNim                  -> "nim"
  LangNit                  -> "nit"
  LangNixos                -> "nixos"
  LangNotmuch              -> "notmuch"
  LangNsis                 -> "nsis"
  LangNumpy                -> "numpy"
  LangNusmv                -> "nusmv"
  LangObjdumpNasm          -> "objdump-nasm"
  LangObjdump              -> "objdump"
  LangObjectiveC           -> "objective-c"
  LangObjectiveCpp         -> "objective-c++"
  LangObjectiveJ           -> "objective-j"
  LangOcaml                -> "ocaml"
  LangOctave               -> "octave"
  LangOdin                 -> "odin"
  LangOoc                  -> "ooc"
  LangOpa                  -> "opa"
  LangOpenedge             -> "openedge"
  LangPacmanconf           -> "pacmanconf"
  LangPan                  -> "pan"
  LangParasail             -> "parasail"
  LangPawn                 -> "pawn"
  LangPeg                  -> "peg"
  LangPerl6                -> "perl6"
  LangPerl                 -> "perl"
  LangPhp                  -> "php"
  LangPig                  -> "pig"
  LangPike                 -> "pike"
  LangPkgconfig            -> "pkgconfig"
  LangPlpgsql              -> "plpgsql"
  LangPony                 -> "pony"
  LangPostgresql           -> "postgresql"
  LangPostscript           -> "postscript"
  LangPot                  -> "pot"
  LangPov                  -> "pov"
  LangPowershell           -> "powershell"
  LangPraat                -> "praat"
  LangProlog               -> "prolog"
  LangProperties           -> "properties"
  LangProtobuf             -> "protobuf"
  LangPs1con               -> "ps1con"
  LangPsql                 -> "psql"
  LangPug                  -> "pug"
  LangPuppet               -> "puppet"
  LangPy2tb                -> "py2tb"
  LangPycon                -> "pycon"
  LangPypylog              -> "pypylog"
  LangPytb                 -> "pytb"
  LangPython2              -> "python2"
  LangPython               -> "python"
  LangQbasic               -> "qbasic"
  LangQml                  -> "qml"
  LangQvto                 -> "qvto"
  LangRacket               -> "racket"
  LangRagelCpp             -> "ragel-cpp"
  LangRagelC               -> "ragel-c"
  LangRagelD               -> "ragel-d"
  LangRagelEm              -> "ragel-em"
  LangRagelJava            -> "ragel-java"
  LangRagelObjc            -> "ragel-objc"
  LangRagel                -> "ragel"
  LangRagelRuby            -> "ragel-ruby"
  LangRaw                  -> "raw"
  LangRbcon                -> "rbcon"
  LangRb                   -> "rb"
  LangRconsole             -> "rconsole"
  LangRd                   -> "rd"
  LangReason               -> "reason"
  LangRebol                -> "rebol"
  LangRedcode              -> "redcode"
  LangRed                  -> "red"
  LangRegistry             -> "registry"
  LangResource             -> "resource"
  LangRexx                 -> "rexx"
  LangRhtml                -> "rhtml"
  LangRide                 -> "ride"
  LangRnc                  -> "rnc"
  LangRoboconfGraph        -> "roboconf-graph"
  LangRoboconfInstances    -> "roboconf-instances"
  LangRobotFramework       -> "robotframework"
  LangRql                  -> "rql"
  LangR                    -> "r"
  LangRsl                  -> "rsl"
  LangRst                  -> "rst"
  LangRts                  -> "rts"
  LangRust                 -> "rust"
  LangSarl                 -> "sarl"
  LangSas                  -> "sas"
  LangSass                 -> "sass"
  LangScala                -> "scala"
  LangScaml                -> "scaml"
  LangScdoc                -> "scdoc"
  LangScheme               -> "scheme"
  LangScilab               -> "scilab"
  LangSc                   -> "sc"
  LangScss                 -> "scss"
  LangSgf                  -> "sgf"
  LangShen                 -> "shen"
  LangShexc                -> "shexc"
  LangSieve                -> "sieve"
  LangSilver               -> "silver"
  LangSlash                -> "slash"
  LangSlim                 -> "slim"
  LangSlurm                -> "slurm"
  LangSmali                -> "smali"
  LangSmalltalk            -> "smalltalk"
  LangSmarty               -> "smarty"
  LangSml                  -> "sml"
  LangSnobol               -> "snobol"
  LangSnowball             -> "snowball"
  LangSolidity             -> "solidity"
  LangSourceslist          -> "sourceslist"
  LangSparql               -> "sparql"
  LangSpec                 -> "spec"
  LangSplus                -> "splus"
  LangSp                   -> "sp"
  LangSqlite3              -> "sqlite3"
  LangSql                  -> "sql"
  LangSquidconf            -> "squidconf"
  LangSsp                  -> "ssp"
  LangStan                 -> "stan"
  LangStata                -> "stata"
  LangSwift                -> "swift"
  LangSwig                 -> "swig"
  LangSystemverilog        -> "systemverilog"
  LangTads3                -> "tads3"
  LangTap                  -> "tap"
  LangTasm                 -> "tasm"
  LangTcl                  -> "tcl"
  LangTcshcon              -> "tcshcon"
  LangTcsh                 -> "tcsh"
  LangTea                  -> "tea"
  LangTermcap              -> "termcap"
  LangTerminfo             -> "terminfo"
  LangTerraform            -> "terraform"
  LangTex                  -> "tex"
  LangText                 -> "text"
  LangThrift               -> "thrift"
  LangTodotxt              -> "todotxt"
  LangToml                 -> "toml"
  LangTracWiki             -> "trac-wiki"
  LangTreetop              -> "treetop"
  LangTsql                 -> "tsql"
  LangTs                   -> "ts"
  LangTtl                  -> "ttl"
  LangTurtle               -> "turtle"
  LangTwig                 -> "twig"
  LangTyposcriptcssdata    -> "typoscriptcssdata"
  LangTyposcripthtmldata   -> "typoscripthtmldata"
  LangTyposcript           -> "typoscript"
  LangUcode                -> "ucode"
  LangUnicon               -> "unicon"
  LangUrbiscript           -> "urbiscript"
  LangUsd                  -> "usd"
  LangVala                 -> "vala"
  LangVbNet                -> "vb.net"
  LangVbscript             -> "vbscript"
  LangVcl                  -> "vcl"
  LangVclsnippets          -> "vclsnippets"
  LangVctreestatus         -> "vctreestatus"
  LangVelocity             -> "velocity"
  LangVerilog              -> "verilog"
  LangVgl                  -> "vgl"
  LangVhdl                 -> "vhdl"
  LangVim                  -> "vim"
  LangWdiff                -> "wdiff"
  LangWebidl               -> "webidl"
  LangWhiley               -> "whiley"
  LangX10                  -> "x10"
  LangXmlCheetah           -> "xml+cheetah"
  LangXmlDjango            -> "xml+django"
  LangXmlErb               -> "xml+erb"
  LangXmlEvoque            -> "xml+evoque"
  LangXmlLasso             -> "xml+lasso"
  LangXmlMako              -> "xml+mako"
  LangXmlMyghty            -> "xml+myghty"
  LangXmlPhp               -> "xml+php"
  LangXml                  -> "xml"
  LangXmlSmarty            -> "xml+smarty"
  LangXmlVelocity          -> "xml+velocity"
  LangXorgConf             -> "xorg.conf"
  LangXquery               -> "xquery"
  LangXslt                 -> "xslt"
  LangXtend                -> "xtend"
  LangXulMozPreProc        -> "xul+mozpreproc"
  LangYamlJinja            -> "yaml+jinja"
  LangYaml                 -> "yaml"
  LangZeek                 -> "zeek"
  LangZephir               -> "zephir"
  LangZig                  -> "zig"
  LangDefault              -> "text"
