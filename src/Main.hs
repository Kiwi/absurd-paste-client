{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Main where

-- Main.imports
import           Control.Monad                  ( forM
                                                , when
                                                )
import qualified Data.ByteString               as S
import qualified Data.ByteString.Char8         as C
import           Data.Either                    ( rights )
import           Network.Http.Client            ( Method(POST)
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

import qualified System.IO.Streams             as Streams
import           Options.Applicative            ( execParser )

import           Types                          ( Uppity
                                                  ( Uppity
                                                  , debug
                                                  , listServices
                                                  , listExpires
                                                  , service
                                                  , language
                                                  , expiretime
                                                  , serviceLangs
                                                  , inputs
                                                  )
                                                , Service(Service)
                                                , Mode
                                                , s_host
                                                , s_port
                                                , s_endpoint
                                                , s_nvs
                                                , toBody
                                                , Mode(Debug)
                                                , ServiceName
                                                  ( DpasteCom
                                                  , DpasteOrg
                                                  )
                                                )
import           Parser                         ( opts
                                                , getInputString
                                                )
import           LanguageDpasteCom              ( dpasteCom )
import           LanguageDpasteOrg              ( dpasteOrg )
import           Language                       ( unLexer )

main :: IO ()
main = execParser opts >>= run


run :: Uppity -> IO ()
run Uppity { debug = mode, listServices = listSvcs, listExpires = listExps, service = srvc, language = lang, expiretime = exptime, serviceLangs = svcLang, inputs = targets }
  = do
    if listSvcs
      then do
        mapM_ C.putStrLn ["dpaste.com", "dpaste.org"]
        putStrLn ""
      else do
        if not (null svcLang)
          then do
            let strs = map (\l -> " " `S.append` unLexer l) svcLang
            mapM_ S.putStr strs
            putStrLn ""
          else if not (null listExps)
            then do
              mapM_ putStrLn listExps
              putStrLn ""
            else do
              strs <- forM
                targets
                (\t -> do
                  x <- getInputString t
                  case srvc of
                    DpasteCom ->
                      apcPost (dpasteCom (C.pack x) lang exptime) mode
                    DpasteOrg ->
                      apcPost (dpasteOrg (C.pack x) lang exptime) mode
                )
              mapM_ S.putStr (rights strs)

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
