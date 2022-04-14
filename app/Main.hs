{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant variable capture" #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}



module Main where


import           Codec.Archive.Zip (createArchive,withArchive,unpackInto,mkEntrySelector,addEntry,getEntry,CompressionMethod (..))
import qualified Control.Exception as E
import qualified Data.ByteString.Internal as IB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import           Data.Text.Encoding(encodeUtf8)
import qualified Data.Text.IO as TI
import           System.Directory ()

import           System.IO ( hClose, openFile, IOMode(ReadMode,WriteMode),hPutStrLn,hGetLine, Handle,hIsEOF)
import Data.ByteString (ByteString)
import qualified Data.String
import GHC.IO.Exception (IOErrorType(EOF))





getCounts :: T.Text -> (Int,Int,Int)

getCounts input = (charCount,wordCount,lineCount)
          where charCount = T.length input
                wordCount = (length . T.words) input
                lineCount = (length . T.lines) input

countText :: Show int => (int,int,int)->T.Text
countText (cc,wc,lc) =T.pack (unwords ["chars:",show cc,"words:",show wc,"lines:",show lc])


word::Num word8 =>word8
word = 0x0A

--mayorLine64Bytes::ByteString->IO ByteString
--mayorLine64Bytes restLine =  do  let firstFile = "C:/Users/arhuoje/haskell/zip1/app/zip/firstFile.txt"
--                                 let firstLine64Bytes =B.take 64 restLine 
--                                 let firstLine64Bytes1 = B.snoc  firstLine64Bytes word
--                                 B.appendFile firstFile firstLine64Bytes1  
--                                 B.readFile firstFile  

readLineFile :: Handle->IO ByteString
readLineFile pathFile  = do  let firstFile = "C:/Users/arhuoje/haskell/zip1/app/zip/firstFile.txt"
                            
                             
                             hasLine <- hIsEOF pathFile 
                             if hasLine  then
                                 do
                                   B.readFile firstFile
                             else
                                 do
                                   primerLine <- B.hGetLine pathFile  
                                   let lengthBytes = B.length primerLine
                                   if lengthBytes  > 64 then
                                    do

                                    let firstLine64Bytes =B.take 64 primerLine
                                    let firstLine64Bytes1 = B.snoc  firstLine64Bytes word
                                    B.appendFile firstFile firstLine64Bytes1
                                    let secondLine64Bytes =B.drop 64 primerLine
                                    let restLineBytes =B.length secondLine64Bytes
                                    if restLineBytes<64 then
                                        do
                                          let secondLine64Bytes1 = B.snoc  secondLine64Bytes word
                                          B.appendFile firstFile secondLine64Bytes1
                                          readLineFile pathFile
                                    else do
                                        mayorLine64Bytes secondLine64Bytes
                                       -- B.readFile firstFile   
                                        readLineFile pathFile
                                   else do
                                    let firstLine64Bytes =B.take 64 primerLine
                                    let firstLine64Bytes1 = B.snoc  firstLine64Bytes word
                                    B.appendFile firstFile firstLine64Bytes1
                                    --B.readFile firstFile
                                    readLineFile pathFile

            where mayorLine64Bytes::ByteString->IO()
                  mayorLine64Bytes restLine =  do   let firstFile = "C:/Users/arhuoje/haskell/zip1/app/zip/firstFile.txt"
                                                    let firstLine64Bytes =B.take 64 restLine
                                                    let firstLine64Bytes1 = B.snoc  firstLine64Bytes word
                                                    B.appendFile firstFile firstLine64Bytes1
                                                    let secondLine64Bytes =B.drop 64 restLine
                                                    let restLineBytes =B.length secondLine64Bytes
                                                    if restLineBytes>64 then do
                                                            mayorLine64Bytes secondLine64Bytes
                                                        else do
                                                            let secondLine64Bytes1 = B.snoc secondLine64Bytes word
                                                            B.appendFile firstFile secondLine64Bytes1
main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    print "Open File json!!!!"

    let fileName = "C:/Users/arhuoje/haskell/zip1/app/json/xapi-example.json"
    input <- TI.readFile fileName

    let binary = encodeUtf8 input
    let summary = (countText.getCounts) input
    TI.appendFile "stat.dat" (mconcat [T.pack fileName,"",summary,"\n"])
    TI.putStrLn summary
    s      <- mkEntrySelector "xapi-example_1.json"
    c      <- mkEntrySelector "example_1.zip"
    createArchive "C:/Users/arhuoje/haskell/zip1/app/zip/xapi-example_1.zip" (addEntry Deflate binary s)
    let pathZipFile = "C:/Users/arhuoje/haskell/zip1/app/zip/xapi-example_1.zip"
    zipFile<- openFile pathZipFile ReadMode
    firstLine <-readLineFile zipFile
    --firstLine <- B.hGetLine zipFile
  --  let firstLine = readLineFile pathZipFile
   --print firstLine

    --let lengthLine = B.length firstLine
    --let line64Bytes = B.take 64 firstLine
    createArchive "C:/Users/arhuoje/haskell/zip1/app/zip/example_1.zip" (addEntry Store firstLine c )
    --print line64Bytes
    --let line64Bytes = B.drop 64 firstLine
    --print line64Bytes
    print firstLine
    hClose zipFile
    --withArchive "C:/Users/arhuoje/haskell/zip1/app/zip/xapi-example_1.zip" (unpackInto "C:/Users/arhuoje/haskell/zip1/app/zip/")
   