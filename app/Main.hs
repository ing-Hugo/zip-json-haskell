{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant variable capture" #-}


module Main where
               

import           Codec.Archive.Zip (createArchive,mkEntrySelector,addEntry,CompressionMethod (..))
import qualified Control.Exception as E
import qualified Data.ByteString.Internal as IB
import qualified Data.ByteString.Lazy as B
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import           Data.Text.Encoding(encodeUtf8)
import qualified Data.Text.IO as TI
import           System.Directory ()

import           System.IO ( hClose, openFile, IOMode(ReadMode,WriteMode),hPutStrLn,hGetLine )



   

getCounts :: T.Text -> (Int,Int,Int)

getCounts input = (charCount,wordCount,lineCount)
          where charCount = T.length input
                wordCount = (length . T.words) input
                lineCount = (length . T.lines) input

countText :: Show int => (int,int,int)->T.Text
countText (cc,wc,lc) =T.pack (unwords ["chars:",show cc,"words:",show wc,"lines:",show lc])





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
    createArchive "C:/Users/arhuoje/haskell/zip1/app/zip/xapi-example_1.zip" (addEntry Deflate binary s)