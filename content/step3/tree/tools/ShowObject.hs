#!/usr/bin/env stack
-- stack --resolver lts-9.0 script

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Compression.Zlib           as Z (decompress)
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString                  as B
import           Data.ByteString.Base16                (encode)
import qualified Data.ByteString.Char8            as C
import           Data.ByteString.Lazy                  (fromStrict, toStrict)
import           Data.ByteString.UTF8                  (fromString, toString)
import           Data.Digest.Pure.SHA
import           Data.List
import           GHC.Word                              (Word8)
import           System.Environment
import           System.FilePath
import           System.Directory

sha1ToDirectory :: String -> FilePath -> FilePath
sha1ToDirectory (x:y:_) dir = dir </> "objects" </> [x,y]

sha1ToPath :: String -> FilePath -> FilePath
sha1ToPath (x:y:suffix) dir = dir </> "objects" </> [x,y] </> suffix

disambiguateSha1 :: String -> FilePath -> IO String
disambiguateSha1 sha1@(x:x':suffix) dir = do
    let prefix    =  [x,x']
    let searchDir =  sha1ToDirectory sha1 dir
    matchingFiles <- filter (isPrefixOf suffix) <$> listDirectory searchDir
    case length matchingFiles `compare` 1 of
        LT -> error "no object exists with that SHA1"
        GT -> error "ambiguous SHA1 provided"
        EQ -> return $ prefix ++ head matchingFiles

readRef :: FilePath -> IO String
readRef = fmap init . readFile

resolveRef :: String -> FilePath -> IO String
resolveRef ref dir = do
    let symRefPath      =  dir </> ref
    symRefPathExists    <- doesFileExist symRefPath
    let refsHeadsPath   =  dir </> "refs" </> "heads" </> ref
    refsHeadsPathExists <- doesFileExist refsHeadsPath
    let refsTagsPath    =  dir </> "refs" </> "tags"  </> ref
    refsTagsPathExists  <- doesFileExist refsTagsPath
    case () of
      _ | "ref" `isPrefixOf` ref -> readRef symRefPath
        | symRefPathExists       -> do
            content <- readRef symRefPath
            if "ref: " `isPrefixOf` content
                then resolveRef (drop 5 content) dir
                else return content
        | refsHeadsPathExists    -> readRef refsHeadsPath
        | refsTagsPathExists     -> readRef refsTagsPath
        | otherwise              -> disambiguateSha1 ref dir

hash :: ByteString -> ByteString
hash = fromString . showDigest . sha1 . fromStrict

decompress :: ByteString -> ByteString
decompress = toStrict . Z.decompress . fromStrict

parseTreeEntry :: Parser [ByteString]
parseTreeEntry = do
    perms <- fromString <$> A.many' A.digit
    A.space
    name  <- A.takeWhile (/= '\NUL')
    A.char '\NUL'
    ref   <- encode <$> A.take 20
    return [perms, name, ref]

parsedObject :: ByteString -> [ByteString]
parsedObject raw = let
    (headerLen, rest) = C.break (=='\NUL') raw
    content           = B.tail rest
    objectType        = fst $ C.break (==' ') headerLen
    in case objectType of
        "tree" -> let
                parser = A.many' parseTreeEntry
                parsed = either error id $ A.parseOnly parser content
            in map (B.intercalate "\t") parsed
        _ -> [content]

main :: IO ()
main = do
    (ref':_) <- getArgs
    ref      <- resolveRef ref' ".git"
    let path =  sha1ToPath ref  ".git"
    content  <- decompress <$> B.readFile path
    traverse C.putStrLn (parsedObject content)
    unless (fromString ref == hash content) $ error "refs don't match"
