{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Set             as S

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mapM_, unless)
import System.Directory
import System.FilePath

import Duffer.Loose
import Duffer.Loose.Objects
import Duffer.Porcelain

main :: IO ()
main = do
    removeDirectoryRecursive "output"
    duffer initRepo
    duffer makeRepo
    return ()

duffer = flip runReaderT "output/.git"

me = PersonTime "Vaibhav Sagar" "me@vaibhavsagar.com" "0000000000" "+0000"

master parent = do
    message           <- liftIO $ B.readFile "content/step0/commit.txt"
    rootTreeHash      <- writeTree $ "content" </> "step0" </> "tree"
    let commitObject  =  Commit rootTreeHash [parent] me me message
    writeObject commitObject
    return commitObject

stepN step parents = do
    message           <- liftIO $ B.readFile $ stepPath </> "commit.txt"
    rootTreeHash      <- writeTree           $ stepPath </> "tree"
    let commitObject  =  Commit rootTreeHash parents me me message
    commitHash        <- writeObject commitObject
    updateRef ("refs/tags/step" ++ show step) commitObject
    return commitHash
    where stepPath = "content/step" ++ show step

makeRepo = do
    step4 <- foldl (>>=) (stepN 1 [])
        [ stepN 2 . return
        , stepN 3 . return
        , stepN 4 . return
        ]
    let annotTag =
            Tag step4 "commit" "step4" me "Look at me. I am the tag now.\n"
    writeObject annotTag
    updateRef "refs/tags/step4" annotTag
    history <- stepN 5 [step4]
    latest <- master history
    updateRef "refs/heads/master" latest
    return latest
