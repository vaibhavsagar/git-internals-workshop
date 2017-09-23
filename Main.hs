{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B

import Control.Monad (unless, foldM, void)
import System.Directory
import System.FilePath

import Duffer.Loose.Objects
import Duffer.Plumbing
import Duffer.WithRepo
import Duffer.Unified

main :: IO ()
main = do
    doesn'tExist <- not <$> doesDirectoryExist     "workshop"
    unless doesn'tExist $ removeDirectoryRecursive "workshop"
    void $ withRepo "workshop/.git" (initRepo >> makeRepo)

me :: PersonTime
me = PersonTime "Vaibhav Sagar" "me@vaibhavsagar.com" "0" "+0000"

master :: [Ref] -> WithRepo GitObject
master parents = do
    msg               <- liftIO $ B.readFile "content/step0/commit.txt"
    rootTreeHash      <- writeTree $ "content" </> "step0" </> "tree"
    let commitObject  =  Commit rootTreeHash parents me me Nothing msg
    writeObject commitObject
    return commitObject

stepN :: [Ref] -> Int -> WithRepo [Ref]
stepN parents step = do
    msg              <- liftIO $ B.readFile $ stepPath </> "commit.txt"
    rootTreeHash     <- writeTree           $ stepPath </> "tree"
    let commitObject =  Commit rootTreeHash parents me me Nothing msg
    updateRef ("refs/tags/step" ++ show step) commitObject
    return <$> writeObject commitObject
    where stepPath = "content/step" ++ show step

makeRepo :: WithRepo GitObject
makeRepo = do
    steps  <- foldM stepN [] [1..4]
    let step4    = head steps
    let annotTag = Tag step4 "commit" "step4" me "Look at me. I am the tag now.\n"
    writeObject annotTag
    updateRef "refs/tags/step4" annotTag
    latest <- master =<< stepN steps 5
    updateRef "refs/heads/master" latest
    return latest
