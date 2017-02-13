{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B

import Control.Monad (unless)
import System.Directory
import System.FilePath

import Duffer.Loose.Objects
import Duffer.Plumbing
import Duffer.WithRepo
import Duffer.Unified

main :: IO ()
main = do
    notExisting <- not <$> doesDirectoryExist     "workshop"
    unless notExisting $ removeDirectoryRecursive "workshop"
    duffer initRepo
    duffer makeRepo
    return ()

duffer :: ReaderT Repo m a -> m a
duffer = flip runReaderT "workshop/.git"

me :: PersonTime
me = PersonTime "Vaibhav Sagar" "me@vaibhavsagar.com" "0000000000" "+0000"

master :: Ref -> WithRepo GitObject
master parent = do
    msg               <- liftIO $ B.readFile "content/step0/commit.txt"
    rootTreeHash      <- writeTree $ "content" </> "step0" </> "tree"
    let commitObject  =  Commit rootTreeHash [parent] me me msg
    writeObject commitObject
    return commitObject

stepN :: Int -> [Ref] -> WithRepo Ref
stepN step parents = do
    msg               <- liftIO $ B.readFile $ stepPath </> "commit.txt"
    rootTreeHash      <- writeTree           $ stepPath </> "tree"
    let commitObject  =  Commit rootTreeHash parents me me msg
    commitHash        <- writeObject commitObject
    updateRef ("refs/tags/step" </> show step) commitObject
    return commitHash
    where stepPath = "content/step" </> show step

makeRepo :: WithRepo GitObject
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
