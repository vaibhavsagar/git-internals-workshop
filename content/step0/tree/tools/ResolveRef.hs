#!/usr/bin/env stack
-- stack --resolver lts-9.0 script

module Main where

import System.Environment
import System.FilePath

resolveRef :: String -> FilePath -> IO String
resolveRef ref dir = case ref of
    "HEAD" -> do
        content <- readFile $ dir </> "HEAD"
        let pointedRef = init . drop 5 $ content
        resolveRef pointedRef dir
    _      -> do
        content <- readFile $ dir </> ref
        return $ init content

main :: IO ()
main = do
    (ref:_) <- getArgs
    resolved <- resolveRef ref ".git"
    putStrLn resolved
