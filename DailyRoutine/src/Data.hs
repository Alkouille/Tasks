{-# LANGUAGE TemplateHaskell #-}

module Data where

import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Activities


savePath :: FilePath
savePath = "data.json"


readActivities :: IO (Either String Activities)
readActivities = do
  text <- B.readFile savePath
  return $ eitherDecode text



writeActivities :: Activities -> IO ()
writeActivities activities = B.writeFile savePath (encode activities)
