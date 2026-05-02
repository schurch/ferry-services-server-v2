{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Env (Env (logger))
import App.Logger (logInfo)
import App.Runner (createEnv)
import Control.Monad.Reader (runReaderT)
import OfflineSnapshot
  ( OfflineSnapshotMetadata (..),
    generateAndWriteOfflineSnapshot,
  )

main :: IO ()
main = do
  env <- createEnv
  metadata <- runReaderT generateAndWriteOfflineSnapshot env
  logInfo (logger env) $
    "Offline snapshot ready: "
      <> offlineSnapshotMetadataDataVersion metadata
      <> " valid "
      <> show (offlineSnapshotMetadataValidFrom metadata)
      <> " to "
      <> show (offlineSnapshotMetadataValidTo metadata)
