{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Args
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.AWS
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.AWS.SSM
import           System.IO
import           System.Posix

data EnvVar = EnvVar
  { evName :: !Text
  , evSSMName :: !Text
  } deriving (Eq, Show)

data Cmd = Cmd
  { cmdEnv :: [EnvVar]
  , cmdExe :: [Text]
  } deriving (Eq, Show)


parseCmd :: Args -> Either Text Cmd
parseCmd Args{..} = do
  let (evStrs, cmds) = span (isJust . T.find (== '=')) argsSSMParamsCmd
  when (null evStrs) $ Left "Please specify at least one SSM parameter substitution, see --help for more"
  when (null cmds) $ Left "Please specify the command to run, see --help for more"

  evs <- traverse parseEnvVar evStrs
  Right $ Cmd evs cmds

  where parseEnvVar e = let evErr = Left $ "Unable to parse env var" <> e
                         in case T.splitOn "=" e
                              of [n, ssmn] -> do when (T.null n || T.null ssmn) evErr
                                                 Right $ EnvVar n (maybe "" (<> ssmn) argsPrefix)
                                 _         -> evErr


main:: IO ()
main = runWithArgs $ \args@Args{..} -> do
  cmd@Cmd{..} <- either (error . T.unpack) return (parseCmd args)
  when argsVerbose $ print cmd

  lgr <- newLogger (if argsVerbose then Debug else Error) stderr
  env <- newEnv Discover <&> set envLogger lgr

  let req = getParameters (NEL.fromList (evSSMName <$> cmdEnv)) & (gpWithDecryption .~ Just True)

  ssmPs <- runResourceT . runAWST env $ do
    res <- send req
    let invalidPs = res ^. grsInvalidParameters
    unless (null invalidPs) $ error $ "Invalid parameters: " <> show invalidPs
    return $ res ^. grsParameters

  let !pnvs = Map.fromList $ (\p -> (fromMaybe "" (p ^. pName), fromMaybe "" (p ^. pValue))) <$> ssmPs
      cmdenvs = (\EnvVar{..} -> (evName, fromMaybe "" (Map.lookup evSSMName pnvs))) <$> cmdEnv

  forM_ cmdenvs $ \(n,v) -> setEnv (T.unpack n) (T.unpack v) True
  executeFile ((T.unpack . head) cmdExe) True (T.unpack <$> tail cmdExe) Nothing
