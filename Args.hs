{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Args (
  Args(..)
, runWithArgs
) where


import           Control.Monad
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Development.GitRev
import           Options.Applicative
import           System.Exit
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Args = Args
  { argsVerbose      :: !Bool
  , argsVersion      :: !Bool
  , argsPrefix       :: !(Maybe Text)
  , argsSSMParamsCmd :: [Text]
  } deriving (Show)


parseArgs :: Parser Args
parseArgs =
  Args <$> switch (long "verbose" <> short 'v' <> help "Be verbose.") <*>
  switch (long "version" <> short 'V' <> help "Print version and exit.") <*>
  optional
    (T.pack <$> option
       str
       (long "prefix" <> short 'p' <> metavar "/ssm/prefix" <>
        help "prefix applied to all SSM parameters")) <*>
  many
    (T.pack <$> argument
       str
       (metavar "ENV_FOO=ssm/path/foo ENV_BAR=ssm/path/bar cmd -opt1 ... arg1"))


runWithArgs :: (Args -> IO ()) -> IO ()
runWithArgs rwa = execParser opts >>= printVersion >>= rwa
  where
    opts =
      info
        (helper <*> parseArgs)
        (fullDesc <>
         header
           "Runs given command in a modified shell environment, populated from SSM parameters." <>
         progDesc
           "Runs given command in a modified shell environment, populated from SSM parameters.Source: https://github.com/andreyk0/run-with-ssm" <>
         (footerDoc . Just)
           (PP.text
              "See https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html for SSM docs." PP.<+>
            PP.linebreak PP.<$>
            (PP.indent
               2
               ((PP.text
                   "E.g. $ run-with-ssm --prefix /svc-foo/ -- BAR=bar BAZ=baz someprog -arg1 -arg2 ...") PP.<$>
                (PP.text
                   "Runs 'someprog' with BAR environmental variable set to the SSM /svc-foo/bar and BAZ variable set to /svc-foo/baz")))))
    printVersion args@Args {..} = do
      when argsVersion $ die $ "Version: " <> $(gitBranch) <> "@" <> $(gitHash)
      return args
