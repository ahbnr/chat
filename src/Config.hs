module Config where

import Data.Ini.Config (IniParser, sectionMb, fieldOf, string, number, parseIniFile)

import Data.Text
import Data.Maybe (fromMaybe)

import Control.Monad (join)
import Control.Exception (try, IOException)

import Data.Either.Combinators (rightToMaybe)
import System.FilePath ( (</>) )
import System.Directory ( getXdgDirectory, XdgDirectory (XdgConfig), doesFileExist )

data Config = Config
  { 
    cfIrc :: Maybe IrcConfig
  } deriving (Eq, Show)

data IrcConfig = IrcConfig
  { ircServer :: String
  , ircPort   :: Int
  } deriving (Eq, Show)

defaultConfig = Config {
    cfIrc = Nothing
  }

configParser :: IniParser Config
configParser = do
  ircCf <- sectionMb (Data.Text.pack "IRC") $
    IrcConfig <$> fieldOf (Data.Text.pack "server") string
              <*> fieldOf (Data.Text.pack "port") number
  return Config {
      cfIrc = ircCf
    }

parseConfig :: IO Config
parseConfig = do
  result <- try (do
    chatConfigDir <- getXdgDirectory XdgConfig "chat"
    let chatConfigFile = chatConfigDir </> "config.ini"

    configPresent <- doesFileExist chatConfigFile
    if configPresent then do
      configContent <- readFile chatConfigFile
      (pure . rightToMaybe) (parseIniFile (Data.Text.pack configContent) configParser)
    else 
      pure Nothing
    ) :: IO (Either IOException (Maybe Config))
  (pure . fromMaybe defaultConfig . join . rightToMaybe) result
