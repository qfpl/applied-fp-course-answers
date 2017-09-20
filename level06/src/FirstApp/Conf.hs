{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Conf
    ( Conf (..)
    , ConfigError (..)
    , Port (getPort)
    , HelloMsg (getHelloMsg)
    , parseOptions
    , mkMessage
    ) where

import           Control.Exception          (catch)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last, getLast),
                                             Monoid (mappend, mempty), (<>))
import           Data.String                (fromString)

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import           Data.Aeson                 (FromJSON)

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson

import           Options.Applicative        (Parser, ParserInfo, eitherReader,
                                             execParser, fullDesc, header, help,
                                             helper, info, long, metavar,
                                             option, optional, progDesc, short,
                                             strOption)

import           Text.Read                  (readEither)

import           FirstApp.DB                (Table (Table))

-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

data ConfigError
  = MissingPort
  | MissingHelloMsg
  | MissingTableName
  | MissingDbFilePath
  | JSONDecodeError String
  | JSONFileReadError IOError
  deriving Show

newtype Port = Port
  { getPort :: Int }
  deriving Show

newtype HelloMsg = HelloMsg
  { getHelloMsg :: ByteString }
  deriving Show

helloFromStr
  :: String
  -> HelloMsg
helloFromStr =
  HelloMsg . fromString

mkMessage
  :: Conf
  -> LBS.ByteString
mkMessage =
  mappend "App says: "
  . getHelloMsg
  . helloMsg

data Conf = Conf
  { port       :: Port
  , helloMsg   :: HelloMsg
  , tableName  :: Table
  , dbFilePath :: FilePath
  }

{-|
Our application will be able to have configuration from both a file and from
command line input. We can use the command line to temporarily override the
configuration from our file. But how to combine them? This question will help us
find which abstraction is correct for our needs...

We want the CommandLine configuration to override the File configuration, so if
we think about combining each of our config records, we want to be able to write
something like this:

defaults <> file <> commandLine

The commandLine should override any options it has input for.

We can use the Monoid typeclass to handle combining the config records together,
and the Last newtype to wrap up our values. The Last newtype is a wrapper for
Maybe that when used with its Monoid instance will always preference the last
Just value that it has:

Last (Just 3) <> Last (Just 1) = Last (Just 1)
Last Nothing  <> Last (Just 1) = Last (Just 1)
Last (Just 1) <> Last Nothing  = Last (Just 1)

To make this easier, we'll make a new record PartialConf that will have our Last
wrapped values. We can then define a Monoid instance for it and have our Conf be
a known good configuration.
-}
data PartialConf = PartialConf
  { pcPort       :: Last Port
  , pcHelloMsg   :: Last HelloMsg
  , pcTableName  :: Last Table
  , pcDbFilePath :: Last FilePath
  }

{-|
We now define our Monoid instance for PartialConf. Allowing us to define our
always empty configuration, which would always fail our requirements. More
interestingly, we define our mappend function to lean on the Monoid instance for
Last to always get the last value.

Note that the types won't be able to completely save you here, if you mess up
the ordering of your 'a' and 'b' you will not end up with the desired result.
-}
instance Monoid PartialConf where
  mempty = PartialConf mempty mempty mempty mempty

  mappend a b = PartialConf
    -- Compiler tells us about the little things we might have forgotten.
    { pcPort      = pcPort a <> pcPort b
    , pcHelloMsg  = pcHelloMsg a <> pcHelloMsg b
    , pcTableName = pcTableName a <> pcTableName b
    , pcDbFilePath = pcDbFilePath a <> pcDbFilePath b
    }

-- We have some sane defaults that we can always rely on, so define them using
-- our PartialConf.
defaultConf
  :: PartialConf
defaultConf = PartialConf
  (pure (Port 3000))
  (pure (HelloMsg "World!"))
  (pure (Table "comments"))
  (pure "firstapp_db.db")

-- We need something that will take our PartialConf and see if can finally build
-- a complete Conf record. Also we need to highlight any missing config values
-- by providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc = Conf
  <$> lastToEither MissingPort pcPort
  <*> lastToEither MissingHelloMsg pcHelloMsg
  <*> lastToEither MissingTableName pcTableName
  <*> lastToEither MissingDbFilePath pcDbFilePath
  where
    -- You don't need to provide type signatures for most functions in where/let
    -- sections. Sometimes the compiler might need a bit of help, or you would
    -- like to be explicit in your intentions.
    lastToEither e g =
      maybe (Left e) Right . getLast $ g pc

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions fp =
  let mkCfg cli file = makeConfig (defaultConf <> file <> cli)
  in do
    cli' <- execParser commandLineParser
    ( >>= mkCfg cli' ) <$> parseJSONConfigFile fp

-- | File Parsing

-- | fromJsonObjWithKey
-- >>> let (Just obj) = ( Aeson.decode "{\"foo\":\"Susan\"}" ) :: Maybe Aeson.Object
--
-- >>> fromJsonObjWithKey "foo" (id :: Text -> Text) obj
-- Last {getLast = Just "Susan"}
--
-- >>> fromJsonObjWithKey "foo" id obj
-- Last {getLast = Nothing}
--
fromJsonObjWithKey
  :: FromJSON a
  => Text
  -> (a -> b)
  -> Aeson.Object
  -> Last b
fromJsonObjWithKey k c obj =
  Last ( c <$> Aeson.parseMaybe (Aeson..: k) obj )

-- | decodeObj
-- >>> decodeObj ""
-- Left (JSONDecodeError "Error in $: not enough input")
--
-- >>> decodeObj "{\"bar\":33}"
-- Right (fromList [("bar",Number 33.0)])
--
decodeObj
  :: ByteString
  -> Either ConfigError Aeson.Object
decodeObj =
  first JSONDecodeError . Aeson.eitherDecode

-- | readObject
-- >>> readObject "badFileName.no"
-- Left (JSONFileReadError badFileName.no: openBinaryFile: does not exist (No such file or directory))
--
-- >>> readObject "test.json"
-- Right "{\"foo\":33}\n"
--
readObject
  :: FilePath
  -> IO (Either ConfigError ByteString)
readObject fp =
  (Right <$> LBS.readFile fp) `catch` (pure . Left . JSONFileReadError)

parseJSONConfigFile
  :: FilePath
  -> IO ( Either ConfigError PartialConf )
parseJSONConfigFile fp =
  (>>= fmap toPartialConf . decodeObj) <$> readObject fp
  where
    toPartialConf
      :: Aeson.Object
      -> PartialConf
    toPartialConf cObj = PartialConf
      ( fromJsonObjWithKey "port" Port cObj )
      ( fromJsonObjWithKey "helloMsg" helloFromStr cObj )
      ( fromJsonObjWithKey "tableName" Table cObj )
      ( fromJsonObjWithKey "dbFilePath" id cObj )

-- | Command Line Parsing

-- We will use the optparse-applicative package to build our command line
-- parser, as this problem is fraught with silly dangers and we appreciate
-- someone else having eaten this gremlin on our behalf.
commandLineParser
  :: ParserInfo PartialConf
commandLineParser =
  let mods = fullDesc
        <> progDesc "Manage comments for something"
        <> header "Your first Haskell app!"
  in
    info (helper <*> partialConfParser) mods

-- Combine the smaller parsers into our larger PartialConf type.
partialConfParser
  :: Parser PartialConf
partialConfParser = PartialConf
  <$> portParser
  <*> strParse helloFromStr helloMods
  -- Add our two new fields to the parsing of the PartialConf record. Note that
  -- if you update the data structure the compiler will do its best to inform
  -- you about everywhere that needs attention.
  <*> strParse ( Table . Text.pack ) tableMods
  <*> strParse id dbFilePathMods
  where
    -- With the addition of two new very similar parsers, we can abstract out
    -- part of the construction into a separate function so we avoid repeating
    -- ourselves.
    strParse c m =
      Last <$> optional (c <$> strOption m)

    helloMods = long "hello-msg"
                <> short 'm'
                <> metavar "HELLOMSG"
                <> help "Message to respond to requests with."

    dbFilePathMods = long "db-filepath"
                 <> short 'd'
                 <> metavar "DBFILEPATH"
                 <> help "FilePath to the SQLite DB"

    tableMods = long "table-name"
                 <> short 't'
                 <> metavar "TABLENAME"
                 <> help "Comments DB table name"

-- Parse the Port value off the command line args and into our Last wrapper.
portParser
  :: Parser (Last Port)
portParser =
  let mods = long "port"
             <> short 'p'
             <> metavar "PORT"
             <> help "TCP Port to accept requests on"
      portReader =
        eitherReader (fmap Port . readEither)
  in
    Last <$> optional (option portReader mods)
