{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (when)
import Data.Function ((&))
import Data.Text (Text)
import Data.Word (Word8)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Stream (Stream)
import Text.Printf (printf)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified FlatParse.Basic as FlatParse
import qualified Options.Applicative as Options
import qualified Streamly.Console.Stdio as Stdio
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.External.ByteString as ByteString
import qualified System.Console.ANSI as AnsiTerminal

isNewline :: Word8 -> Bool
isNewline = (== fromIntegral (Char.ord '\n'))

splitOn :: (a -> Bool) -> Fold IO a b -> Stream IO a -> Stream IO b
splitOn p f = Stream.foldMany (Fold.takeEndBy_ p f)

data Progress = Progress
  { progress :: {-# UNPACK #-} Word
  , total :: {-# UNPACK #-} Word
  , moduleName :: Text
  }

progressParser :: FlatParse.Parser () Progress
progressParser = do
  _ <- $(FlatParse.string "[")
  FlatParse.skipMany $(FlatParse.string " ")
  progress <- FlatParse.anyAsciiDecimalWord
  _ <- $(FlatParse.string " of ")
  total <- FlatParse.anyAsciiDecimalWord
  _ <- $(FlatParse.string "] Compiling ")
  moduleName <- Text.pack <$> FlatParse.some (FlatParse.satisfy (/= ' '))
  pure Progress{ progress, total, moduleName }

formatLine :: Progress -> Text
formatLine Progress{ progress, total, moduleName } =
  Text.concat
    [ "["
    , Text.pack (printf "%2d" progress)
    , " of "
    , Text.pack (show total)
    , "] Compiling "
    , moduleName
    ]

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust m k = maybe (pure ()) k m

data Options = Options
  { quiet :: !Bool
  }

parseOptions :: Options.Parser Options
parseOptions = do
  quiet <-
    Options.switch . mconcat $
      [ Options.long "quiet"
      , Options.help "Suppress logging, only show progress bar"
      ]

  pure Options{ quiet }

getOptions :: IO Options
getOptions =
  Options.execParser $ Options.info (Options.helper <*> parseOptions) mempty

main :: IO ()
main = do
  options <- getOptions

  prevProgressIORef <- IORef.newIORef Nothing

  let clearPrevProgress = do
        prevProgress <- IORef.readIORef prevProgressIORef
        whenJust prevProgress \_ -> do
          AnsiTerminal.cursorUpLine 1
          AnsiTerminal.clearLine

  let rememberProgress progress = do
        IORef.writeIORef prevProgressIORef (Just progress)

  let restorePrevProgress = do
        prevProgress <- IORef.readIORef prevProgressIORef
        whenJust prevProgress ByteString.putStr

  do
      Stream.unfold Stdio.reader ()
    & splitOn isNewline ByteString.write
    & Stream.fold (Fold.drainMapM \bytes -> do

        case FlatParse.runParser progressParser bytes of
          FlatParse.OK line _ -> do
            clearPrevProgress
            let progress = Text.encodeUtf8 (formatLine line `Text.snoc` '\n')
            rememberProgress progress
            ByteString.putStr progress

          _ ->
            when (not options.quiet) do
              clearPrevProgress
              ByteString.putStr $ bytes `Char8.snoc` '\n'
              restorePrevProgress
      )

  clearPrevProgress
