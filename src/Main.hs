{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (when)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Char qualified as Char
import Data.Function ((&))
import Data.IORef qualified as IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import FlatParse.Basic qualified as FlatParse
import Options.Applicative qualified as Options
import Streamly.Console.Stdio qualified as Stdio
import Streamly.Data.Fold (Fold)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.External.ByteString qualified as ByteString
import System.Console.ANSI qualified as AnsiTerminal
import Text.Printf (printf)

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
