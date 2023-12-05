{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.ShortText (ShortText (contents))
import GHC.Driver.Session (FileSettings (fileSettings_ghcUsagePath))
import Monomer
import Monomer.Lens qualified as L
import System.IO
import System.Random
import TextShow

data ListItem = ListItem
  { _ts :: Millisecond,
    _text :: Text,
    _correctLetters :: Text,
    _wrongLetters :: Text,
    _incorrectLetters :: Text
  }
  deriving (Eq, Show)

data GuessResult = GuessResult
  { _correctPlace :: [Char],
    _wrongPlace :: [Char],
    _incorrect :: [Char]
  }
  deriving (Eq, Show)

data AppModel = AppModel
  { _newItemText :: Text,
    _items :: [ListItem],
    _colorMatch :: GuessResult,
    _target :: Text
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AddItem
  --   | RemoveItem Int
  deriving (Eq, Show)

wordBankFilePath :: FilePath
wordBankFilePath = "Words/wordle-answers-alphabetical.txt"

getWords :: String -> [String]
getWords = words

getRandomElement :: [a] -> IO a
getRandomElement xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index

getRandomWordFromFile :: FilePath -> IO String
getRandomWordFromFile filepath = do
  contents <- readFile filepath
  let wordsList = getWords contents
  getRandomElement wordsList

checkGuess :: Text -> Text -> GuessResult
checkGuess targetText guessText =
  let target = T.unpack targetText
      guess = T.unpack guessText
      correct = [ch | (ch, idx) <- zip guess [0 ..], target !! idx == ch]
      remainingTarget = filter (`notElem` correct) target
      remainingGuess = filter (`notElem` correct) guess
      correctButWrongPlace = intersect remainingTarget remainingGuess
      incorrectGuess = remainingGuess \\ correctButWrongPlace
   in GuessResult correct correctButWrongPlace incorrectGuess

makeLenses 'ListItem
makeLenses 'AppModel
makeLenses ''GuessResult

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    listItem idx item =
      vstack
        [ label_ (item ^. text) [ellipsis] `styleBasic` [textSize 24, paddingH 8],
          spacer,
          label $ "Correct: " <> showt (item ^. correctLetters),
          spacer,
          label $ "Incorrectly placed: " <> showt (item ^. wrongLetters),
          spacer,
          label $ "Incorrect: " <> showt (item ^. incorrectLetters)
        ]
        `nodeKey` showt (item ^. ts)
        `styleBasic` [paddingT 10]

    widgetTree =
      vstack
        [ keystroke [("Enter", AddItem)] $
            hstack
              [ label "Enter your guess here:",
                spacer,
                textField_ newItemText [placeholder "Write here!"]
                  `nodeKey` "description",
                spacer,
                button "Submit" AddItem
                  `styleBasic` [paddingH 5]
                  `nodeEnabled` (model ^. newItemText /= "")
              ],
          separatorLine `styleBasic` [paddingT 20, paddingB 10],
          vstack (zipWith listItem [0 ..] (model ^. items))
        ]
        `styleBasic` [padding 20]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AddItem
    | T.length (model ^. newItemText) == 5 ->
        [ Model $
            model
              & colorMatch .~ colorMatchChars
              & newItemText .~ ""
              & items .~ newItem : model ^. items,
          SetFocusOnKey "description"
        ]
  _ -> []
  where
    colorMatchChars = checkGuess (model ^. target) (model ^. newItemText)
    newItem = ListItem (currentTimeMs wenv) (model ^. newItemText) (T.pack (grabChar colorMatchChars 0)) (T.pack (grabChar colorMatchChars 1)) (T.pack (grabChar colorMatchChars 2))

grabChar :: GuessResult -> Int -> [Char]
grabChar match 0 = match ^. correctPlace
grabChar match 1 = match ^. wrongPlace
grabChar match 2 = match ^. incorrect
grabChar match _ = ""

main :: IO ()
main = do
  randomWord <- getRandomWordFromFile wordBankFilePath -- Get the random word
  startApp (model randomWord) handleEvent buildUI config
  where
    config =
      [ appWindowTitle "WordleGUI",
        appWindowIcon "./assets/images/icon.png",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/ComicSansMS3.ttf",
        appInitEvent AppInit
      ]
    model randomWord = do
      AppModel
        { _newItemText = "",
          _items = [],
          _colorMatch =
            let correctPlace = ""
                wrongPlace = ""
                incorrect = ""
             in GuessResult correctPlace wrongPlace incorrect,
          _target = T.pack randomWord
        }
