{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Char
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

-- DATA TYPE FOR EACH GUESS ITEM OUTPUTTED TO THE USER --
data ListItem = ListItem
  { _text :: Text,
    _itemChar1 :: ([Char], Int),
    _itemChar2 :: ([Char], Int),
    _itemChar3 :: ([Char], Int),
    _itemChar4 :: ([Char], Int),
    _itemChar5 :: ([Char], Int)
  }
  deriving (Eq, Show)

-- DATA TYPE TO STORE POSITIONAL INFORMATION ABOUT GUESS RESULTS --
data GuessResult = GuessResult
  { _char1 :: ([Char], Int),
    _char2 :: ([Char], Int),
    _char3 :: ([Char], Int),
    _char4 :: ([Char], Int),
    _char5 :: ([Char], Int)
  }
  deriving (Eq, Show)

-- DATA TYPE WHICH DEFINES THE MODEL FOR THE APPLICATION AND ALL ITS FIELDS --
data AppModel = AppModel
  { _guessText :: Text,
    _items :: [ListItem],
    _colorMatch :: GuessResult,
    _target :: Text,
    _gameWon :: Bool
  }
  deriving (Eq, Show)

-- DATA TYPE DEFINING THE EVENTS OF THE APPLICATION --
data AppEvent
  = AppInit
  | GenerateGuess
  deriving (Eq, Show)

-- RANDOM WORD FROM WORD BANK SELECTION HELPERS --
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

-- GUESS COMPARISON HELPERS --
checkGuess :: String -> String -> GuessResult
checkGuess target guess =
  GuessResult
    { _char1 = compareChars (target !! 0) (guess !! 0) target,
      _char2 = compareChars (target !! 1) (guess !! 1) target,
      _char3 = compareChars (target !! 2) (guess !! 2) target,
      _char4 = compareChars (target !! 3) (guess !! 3) target,
      _char5 = compareChars (target !! 4) (guess !! 4) target
    }

compareChars :: Char -> Char -> String -> ([Char], Int)
compareChars tChar gChar target
  | tChar == gChar = ([gChar], 0)
  | gChar `elem` target = ([gChar], 1)
  | otherwise = ([gChar], 2)

-- HELPER TO PULL OUT GUESS RESULTS --
labelText :: ([Char], Int) -> Text
labelText (_, 0) = T.pack "G"
labelText (_, 1) = T.pack "Y"
labelText (_, 2) = T.pack "R"
labelText _ = T.pack "Invalid Label"

-- LENSES TO SUFFICIENTLY LOOK INSIDE DATATYPES --
makeLenses 'ListItem
makeLenses 'AppModel
makeLenses ''GuessResult

-- BUILDUI TO BUILD THE UI OFF THE GIVEN MODEL AND ACTUALLY OUTPUT, BUILD AND STYLE THINGS ONTO THE PANEL FOR THE USER --
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
          hstack
            [ label "Chars: ",
              spacer,
              label ("" <> T.pack (show $ fst (item ^. itemChar1))) `styleBasic` [textSize 20],
              spacer,
              label ("" <> T.pack (show $ fst (item ^. itemChar2))) `styleBasic` [textSize 20],
              spacer,
              label ("" <> T.pack (show $ fst (item ^. itemChar3))) `styleBasic` [textSize 20],
              spacer,
              label ("" <> T.pack (show $ fst (item ^. itemChar4))) `styleBasic` [textSize 20],
              spacer,
              label ("" <> T.pack (show $ fst (item ^. itemChar5))) `styleBasic` [textSize 20]
            ],
          spacer,
          hstack
            [ label "Labels: ",
              spacer,
              if labelText (item ^. itemChar1) == "G"
                then label "▪" `styleBasic` [textSize 40, textColor green]
                else
                  if labelText (item ^. itemChar1) == "Y"
                    then label "▪" `styleBasic` [textSize 40, textColor yellow]
                    else label "▪" `styleBasic` [textSize 40, textColor red],
              spacer,
              if labelText (item ^. itemChar2) == "G"
                then label "▪" `styleBasic` [textSize 40, textColor green]
                else
                  if labelText (item ^. itemChar2) == "Y"
                    then label "▪" `styleBasic` [textSize 40, textColor yellow]
                    else label "▪" `styleBasic` [textSize 40, textColor red],
              spacer,
              if labelText (item ^. itemChar3) == "G"
                then label "▪" `styleBasic` [textSize 40, textColor green]
                else
                  if labelText (item ^. itemChar3) == "Y"
                    then label "▪" `styleBasic` [textSize 40, textColor yellow]
                    else label "▪" `styleBasic` [textSize 40, textColor red],
              spacer,
              if labelText (item ^. itemChar4) == "G"
                then label "▪" `styleBasic` [textSize 40, textColor green]
                else
                  if labelText (item ^. itemChar4) == "Y"
                    then label "▪" `styleBasic` [textSize 40, textColor yellow]
                    else label "▪" `styleBasic` [textSize 40, textColor red],
              spacer,
              if labelText (item ^. itemChar5) == "G"
                then label "▪" `styleBasic` [textSize 40, textColor green]
                else
                  if labelText (item ^. itemChar5) == "Y"
                    then label "▪" `styleBasic` [textSize 40, textColor yellow]
                    else label "▪" `styleBasic` [textSize 40, textColor red]
            ]
        ]
        `styleBasic` [paddingT 10]
    widgetTree =
      vstack
        [ keystroke [("Enter", GenerateGuess)] $
            hstack
              [ label "Enter your guess here:",
                spacer,
                textField_ guessText [placeholder "Write here!"]
                  `nodeKey` "description"
                  `nodeEnabled` not (model ^. gameWon),
                spacer,
                button "Submit" GenerateGuess
                  `styleBasic` [paddingH 5]
                  `nodeEnabled` not (model ^. gameWon && model ^. guessText /= "")
              ],
          separatorLine `styleBasic` [paddingT 20, paddingB 10],
          if model ^. gameWon
            then label ("Correct! The word was: " <> (model ^. target)) `styleBasic` [textSize 24, padding 10]
            else spacer,
          vscroll (vstack (zipWith listItem [0 ..] (model ^. items))) `nodeKey` "mainScroll"
        ]
        `styleBasic` [padding 20]

-- EVENT HANDLER WHICH HANDLES MOST OF THE INTERNAL GAME LOGIC, JUST HAS INIT AND GUESS EVENTS --
handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  GenerateGuess
    | all isAlpha (T.unpack (model ^. guessText)) && not (model ^. gameWon) && T.length (model ^. guessText) == 5 ->
        let newModel =
              model
                & colorMatch .~ colorMatchChars
                & guessText .~ ""
                & items .~ newItem : model ^. items
         in if model ^. target == model ^. guessText
              then
                [ Model $
                    newModel & gameWon .~ True,
                  SetFocusOnKey "description"
                ]
              else
                [ Model newModel,
                  SetFocusOnKey "description"
                ]
  _ -> []
  where
    colorMatchChars = checkGuess (T.unpack (model ^. target)) (map toLower (T.unpack (model ^. guessText)))
    newItem = ListItem (T.pack (map toLower (T.unpack (model ^. guessText)))) (grabChar colorMatchChars 0) (grabChar colorMatchChars 1) (grabChar colorMatchChars 2) (grabChar colorMatchChars 3) (grabChar colorMatchChars 4)

-- HELPER TO GRAB CHARACTERS OUT FROM GUESS RESULT FOR THE LIST ITEM --
grabChar :: GuessResult -> Int -> ([Char], Int)
grabChar match 0 = match ^. char1
grabChar match 1 = match ^. char2
grabChar match 2 = match ^. char3
grabChar match 3 = match ^. char4
grabChar match 4 = match ^. char5
grabChar match _ = ("", -1)

-- MAIN TO INITIALIZE STARTING STATE, CONFIGURATIONS, THE EVENT HANDLER, THE MODEL, AND START THE APP FROM THIS INFORMATION --
main :: IO ()
main = do
  randomWord <- getRandomWordFromFile wordBankFilePath
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
        { _guessText = "",
          _items = [],
          _colorMatch =
            let char1 = ("", -1)
                char2 = ("", -1)
                char3 = ("", -1)
                char4 = ("", -1)
                char5 = ("", -1)
             in GuessResult char1 char2 char3 char4 char5,
          _target = T.pack randomWord,
          _gameWon = False
        }
