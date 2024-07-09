{-# LANGUAGE QuasiQuotes #-} -- for help function

module CLParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.RawString.QQ

import Control.Monad (void)


type Name = String

data Command
  = AddActivity Name
  | RemoveActivity Name
  | ActivityDone Name
  | RenameActivity Name Name
  | InfoActivity Name
  | Help


data ParseError
  = ActivityDoesNotExist Name


lexer :: P.TokenParser a
lexer = P.makeTokenParser haskellDef

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer


parseAddActivity :: Parser Command
parseAddActivity = do
  void $ lexeme $ string "add"
  name <- stringLiteral
  return $ AddActivity name


parseRemoveActivity :: Parser Command
parseRemoveActivity = do
  void $ lexeme $ string "remove"
  name <- stringLiteral
  return $ RemoveActivity name


parseActivityDone :: Parser Command
parseActivityDone = do
  void $ lexeme $ string "done"
  name <- stringLiteral
  return $ ActivityDone name



parseRenameActivity = do
  void $ lexeme $ string "done"
  oldName <- stringLiteral
  newName <- stringLiteral
  return $ RenameActivity oldName newName


parseInfoActivity :: Parser Command
parseInfoActivity = do
  void $ lexeme $ string "info"
  name <- stringLiteral
  return $ InfoActivity name


parseHelp :: Parser Command
parseHelp = do
  void $ lexeme $ string "help"
  return Help


parseCommand :: Parser Command
parseCommand = choice [parseAddActivity, parseRemoveActivity, parseActivityDone, parseRenameActivity, parseInfoActivity, parseHelp]


parseConfirmation :: Parser Bool
parseConfirmation = do
  answer <- choice $ lexeme <$> char <$> ['y', 'n']
  case answer of
    'y' ->  return True
    'f' -> return False



confirmation :: IO Bool
confirmation = do
  putStrLn "are you sure? (y/n)"
  answer <- getLine
  case parse parseConfirmation "" answer of
    Left _ -> confirmation
    Right c -> return c


help :: String
help = [r|all command availlable :
          - add "name" : add an activity named "name"
          - remove "name" : remove the activity named "name"
          - done "name" : to achieve an activity
          - rename "oldName" "newName" : to rename an activity from "oldName" to "newName"
          - info "name" : info about the activity "name"
          note that activity's name support spaces|]
