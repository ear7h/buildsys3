{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Applicative (optional)
import Control.Exception (throwIO)
import Control.Monad (when)

import Data.List (intersperse)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text, pack)

import System.Exit (exitFailure)
import System.IO.Error  (catchIOError, isDoesNotExistError, ioeGetFileName)
import System.Exit (ExitCode(ExitSuccess))

import GHC.IO.Handle (hPutStr, hClose)
import GHC.Generics

import Options.Applicative hiding (ParseError, auto)

import System.FilePath.Posix ((</>), takeDirectory)

import Control.Monad.Except

import System.Process (spawnProcess, waitForProcess)

import System.Posix.Directory (getWorkingDirectory, changeWorkingDirectory)
import System.Posix.Files (fileExist, modificationTime, getFileStatus)
import System.Posix.Types (EpochTime)
import System.Posix.Time  (epochTime)

import Dhall

data Command = MkCommand
    { cmd :: String
    , args :: [String]
    }
    deriving (Generic, Show)

instance FromDhall Command

prettyCommand :: Command -> String
prettyCommand (MkCommand cmd args) = cmd ++ (foldr f "" args)
    where f el acc = " " ++ el ++ acc

data Item
    = Generated
        { results :: [String]
        , command :: Command
        , prereqs :: [Item]
        }
    | Source String
    deriving (Show)


data Error
    = MissingSource String
    | NotGenerated String
    | CommandFailure ExitCode
    deriving (Show)

data Options = MkOptions
    { attr :: String
    , isExpr :: Bool
    , dryRun :: Bool
    -- TODO(ear7h): change to glob pattern/regex?
    , force :: Bool
    , chdir :: Maybe String
    }
    deriving (Show)

defaultOptions :: Options
defaultOptions = MkOptions
    { attr = "default"
    , isExpr = False
    , dryRun = False
    , force = False
    , chdir = Nothing
    }

mtime :: FilePath -> IO EpochTime
mtime = fmap modificationTime . getFileStatus

catchNotExists :: IO a -> (IOError -> IO a) -> IO a
catchNotExists a f =
    catchIOError
        a
        (\e -> do
            if isDoesNotExistError e
            then f e
            else ioError e
        )

type ExceptIO = ExceptT Error IO

build :: Options -> Item -> ExceptIO EpochTime
build options (Source file) =
    ExceptT $ catchNotExists
        (Right <$> mtime file)
        (const $ return $ Left $ MissingSource file );

build options (Generated results command prereqs) = do
    pTimes <- mapM buildRec prereqs
    aTimes <- liftIO $
        catchNotExists
            (mapM mtime results)
            (const $ return [])
    let
        pTime = foldl1May max pTimes
        aTime = foldl1May min aTimes
    if Nothing == aTime || pTime > aTime || (force options)
    then do
        liftIO $ putStrLn $ prettyCommand command
        newTimes <-
            if dryRun options
            then return []
            else do
                runInstr
                ExceptT $ catchNotExists
                    (Right <$> mapM mtime results)
                    (return . Left . MissingSource . fromJust . ioeGetFileName)
        now <- liftIO epochTime
        return $ fromMaybe now $ foldl1May max newTimes
    else
        return $ fromJust $ foldl1May max aTimes
    where
        buildRec :: Item -> ExceptIO EpochTime
        buildRec = build options

        runInstr :: ExceptIO ()
        runInstr = do
            handle <- liftIO $ spawnProcess (cmd command) (args command)
            exitCode <- liftIO $ waitForProcess handle
            case exitCode of
                ExitSuccess -> return ()
                exitFailure -> throwError $ CommandFailure exitFailure

foldl1May :: Foldable t => (a -> a -> a) -> t a -> Maybe a
foldl1May f = foldl (flip $ noNothing f . Just) Nothing
    where
        noNothing f Nothing x = x
        noNothing f x Nothing = x
        noNothing f (Just x) (Just y) = Just $ f x y

data Token
    = TOpen
    | TClose
    | TGenerated [String] Command
    | TSource String
    deriving (Show, Generic)

instance FromDhall Token

data ParseError
    = Unexpected Token String
    | Unparsed  [Token]

instance Show ParseError where
    show (Unexpected got exp) =
        "unexpected token " ++ (show got) ++ ", expected " ++ exp
    show (Unparsed tokens) =
        "unparsed tokens " ++ (show tokens)

parse :: [Token] -> Either ParseError (Item, [Token])
parse ((TSource s):rest) = Right $ (Source s, rest)
parse ((TGenerated a i):rest) = do
    (items, tokens) <- parsePrereqs rest
    Right $ (Generated a i items, tokens)
parse (x:_) = Left $ Unexpected x "TGenerated"

parsePrereqs :: [Token] -> Either ParseError ([Item], [Token])
parsePrereqs (TOpen:rest) = go rest
    where
        go :: [Token] -> Either ParseError ([Item], [Token])
        go (TClose:rest) = return ([], rest)
        go tokens = do
            (x, rest) <- parse tokens
            (xs, rest) <- go rest
            return (x:xs, rest)
parsePrereqs (x:_) = Left $ Unexpected  x "TOpen"

parseTokens :: [Token] -> Either ParseError Item
parseTokens tokens = do
    (item, rest) <- parse tokens
    case rest of
        []   -> Right item
        rest -> Left (Unparsed rest)

searchDir :: IO (Maybe FilePath)
searchDir = do
    dir <- getWorkingDirectory
    go dir
    where
        go :: FilePath -> IO (Maybe FilePath)
        go "/" = return Nothing
        go "" = undefined
        go dir = do
            exist <- fileExist $ dir </> "buildsys.dhall"
            if exist
            then return $ Just dir
            else go (takeDirectory dir)

buildDhall :: Options -> IO ()
buildDhall opt = do
    putStrLn (show opt)
    case (chdir opt, isExpr opt) of
        (Nothing, True) -> return ()
        (Nothing, False) -> do
            dir <- searchDir
            case dir of
                Nothing -> do
                    putStrLn "could not find directory with buildsys.dhall"
                    exitFailure
                Just dir' -> do
                    putStrLn $ "changing directory: " ++ dir'
                    changeWorkingDirectory dir'
        (Just s, _) -> do
            putStrLn $ "changing directory: " ++ s
            changeWorkingDirectory s
    let s =
            if isExpr opt
            then attr opt
            else "(./buildsys.dhall).`" ++ (attr opt) ++ "`"
    tokens <- input auto (pack s)
    item <- case parse tokens of
        Right (item, []) -> return item
        Right (_, rest) -> do
            putStrLn "invalid tokens, are you using the buildsys prelude?"
            exitFailure
        Left err -> fail (show err)
    res <- runExceptT $ build opt item
    case res of
        Right _ -> return ()
        Left x -> putStrLn (show x)

optionsParse :: Parser Options
optionsParse = MkOptions
    <$> strArgument
        ( value "default"
        <> showDefault
        <> metavar "ATTR" )
    <*> switch
        ( long "expr"
        <> short 'e'
        <> help "When set, ATTR is intepreted as an expression" )
    <*> switch
        ( long "dry-run"
        <> short 'd'
        <> help "Don't run build commands" )
    <*> switch
        ( long "force"
        <> short 'f'
        <> help "Force run build commands" )
    <*> (optional $ strOption $
        long "chdir"
        <> short 'c'
        <> help "Change directory before doing building" )

main :: IO ()
main = (execParser opts) >>= buildDhall
    where
        opts = info (optionsParse <**> helper)
            (fullDesc
            <> progDesc "Build from commands defined in a dhall file"
            <> header "buildsys - a build system ")
