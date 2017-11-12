module Files where

import Data.Foldable
import Data.Traversable
import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (error, log, logShow)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn4, Fn3, runFn4, runFn3)
import Types (Async)

foreign import data FS :: Effect

type ErrorCode = String

type FilePath = String

foreign import readFileImpl ::
                 forall eff. Fn3 FilePath
                   (String -> Eff (fs :: FS | eff) Unit)
                   (ErrorCode -> Eff (fs :: FS | eff) Unit)
                   (Eff (fs :: FS | eff) Unit)

foreign import writeFileImpl ::
                 forall eff. Fn4 FilePath
                   String
                   (Eff (fs :: FS | eff) Unit)
                   (ErrorCode -> Eff (fs :: FS | eff) Unit)
                   (Eff (fs :: FS | eff) Unit)

readFile :: forall eff. FilePath -> (Either ErrorCode String -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
readFile path k = runFn3 readFileImpl path (k <<< Right) (k <<< Left)

writeFile :: forall eff. FilePath -> String -> (Either ErrorCode Unit -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
writeFile path text k = runFn4 writeFileImpl path text (k $ Right unit) (k <<< Left)

readFileCont :: forall eff. FilePath -> Async (fs :: FS | eff) (Either ErrorCode String)
readFileCont path = ContT $ readFile path

writeFileCont :: forall eff. FilePath -> String -> Async (fs :: FS | eff) (Either ErrorCode Unit)
writeFileCont path text = ContT $ writeFile path text

copyFileCont :: forall eff. FilePath -> FilePath -> Async (fs :: FS | eff) (Either ErrorCode Unit)
copyFileCont src dst = do
  e <- readFileCont src
  case e of
    Left err -> pure $ Left err
    Right content -> writeFileCont dst content

concatFilesCont :: forall eff. FilePath -> FilePath -> FilePath -> Async (fs :: FS | eff) (Either ErrorCode Unit)
concatFilesCont dst f1 f2 = do
  d1 <- readFileCont f1
  d2 <- readFileCont f2
  handleCases d1 d2
  where
    handleCases (Right c1) (Right c2) = writeFileCont dst (c1 <> c2)
    handleCases (Right c1) (Left err) = pure $ Left err
    handleCases (Left err) (Right c2) = pure $ Left err
    handleCases (Left err1) (Left err2) = pure $ Left (err1 <> " and  " <> err2)

readFileContEx :: forall eff. FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) String
readFileContEx path = ExceptT $ readFileCont path

writeFileContEx :: forall eff. FilePath -> String -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
writeFileContEx path text = ExceptT $ writeFileCont path text

copyFileContEx :: forall eff. FilePath -> FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
copyFileContEx src dest = do
  content <- readFileContEx src
  writeFileContEx dest content

concatFilesContEx :: forall eff. FilePath -> FilePath -> FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
concatFilesContEx dst f1 f2 = do
  c1 <- readFileContEx f1
  c2 <- readFileContEx f2
  writeFileContEx dst (c1 <> c2)

concatenateMany :: forall eff t. Traversable t => t FilePath -> FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
concatenateMany files dst = do
  --contents <- runContT $ runExceptT $ traverse readFileContEx files
  contents <- traverse readFileContEx files
  writeFileContEx dst (foldl (\a b -> a <> b) "" contents)

test = runContT (readFileCont "/tmp/final.txt") (\r -> case r of
                                                    Right s -> log s
                                                    Left e -> log e
                                                )

main = do
  --runContT (copyFileCont "/tmp/1.txt" "/tmp/2.txt") logShow
  --runContT (concatFilesCont "/tmp/1.txt" "/tmp/2.txt" "/tmp/3.txt") logShow
  runContT (runExceptT (copyFileContEx "/tmp/1.txt" "/tmp/2.txt")) logShow
  runContT (runExceptT (concatFilesContEx "/tmp/3.txt" "/tmp/1.txt" "/tmp/2.txt")) logShow
  runContT (runExceptT (concatenateMany ["/tmp/1.txt", "/tmp/2.txt", "/tmp/3.txt"] "/tmp/final.txt")) logShow
