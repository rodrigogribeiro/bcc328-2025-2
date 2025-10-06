{-|
Module      : Pipeline.MatchPipeline
Description : Functions for processing grammars, patterns, and ASTs.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functions for processing PEGs, patterns, and ASTs.
It includes grammar and pattern validation, pattern matching, subtree capturing, and AST rewriting.
It also provides auxiliary functions for file input and output.
-}
module PEG.Pattern.Pipeline.MatchPipeline (module PEG.Pattern.Pipeline.MatchPipeline) where

import PEG.Pattern.Syntax.Base (Pretty(pPrint))
import PEG.Pattern.Syntax.Peg (Grammar)
import PEG.Pattern.Syntax.Pattern (NamedSynPat, NamedPattern, Pattern (PatVar))
import PEG.Pattern.Syntax.ParsedTree (ParsedTree, flatten)
import PEG.Pattern.Parser.Base (parseWith)
import PEG.Pattern.Parser.Peg (parseGrammar)
import PEG.Pattern.Parser.Pattern (parsePatterns)
import PEG.Pattern.Parser.ParsedTree (mkParser)
import PEG.Pattern.Semantic.Peg (processPeg)
import PEG.Pattern.Semantic.Pattern (validPat, correctPat, processPats)
import PEG.Pattern.Match.Capture (match, capture)
import PEG.Pattern.Match.Rewrite (rewrite)
import Text.Megaparsec (errorBundlePretty)
import Data.Bifunctor (Bifunctor(first, bimap, second))
import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import Data.List (nub)

type PrettyError = String

{-|
Validates a PEG from an input string.

Returns the processed grammar or a formatted error.

@since 1.0.0
-}
parseValidGrammar :: String -> Either PrettyError Grammar
parseValidGrammar contents =
    case parseGrammar contents of
        Left e -> Left $ errorBundlePretty e
        Right g -> first (show . pPrint) (processPeg g)

{-|
Validates syntactic patterns against a PEG.

Receives the grammar and pattern contents as strings and returns the processed patterns
or a formatted error.

@since 1.0.0
-}
parseValidPatterns :: String -> String -> Either PrettyError [NamedPattern]
parseValidPatterns contentsG contentsP =
    case (g, ps) of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right g', Right ps') -> first (show . pPrint) (processPats g' ps')
    where
        g = parseValidGrammar contentsG
        ps = first errorBundlePretty (parsePatterns contentsP)

{-|
Corrects syntactic patterns against a PEG.

Receives the grammar and pattern contents as strings and returns the corrected patterns
or a formatted error.

@since 1.0.0
-}
parseCorrectPatterns :: String -> String -> Either PrettyError [NamedPattern]
parseCorrectPatterns contentsG contentsP =
    case (g, ps) of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right g', Right ps') -> bimap (show . pPrint) (correct g') (processPats g' ps')
    where
        g = parseValidGrammar contentsG
        ps = first errorBundlePretty (parsePatterns contentsP)
        mkProof g' (n, p) ps' = maybe ps' ((:ps') . (n,)) (correctPat p =<< validPat g' p)
        correct g' = foldr (mkProof g') []

{-|
Parses an input file based on a PEG.

Receives the grammar and file contents as strings and returns the AST
or a formatted error.

@since 1.0.0
-}
parseFile :: String -> String -> Either PrettyError ParsedTree
parseFile contentsG contentsF =
    case parseValidGrammar contentsG of
        Left e -> Left e
        Right g -> first errorBundlePretty (pFile g)
    where
        pFile g = parseWith (mkParser g) contentsF

{-|
Checks pattern matching in an AST.

Receives the grammar, pattern, and file contents as strings and returns a list
indicating whether each pattern matches the tree.

@since 1.0.0
-}
parseMatch :: String -> String -> String -> Either PrettyError [(String, Bool)]
parseMatch contentsG contentsP contentsF =
    case (g, ps, f) of
        (Left e, _, _) -> Left e
        (_, Left e, _) -> Left e
        (_, _, Left e) -> Left e
        (Right g', Right ps', Right f') -> Right $ map (match' g' f') ps'
    where
        g = parseValidGrammar contentsG
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF
        match' g' f' (n, p) = (n, PEG.Pattern.Match.Capture.match g' p f')

{-|
Checks whether a specific pattern matches an AST.

Receives the grammar, pattern, file contents, and the pattern name as strings.
Returns `True` if the pattern matches the tree, or `False` otherwise.

@since 1.0.0
-}
parseMatch1 :: String -> String -> String -> String -> Either PrettyError Bool
parseMatch1 contentsG contentsP contentsF name =
    case (g, ps, f) of
        (Left e, _, _) -> Left e
        (_, Left e, _) -> Left e
        (_, _, Left e) -> Left e
        (Right g', Right ps', Right f') ->
            case find ((name ==) . fst) ps' of
                Nothing -> Left "Pattern not found in the file"
                Just (_, p) -> Right $ PEG.Pattern.Match.Capture.match g' p f'
    where
        g = parseValidGrammar contentsG
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF

{-|
Captures subtrees matching patterns in an AST.

Receives the grammar, pattern, and file contents as strings and returns a list
of captures for each pattern.

@since 1.0.0
-}
parseCapture :: String -> String -> String -> Either PrettyError [(String, [[(Pattern, ParsedTree)]])]
parseCapture contentsG contentsP contentsF =
    case (g, ps, f) of
        (Left e, _, _) -> Left e
        (_, Left e, _) -> Left e
        (_, _, Left e) -> Left e
        (Right g', Right ps', Right f') -> Right $ map (capture' g' f') ps'
    where
        g = parseValidGrammar contentsG
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF
        capture' g' f' (n, p) = (n, capture g' p f')

{-|
Captures subtrees matching a specific pattern in an AST.

Receives the grammar, pattern, file contents, and the pattern name as strings.
Returns the captures for the specified pattern.

@since 1.0.0
-}
parseCapture1 :: String -> String -> String -> String -> Either PrettyError [[(Pattern, ParsedTree)]]
parseCapture1 contentsG contentsP contentsF name =
    case (g, ps, f) of
        (Left e, _, _) -> Left e
        (_, Left e, _) -> Left e
        (_, _, Left e) -> Left e
        (Right g', Right ps', Right f') ->
            case find ((name ==) . fst) ps' of
                Nothing -> Left "Pattern not found in the file"
                Just (_, p) -> Right $ capture g' p f'
    where
        g = parseValidGrammar contentsG
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF

{-|
Rewrites an AST based on two patterns.

Receives the grammar, pattern, file contents, and the names of the two patterns as strings.
Returns the rewritten tree.

@since 1.0.0
-}
parseRewrite :: String -> String -> String -> String -> String -> Either PrettyError ParsedTree
parseRewrite contentsG contentsP contentsF name1 name2 =
    case (g, ps, f) of
        (Left e, _, _) -> Left e
        (_, Left e, _) -> Left e
        (_, _, Left e) -> Left e
        (Right g', Right ps', Right f') ->
            case (findPat name1 ps', findPat name2 ps') of
                (Nothing, _) -> Left $ "Pattern " ++ name1 ++ " not found in the file"
                (_, Nothing) -> Left $ "Pattern " ++ name2 ++ " not found in the file"
                (Just p1, Just p2) -> Right $ rewrite g' p1 p2 f'
    where
        findPat p = fmap snd . find ((p ==) . fst)
        g = parseValidGrammar contentsG
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF

{-|
Generates a call graph from a given file.

Receives the grammar, patterns, file contents, and the names of the two patterns as strings.
Returns a list of pairs (caller, callee).

@since 1.0.0
-}
parseCallGraph :: String -> String -> String -> String -> String -> Either PrettyError [(ParsedTree, ParsedTree)]
parseCallGraph contentsG contentsP contentsF defPat callPat =
    case (g, ps, f) of
        (Left e, _, _) -> Left e
        (_, Left e, _) -> Left e
        (_, _, Left e) -> Left e
        (Right g', Right ps', Right f') ->
            case (findPat defPat ps', findPat callPat ps') of
                (Nothing, _) -> Left $ "Pattern " ++ defPat ++ " not found in the file"
                (_, Nothing) -> Left $ "Pattern " ++ callPat ++ " not found in the file"
                (Just def, Just call) -> do
                    let definitions = capture g' def f'
                    let pairs = mapMaybe getDef definitions
                    let calls = map (second (mapMaybe getCall . capture g' call)) pairs
                    return $ concatMap (\ (x, y) -> map (x,) y) calls
    where
        findPat p = (snd <$>) . find ((p ==) . fst)
        g = parseValidGrammar contentsG
        ps = parseCorrectPatterns contentsG contentsP
        f = parseFile contentsG contentsF
        findPatTree p = (snd <$>) . find (isVar p)
        isVar p (PatVar _ n, _) = n == p
        isVar _ _ = False
        getCall = findPatTree "name"
        getDef xs = (,) <$> findPatTree "name" xs <*> findPatTree "block" xs


-------------------------------------------------------------------------------
--- IO

{-|
Parses and prints a PEG from a file.

@since 1.0.0
-}
parseGrammarIO :: FilePath -> IO ()
parseGrammarIO f = do
    contents <- readFile f
    case parseGrammar contents of
        Left e -> putStrLn (errorBundlePretty e)
        Right g -> print $ pPrint g

{-|
Validates and prints a PEG from a file.

@since 1.0.0
-}
parseValidGrammarIO :: FilePath -> IO ()
parseValidGrammarIO f = do
    contents <- readFile f
    let g = parseValidGrammar contents
    case g of
        Left e -> putStrLn e
        Right g' -> print $ pPrint g'
        -- Right g' -> print g'

{-|
Parses and prints syntactic patterns from a file.

@since 1.0.0
-}
parsePatternsIO :: FilePath -> IO ()
parsePatternsIO f = do
    contents <- readFile f
    case parsePatterns contents of
        Left e -> putStrLn (errorBundlePretty e)
        Right g -> print $ pPrint g
        -- Right g -> print g

{-|
Applies a function to syntactic patterns read from a file and prints the result.

@since 1.0.0
-}
parsePatApply :: Show a => ([NamedSynPat] -> a) -> FilePath -> IO ()
parsePatApply g f = do
    contents <- readFile f
    case parsePatterns contents of
        Left bundle -> print (errorBundlePretty bundle)
        Right xs -> print (g xs)

{-|
Validates and prints syntactic patterns against a PEG from files.

@since 1.0.0
-}
parseValidPatternsIO :: FilePath -> FilePath -> IO ()
parseValidPatternsIO pathGrammar pathPattern = do
    contentsG <- readFile pathGrammar
    contentsP <- readFile pathPattern
    case parseValidPatterns contentsG contentsP of
        Left e -> putStrLn e
        Right ps' -> print $ pPrint ps'

{-|
Corrects and prints syntactic patterns against a PEG from files.

@since 1.0.0
-}
parseCorrectPatternsIO :: FilePath -> FilePath -> IO ()
parseCorrectPatternsIO pathGrammar pathPattern = do
    contentsG <- readFile pathGrammar
    contentsP <- readFile pathPattern
    case parseCorrectPatterns contentsG contentsP of
        Left e -> putStrLn e
        Right ps' -> print $ pPrint ps'

{-|
Parses and prints an AST from files.

@since 1.0.0
-}
parseFileIO :: FilePath -> FilePath -> Bool -> IO ()
parseFileIO grammarFile inputFile flat = do
    contentsG <- readFile grammarFile
    contentsF <- readFile inputFile
    case parseFile contentsG contentsF of
        Left e -> putStrLn e
        Right t -> putStrLn $ if flat then flatten t else show (pPrint t)

{-|
Checks pattern matching in an AST and prints the results.

@since 1.0.0
-}
parseMatchIO :: FilePath -> FilePath -> FilePath -> IO ()
parseMatchIO grammarFile patternFile inputFile = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseMatch contentsG contentsP contentsF of
        Left e -> putStrLn e
        Right ms -> putStr $ concatMap message ms
    where
        message (n, b) = n ++ (if b then ": match!" else ": not match!") ++ "\n"

{-|
Checks whether a specific pattern matches an AST and prints the result.

@since 1.0.0
-}
parseMatch1IO :: FilePath -> FilePath -> FilePath -> String -> IO ()
parseMatch1IO grammarFile patternFile inputFile pat = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseMatch1 contentsG contentsP contentsF pat of
        Left e -> putStrLn e
        Right b -> putStrLn $ pat ++ (if b then ": match!" else ": not match!")

{-|
Captures subtrees matching patterns in an AST and prints the results.

@since 1.0.0
-}
parseCaptureIO :: FilePath -> FilePath -> FilePath -> IO ()
parseCaptureIO grammarFile patternFile inputFile = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseCapture contentsG contentsP contentsF of
        Left e -> putStrLn e
        Right ms -> putStr $ concatMap message ms
    where
        message (n, c) = "pattern " ++ n ++ ":\n" ++ concatMap printCaptures c ++ "\n"
        printCaptures xs = concatMap printCapture xs ++ "\n"
        printCapture (p, t) = show (pPrint p) ++ ":\n" ++ flatten t ++ "\n"

{-|
Captures subtrees matching a specific pattern in an AST and prints the results.

@since 1.0.0
-}
parseCapture1IO :: FilePath -> FilePath -> FilePath -> String -> IO ()
parseCapture1IO grammarFile patternFile inputFile pat = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseCapture1 contentsG contentsP contentsF pat of
        Left e -> putStrLn e
        Right m -> putStrLn $ concatMap printCaptures m ++ "\n"
    where
        printCaptures xs = concatMap printCapture xs ++ "\n"
        printCapture (p, t) = show (pPrint p) ++ ":\n" ++ flatten t ++ "\n"

{-|
Rewrites an AST based on two patterns and prints the result.

@since 1.0.0
-}
parseRewriteIO :: FilePath -> FilePath -> FilePath -> String -> String -> IO ()
parseRewriteIO grammarFile patternFile inputFile pat1 pat2 = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseRewrite contentsG contentsP contentsF pat1 pat2 of
        Left e -> putStrLn e
        Right t -> putStrLn $ flatten t

{-|
Generates a call graph from a given file and prints the result.

@since 1.0.0
-}
parseCallGraphIO :: FilePath -> FilePath -> FilePath -> String -> String -> IO ()
parseCallGraphIO grammarFile patternFile inputFile pat1 pat2 = do
    contentsG <- readFile grammarFile
    contentsP <- readFile patternFile
    contentsF <- readFile inputFile
    case parseCallGraph contentsG contentsP contentsF pat1 pat2 of
        Left e -> putStrLn e
        Right t -> putStrLn $ concat . nub $ map (\ (x, y) -> flatten x ++ " -> " ++ flatten y ++ "\n") t
