{-|
Module      : Match.Rewrite
Description : Functions for rewriting syntax trees based on patterns.
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functions to rewrite ASTs ('ParsedTree')
based on patterns ('Pattern'). It uses pattern matching to replace
subtrees with new structures.
-}
module PEG.Pattern.Match.Rewrite
    ( replace
    , rewrite
    ) where

import PEG.Pattern.Syntax.Pattern (Pattern(..))
import PEG.Pattern.Syntax.ParsedTree (ParsedTree(..))
import PEG.Pattern.Match.Capture (capture)
import Data.Generics (mkT, everywhere)
import PEG.Pattern.Syntax.Peg (Grammar)

{-|
Substitui uma subárvore em uma AST ('ParsedTree') com base em uma variável ('PatVar').

The 'replace' function checks if a subtree matches a variable and, if so,
replaces the corresponding subtree with the provided subtree.

@since 1.0.0
-}
replace :: Pattern -> ParsedTree -> (Pattern, ParsedTree) -> ParsedTree
replace (PatVar _ name)  t (PatVar _ name', t') = if name == name' then t' else t
replace (PatNT nt p)     (ParsedNT nt' t) subst = if nt == nt'
                                                    then ParsedNT nt' (replace p t subst)
                                                    else ParsedNT nt' t
replace (PatSeq p1 p2)   (ParsedSeq t1 t2)     subst = ParsedSeq (replace p1 t1 subst) (replace p2 t2 subst)
replace (PatSeq p1 p2)   (ParsedIndent t1 t2)  subst = ParsedIndent (replace p1 t1 subst) $ map (\ x -> replace p2 x subst) t2
replace (PatChoice p1 _) (ParsedChoiceLeft t)  subst = ParsedChoiceLeft $ replace p1 t subst
replace (PatChoice _ p2) (ParsedChoiceRight t) subst = ParsedChoiceRight $ replace p2 t subst
replace (PatStar p)      (ParsedStar ts)       subst = ParsedStar $ map (\ x -> replace p x subst) ts
replace _                t                     _     = t

{-|
Rewrites an AST ('ParsedTree') by replacing subtrees that match
a variable ('PatVar') with another pattern.

@since 1.0.0
-}
rewrite :: Grammar -> Pattern -> Pattern -> ParsedTree -> ParsedTree
rewrite g p p' = everywhere $ mkT (\x -> foldr ((flip . replace) p') x (concat (capture g p x)))
