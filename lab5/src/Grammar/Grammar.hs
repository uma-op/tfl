module Grammar where

import qualified Data.Function as Function
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Maybe as Maybe

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data Symbol = NTerm { value :: String } | Term { value :: String } | End deriving (Eq, Show)

isTerm (NTerm t) = False
isTerm (Term t) = True
isTerm End = True

isNTerm = not . isTerm

instance Ord Symbol where
    compare End End = EQ
    compare End _ = LT
    compare _ End = GT
    compare x y = compare (value x) (value y)

data GrammarRule =
    GrammarRule
        { lhs :: Symbol
        , rhs :: [Symbol] }
    deriving (Eq, Show)

instance Ord GrammarRule where
    compare = Function.on compare (\r -> (lhs r, rhs r))

getSymbols :: (Symbol -> Bool) -> Set.Set GrammarRule -> Set.Set Symbol
getSymbols p = List.foldr (flip (List.foldr (\x -> if p x then Set.insert x else id )) . (\r -> lhs r : rhs r)) Set.empty

getNTerms :: Set.Set GrammarRule -> Set.Set Symbol
getNTerms = getSymbols isNTerm

getTerms :: Set.Set GrammarRule -> Set.Set Symbol
getTerms = getSymbols isTerm

parseGrammarRules = parse parseRules "(unknown)"

parseRules :: GenParser Char st [(String, [String])]
parseRules = endBy parseRule (char '\n')

parseRule :: GenParser Char st (String, [String])
parseRule =
    do
        lhs <- parseSymbol
        spaces >> string "->" >> spaces
        rhs <- sepBy parseSymbol (char ' ')
        return (lhs, rhs)

parseSymbol :: GenParser Char st String
parseSymbol = many (noneOf " \n->")


shapeGrammarRules :: [([Char], [[Char]])] -> (Symbol, Set.Set GrammarRule)
shapeGrammarRules rawRules = (NTerm $ fst $ head rawRules, List.foldr (Set.insert . shapeGrammarRule') Set.empty rawRules)
    where
        nterms = Set.fromList $ List.map fst rawRules
        startNTerm = fst $ head rawRules
        shapeGrammarRule' raw =
            GrammarRule
                { lhs = newLhs
                , rhs = if fst raw == startNTerm then newRhs ++ [End] else newRhs }
            where
                toSymbol s = if s `Set.member` nterms then NTerm s else Term s
                newLhs = toSymbol $ fst raw
                newRhs = List.map toSymbol $ snd raw


cleanGrammarRules startNTerm rules = (startNTerm, cleanUnreachable $ cleanNonGenerative rules)
    where
        cleanNonGenerative rules = rules Set.\\ nonGenerative terms rules
            where
                terms = Set.fold (\r s -> Set.union s $ Set.fromList $ List.filter isTerm $ rhs r) Set.empty rules

                nonGenerative :: Set.Set Symbol -> Set.Set GrammarRule -> Set.Set GrammarRule
                nonGenerative generative unprocessed
                    | Set.null newGenerative = stillNonGenerative
                    | otherwise = nonGenerative (Set.union generative newGenerative) stillNonGenerative
                    where
                        isGenerative rule = List.all (`Set.member` generative) (Grammar.rhs rule)
                        (newGenerative, stillNonGenerative) =
                            Bifunctor.first
                                (Set.map lhs)
                                (Set.partition isGenerative unprocessed)

        cleanUnreachable rules = rules Set.\\ unreachable (Set.singleton startNTerm) rules
            where
                unreachable :: Set.Set Symbol -> Set.Set GrammarRule -> Set.Set GrammarRule
                unreachable reachable unprocessed
                    | Set.null newReachable = stillUnreachable
                    | otherwise = unreachable (Set.union reachable newReachable) stillUnreachable
                    where
                        isReachable rule = lhs rule `Set.member` reachable

                        (newReachable, stillUnreachable) =
                            Bifunctor.first
                                (Set.fold (Set.union . Set.fromList . List.filter isNTerm . rhs) Set.empty)
                                (Set.partition isReachable unprocessed)