module Grammar where

import qualified Data.Function as Function
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Bifunctor as Bifunctor

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data Symbol = NTerm { value :: String } | Term { value :: String } deriving (Eq, Show)

isTerm (NTerm t) = False
isTerm (Term t) = True

isNTerm = not . isTerm

instance Ord Symbol where
    compare = Function.on compare value

data GrammarRule =
    GrammarRule
        { lhs :: Symbol
        , rhs :: [Symbol] }
    deriving (Eq, Show)

instance Ord GrammarRule where
    compare = Function.on compare (\r -> (lhs r, rhs r))

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


shapeGrammarRules rawRules = List.reverse $ List.foldl (shapeGrammarRules' (Set.fromList $ List.map fst rawRules)) [] rawRules
    where
        shapeGrammarRules' nterms rules raw = GrammarRule {
            lhs = toSymbol $ fst raw,
            rhs = List.map toSymbol $ snd raw
        } : rules
            where
                toSymbol s = if s `Set.member` nterms then NTerm s else Term s


cleanGrammarRules startRule otherRules = Set.toList $ cleanUnreachable $ cleanNonGenerative $ Set.fromList (startRule : otherRules)
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

        cleanUnreachable rules = rules Set.\\ unreachable (Set.singleton $ lhs startRule) rules
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