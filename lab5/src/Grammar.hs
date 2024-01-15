module Grammar where

import qualified Data.Function as Function
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Bifunctor as Bifunctor

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data Symbol = NTerm { value :: String } | Term { value :: String } deriving Eq

isTerm (NTerm t) = False
isTerm (Term t) = True

isNTerm = not . isTerm

instance Ord Symbol where
    compare = Function.on compare value

data GrammarRule =
    GrammarRule
        { lhs :: Symbol
        , rhs :: [Symbol] }
    deriving Eq

instance Ord GrammarRule where
    compare = Function.on compare (\r -> (lhs r, rhs r))

whitespaces = skipMany $ oneOf " \n"

parseRules :: GenParser Char st [(String, [String])]
parseRules = endBy parseRule endOfLine

parseRule :: GenParser Char st (String, [String])
parseRule =
    do
        whitespaces
        lhs <- parseSymbol
        spaces
        string "->"
        spaces
        rhs <- sepBy parseSymbol $ many $ noneOf " \n"
        return (lhs, rhs)

parseSymbol :: GenParser Char st String
parseSymbol = many $ noneOf " \n"


parseGrammarRules rawRules = parseGrammarRules' (Set.fromList $ List.map fst rawRules) [] rawRules
    where
        parseGrammarRules' nterms rules [] = rules
        parseGrammarRules' nterms rules raw =
            parseGrammarRules'
                nterms
                (Bifunctor.bimap
                    NTerm
                    (List.map (\x -> if x `List.elem` nterms then NTerm x else Term x))
                    (head raw) : rules)
                (tail raw)

cleanGrammarRules startRule otherRules = cleanUnreachable $ cleanNonGenerative (Set.fromList (startRule : otherRules))
    where
        cleanNonGenerative rules = rules Set.\\ nonGenerative terms rules
            where
                terms = Set.fold (\r s -> Set.union s $ Set.fromList $ List.filter isTerm $ rhs r) Set.empty rules

                nonGenerative :: Set.Set Symbol -> Set.Set GrammarRule -> Set.Set GrammarRule
                nonGenerative generative unprocessed
                    | Set.null newGenerative = unprocessed
                    | otherwise = nonGenerative (Set.union generative newGenerative) stillNonGenerative
                    where
                        isGenerative rule = List.any (`Set.member` generative) (Grammar.rhs rule)
                        (newGenerative, stillNonGenerative) =
                            Bifunctor.first
                                (Set.map lhs)
                                (Set.partition isGenerative unprocessed)

        cleanUnreachable rules = rules Set.\\ unreachable (Set.singleton $ lhs startRule) rules
            where
                unreachable :: Set.Set Symbol -> Set.Set GrammarRule -> Set.Set GrammarRule
                unreachable reachable unprocessed
                    | Set.null newReachable = unprocessed
                    | otherwise = unreachable (Set.union reachable newReachable) stillUnreachable
                    where
                        isReachable rule = lhs rule `Set.member` reachable


                        (newReachable, stillUnreachable) =
                            Bifunctor.first
                                (Set.fold (Set.union . Set.fromList . List.filter isNTerm . rhs) Set.empty)
                                (Set.partition isReachable unprocessed)