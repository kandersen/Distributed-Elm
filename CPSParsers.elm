--| CPS-Based Parser Combinators
--| Kristoffer Just Andersen
--| kja@cs.au.dk
--| May, 2013
--| Elm Version 0.8

module CPSParsers where
import List
import Maybe

--| Types

data Unit = Unit

--type FailKont a =                               Unit -> Maybe a
--type SuccKont a = a          -> FailKont a -> String -> Maybe a
--type Parser   a = SuccKont a -> FailKont a -> String -> Maybe a

--| Basic Parsers 
--| Juggling continuations is not for the faint of heart

--pUnit : a -> Parser a
pUnit a sk fk xs = sk a fk xs

--pFail : Parser a
pFail sk fk cs = fk Unit

--pAny : Parser Char
pAny sk fk xs = case xs of 
  [] -> fk Unit
  (c :: cs) -> sk c fk xs

--pChar : Char -> Parser Char
pChar c sk fk xs = case xs of
  [] -> fk Unit
  (c' :: cs) -> if c == c' then sk c' fk cs else fk Unit

--pApp : Parser (a -> b) -> Parser a -> Parser b
pApp pa2b pa sk fk xs =
 pa2b (\a2b fk' xs' -> pa (\a fk'' sk'' -> sk (a2b a) fk'' sk'') fk' xs') fk xs

--pMany : Parser a -> Parser [a]
pMany p sk0 fk0 xs0 = 
  let fk1 = (\foo -> sk0 [] fk0 xs0)
      sk1 = (\x fk1 xs1 ->
      let sk2 = (\xs fk2 xs2 -> sk0 (x::xs) fk2 xs2) in pMany p sk2 fk1 xs1) in
  p sk1 fk1 xs0
  
--pAlt : Parser a -> Parser a -> Parser a
pAlt pa1 pa2 sk fk xs =
 pa1 sk (\_ -> pa2 sk fk xs) xs

--| Applicative Interface
returnFirst a b = a

--liftP : (a -> b) -> Parser (a -> b)
liftP f = pApp (pUnit f)

p <*> r = pApp p r
f <$> p = liftP f p
p <|> r = pAlt p r
f <$ p = (returnFirst <$> (pUnit f)) <*> p
p <* q = (returnFirst <$> p) <*> q
p *> q = (id <$ p) <*> q

--| Derived Parsers
 
--pOpt : Parser a -> a -> Parser a
pOpt p v = pAlt p (pUnit v)

--pMany1 : Parser a -> Parser [a]
pMany1 p = 
  ((::) <$> p) <*> (pMany p)

--pChoice : [Parser a] -> Parser a
pChoice = foldr (<|>) pFail 

--pOneOf : String -> Parser Char
pOneOf = pChoice . (map pChar)

--pDigit : Parser Char
pDigit = pOneOf "0123456789"

--digitToInt : Char -> Int
digitToInt c = 
  if | c == '0' -> 0
     | c == '1' -> 1
     | c == '2' -> 2
     | c == '3' -> 3
     | c == '4' -> 4
     | c == '5' -> 5
     | c == '6' -> 6
     | c == '7' -> 7
     | c == '8' -> 8
     | c == '9' -> 9
     | otherwise -> 0

--pCounting : Parser Int
pCounting = digitToInt <$> pDigit

--pNatural : Parser Int
pNatural = (foldl (\b a -> a * 10 + b) 0) <$> (pMany1 pCounting)

--pInteger : Parser Int
pInteger = (pOpt ((\x -> 0 - x) <$ (pChar '-')) id) <*> pNatural

--pParens : Parser a -> Parser a
pParens p = ((pChar '(') *> p) <* (pChar ')')

--pComma : Parser Unit
pComma = (pUnit ()) <* (pChar ',')

--pPair : Parser a -> Parser b -> Parser (a, b)
pPair a b = pParens ((((,) <$> a) <* pComma) <*> b)

--| External Execution Interface

--| Standard, left-most, greedy parse
--parseMaybe : Parser a -> String -> Maybe a
parseMaybe p = p (\x _ _ -> Just x) (\_ -> Nothing)

--| Returns all possibilites for ambiguous parsers
--parseList : Parser a -> String -> [a]
parseList p = p (\x fk _ -> x :: (fk Unit)) (\_ -> [])

--| Decider for the language represented by the parser; i.e.,
--| does there exists a _possible_ parse
--recognize : Parser a -> String -> Bool
recognize p = p (\_ _ xs -> xs == [] || (fk Unit)) (\_ -> false)

--| Count the number of possible parses
--ambiguity : Parser a -> String -> Int
ambiguity p = p (\_ fk _ -> 1 + (fk Unit)) (\_ -> 0)