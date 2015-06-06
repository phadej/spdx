{-# LANGUAGE CPP #-}
module Data.SPDX.Parser (parseExpression, unsafeParseExpr) where

#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative
#endif

import Data.Char
import Text.ParserCombinators.ReadP

import Data.SPDX.Types
import Data.SPDX.Licenses (licenseIdentifiers, licenseExceptions)

license :: ReadP LicenseId
license = choice (map string licenseIdentifiers)

licenseException :: ReadP LicenseExceptionId
licenseException = choice (map string licenseExceptions)

elicense :: ReadP LicenseExpression
elicense = (\l -> ELicense False (Right l) Nothing) <$> license

elicenseWith :: ReadP LicenseExpression
elicenseWith = (\l e -> ELicense False (Right l) (Just e)) <$> license <* skipSpaces1 <* string "WITH" <* skipSpaces1 <*> licenseException

elicenseAndNewer :: ReadP LicenseExpression
elicenseAndNewer = (\l -> ELicense True (Right l) Nothing) <$> license <* char '+'

elicenseWithAndNewer :: ReadP LicenseExpression
elicenseWithAndNewer = (\l e -> ELicense True (Right l) (Just e)) <$> license <* char '+' <* string " WITH " <*> licenseException

elicenseRef :: ReadP LicenseExpression
elicenseRef = (\licId -> ELicense False (Left $ LicenseRef Nothing licId) Nothing) <$ string "LicenseRef-" <*> idString

elicenseDocRef :: ReadP LicenseExpression
elicenseDocRef = f <$ string "DocumentRef-" <*> idString <* char ':' <* string "LicenseRef-" <*> idString
  where f docId licId = ELicense False (Left $ LicenseRef (Just docId) licId) Nothing

idString :: ReadP String
idString = munch1 p
  where p '.' = True
        p '-' = True
        p c   = isAlphaNum c

skipSpaces1 :: ReadP ()
skipSpaces1 = () <$ char ' ' <* skipSpaces

parens :: ReadP a -> ReadP a
parens = between (char '(') (skipSpaces <* char ')')

terminal :: ReadP LicenseExpression
terminal = choice [ elicense
                  , elicenseWith
                  , elicenseAndNewer
                  , elicenseWithAndNewer
                  , elicenseRef
                  , elicenseDocRef
                  , parens expression
                  ]

conjunction :: ReadP LicenseExpression
conjunction = chainr1 terminal (EConjunction <$ skipSpaces1 <* string "AND" <* skipSpaces1)

disjunction :: ReadP LicenseExpression
disjunction = chainr1 conjunction (EDisjunction <$ skipSpaces1 <* string "OR" <* skipSpaces1)

expression :: ReadP LicenseExpression
expression = skipSpaces *> disjunction

-- | Parse SPDX License Expression
--
-- >>> parseExpression "LGPL-2.1 OR MIT"
-- [EDisjunction (ELicense False "LGPL-2.1" Nothing) (ELicense False "MIT" Nothing)]
parseExpression :: String -> [LicenseExpression]
parseExpression = map fst . readP_to_S (expression <* skipSpaces <* eof)

unsafeParseExpr :: String -> LicenseExpression
unsafeParseExpr s = f . parseExpression $ s
  where f []      = error $ "Failed parse of license expression: " ++ s
        f [l]     = l
        f (_:_:_) = error $ "Ambigious parse of license expression: " ++ s
