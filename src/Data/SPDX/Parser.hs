{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}
module Data.SPDX.Parser (parseExpression, unsafeParseExpr) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

import Control.Applicative
import Data.Char
import Text.ParserCombinators.ReadP

import Data.SPDX.Types
import Data.SPDX.Licenses (licenseIdentifiers, licenseExceptions)

#if !MIN_VERSION_base(4,6,0)
import Control.Monad

instance Applicative ReadP where
  pure = return
  (<*>) = ap

instance Alternative ReadP where
  empty = mzero
  (<|>) = mplus
#endif

license :: ReadP LicenseId
license = choice (map f licenseIdentifiers)
  where f l = l <$ string (getLicenseId l)

licenseException :: ReadP LicenseExceptionId
licenseException = choice (map f licenseExceptions)
  where f l = l <$ string (getLicenseExceptionId l)

licenseRef :: ReadP LicenseRef
licenseRef = l <|> d
  where l = LicenseRef Nothing <$ string "LicenseRef-" <*> idString
        d = (\docId licId -> LicenseRef (Just docId) licId) <$ string "DocumentRef-" <*> idString <* char ':' <* string "LicenseRef-" <*> idString

mkLicense ::  ReadP (Either LicenseRef LicenseId) -> ReadP LicenseExpression
mkLicense p = choice
  [ (\l   -> ELicense False l Nothing)  <$> p
  , (\l e -> ELicense False l (Just e)) <$> p <* skipSpaces1 <* string "WITH" <* skipSpaces1 <*> licenseException
  , (\l   -> ELicense True  l Nothing)  <$> p <* char '+'
  , (\l e -> ELicense True  l (Just e)) <$> p <* char '+' <* string " WITH " <*> licenseException
  ]

elicense :: ReadP LicenseExpression
elicense = mkLicense (Right <$> license)

elicenseRef :: ReadP LicenseExpression
elicenseRef = mkLicense (Left <$> licenseRef)

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
                  , elicenseRef
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
-- [EDisjunction (ELicense False (Right (LicenseId "LGPL-2.1")) Nothing) (ELicense False (Right (LicenseId "MIT")) Nothing)]
parseExpression :: String -> [LicenseExpression]
parseExpression = map fst . readP_to_S (expression <* skipSpaces <* eof)

unsafeParseExpr :: String -> LicenseExpression
unsafeParseExpr s = f . parseExpression $ s
  where f []      = error $ "Failed parse of license expression: " ++ s
        f [l]     = l
        f (_:_:_) = error $ "Ambigious parse of license expression: " ++ s
