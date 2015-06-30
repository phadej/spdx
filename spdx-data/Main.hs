module Main (main) where

import Data.List
import Data.Maybe
import Data.Traversable
import Text.XML.Light

simplify :: Element -> [[[String]]]
simplify el =
  let worksheets = filter isWorksheetElement $ childrenElements el
      Just tables = traverse tableFromWorksheet worksheets
  in map tableElements tables

toElem :: Content -> Maybe Element
toElem (Elem e) = Just e
toElem _        = Nothing

isElement :: String -> Element -> Bool
isElement name = (name ==) . qName . elName

isWorksheetElement :: Element -> Bool
isWorksheetElement = isElement "Worksheet"

childrenElements :: Element -> [Element]
childrenElements = mapMaybe toElem . elContent

tableFromWorksheet :: Element -> Maybe Element
tableFromWorksheet = listToMaybe . filter (isElement "Table") . childrenElements

textContent :: Content -> Maybe String
textContent (Text t) = Just $ cdData t
textContent _        = Nothing

extractData :: Element -> String
extractData el = maybe "" id $ do dataEl <- listToMaybe . filter (isElement "Data") . childrenElements $ el
                                  textEl <- listToMaybe $ elContent dataEl
                                  text <- textContent textEl
                                  return text

tableElements :: Element -> [[String]]
tableElements = map cells . filter (isElement "Row") . childrenElements
  where cells = map extractData . filter (isElement "Cell") . childrenElements

extractLicense :: [String] -> (String, String, Bool)
extractLicense row = (row !! 1, row !! 0, (row !! 4) == "YES")

main :: IO ()
main = do
  contents <- readFile "data/spdx_licenselist.xml"
  let Just el = parseXMLDoc contents
  let parsed = simplify el
  let licenses = filter (not . null) $ parsed !! 0
  f `mapM_` (sort $ map extractLicense licenses)
  where f (l, n, osi) = putStrLn $ "  , (LicenseId " ++ show l ++ ", " ++ show n ++ ", " ++ show osi ++ ")"
