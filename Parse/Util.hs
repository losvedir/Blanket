module Parse.Util (
  parseTags
  ) where

import Parse.Data
import Data.Char

-- parse string into list of Tags
parseTags :: String -> [Tag]
parseTags = parseTokenList . tokenizeTags

-- Tokenize HTML text into a list of strings delineated by tag, 
-- e.g. ["<div>", "some text", "<a>", "more text", "</a>", "</div>"]
tokenizeTags :: String -> [String]
tokenizeTags "" = []
tokenizeTags ('<':ts) = ("<" ++ tag ++ ">") : (tokenizeTags (tail rest))
  where (tag, rest) = break ('>'==) ts
tokenizeTags ts = text : (tokenizeTags rest)
  where (text, rest) = break ('<'==) ts

-- Recursively build tag tree out of tokenized list of strings
-- e.g. ["<div>","<a>","</a>","</div>"] -> (Div [] [(A [] [])])
parseTokenList :: [String] -> [Tag]
parseTokenList [] = []
parseTokenList tokens@(tok:toks)
  | isTag tok = let tagtype = getTagType tok
                    attribs = getAttributes tok
                    (thisTag, siblingTags) = breakTag tokens
                    nestedTags = tail (take (length thisTag - 1) thisTag)
                in (makeTag tagtype attribs (parseTokenList nestedTags)) : parseTokenList siblingTags
  | otherwise = (makeTextTag tok) : parseTokenList toks
                  
-- Checks if String begins with '<'. If so, we say it's a "Tag". Could probably make more robust in future.
isTag :: String -> Bool
isTag ('<':_) = True
isTag _ = False

-- Looks in string of form < TaGnAmE id="uomo"> for 'tagname'
getTagType :: String -> String
getTagType ('<':ss) = getTagType ss
getTagType(' ':ss) = getTagType ss
getTagType str = tagn
  where (tagn, _) = break (not . isAlphaNum) (map toLower str)

-- Takes a list of string tokens, returns the index of the closing tag that matches
-- the nesting level of the first tag. Expects the first token to be the opening tag
-- but does not check whether the closing is the proper matching one; only checks
-- nesting depth.
findClosingTagIndex :: [String] -> Int
findClosingTagIndex [] = 0
findClosingTagIndex (tag:rst) = findClosingTagIndex' rst 1 1 -- first int is nesting depth, second is index
  where findClosingTagIndex' :: [String] -> Int -> Int -> Int  
        findClosingTagIndex' _ 0 n = n-1
        findClosingTagIndex' (t:ts) d n
          | openTagString t  = findClosingTagIndex' ts (d+1) (n+1)
          | closeTagString t = findClosingTagIndex' ts (d-1) (n+1)
          | otherwise        = findClosingTagIndex' ts d (n+1) -- text between tags
        findClosingTagIndex' [] _ n = n
        
-- Takes a tokenized list of strings, returns a tuple where the first element is a list of 
-- string corresponding to the first tag and its contents, and the second element is
-- the list of strings corresponding to sibling tags.
breakTag :: [String] -> ([String],[String])
breakTag [] = ([], [])
breakTag toks = let i      = findClosingTagIndex toks
                    nested = take (i+1) toks
                    sibs   = drop (i+1) toks
                in (nested, sibs)

-- Checks if tag begins '<' and not '</'. For example: "<div>" -> True
openTagString :: String -> Bool
openTagString (c1:c2:cs)
  | (c1 == '<') && (c2 == '/') = False
  | c1 == '<'                  = True
openTagString cs               = False

-- Checks if first characters are '</'. E.g.: "</div>" -> True
closeTagString :: String -> Bool
closeTagString (c1:c2:cs)
  | (c1 == '<') && (c2 == '/') = True
closeTagString cs              = False

-- Extracts attributes from a tag, returns list of them
-- e.g. <div id="uomo" class="meee"> -> [(Id "uomo"),(Class "meee")]
getAttributes :: String -> [Attrib]
getAttributes s = getAttributesList . stripStartSpaces . fst. break ('>'==) . stripTagName . stripBracket $ s
  where stripBracket = tail  -- we just assume string begins with '<' b/c of tokenization
        stripTagName (s:ss)  -- scan until no longer alphanums. Always list, because tag must end with '>'
          | isAlphaNum s = stripTagName ss
          | otherwise = (s:ss)

stripStartSpaces (' ':ss) = stripStartSpaces ss
stripStartSpaces ss = ss
        
getAttributesList :: String -> [Attrib]
-- will get string of form "" at min, or "[attrib = value]*>" Later: accomodate attribs with no vals
getAttributesList "" = []
getAttributesList atva = 
  let (at,va)    = break ('='==) . stripStartSpaces $ atva
      attribute  = map toLower . filter (\x -> not (x == ' ')) $ at
      (val, rst) = break (' '==) . stripStartSpaces $ tail va  -- tail to get rid of '='
      value      = map toLower val
  in makeAttribute attribute value : getAttributesList rst 