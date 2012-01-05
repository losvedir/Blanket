module Parse.Data where

-- "Tag" is composed of a data constructor for each HTML tag (div, a, etc).
-- In addition there is a "Text" constructor for free text within a tag 
-- Each Tag has a list of attributes and a list of children tags (which in turn can nest tags, and so on
-- The "OtherT" tag is for tags which aren't specifically handled yet. The first "String" argument
-- to the data constructor stores the name of such tags. Similarly for Attributes and OtherA.
data Tag 
         = Html   [Attrib] [Tag] 
         | Head   [Attrib] [Tag] 
         | Title  [Attrib] [Tag]
         | Body   [Attrib] [Tag]
         | Div    [Attrib] [Tag] 
         | A      [Attrib] [Tag] 
         | OtherT String [Attrib] [Tag]
         | Text String 
         deriving Show
                  
data Attrib
           = Id     String 
           | Class  String 
           | Href   String
           | OtherA String String

instance Show Attrib where
  show a = case a of
    (Id str)     -> showtext "id" str
    (Class str)  -> showtext "class" str
    (Href str)   -> showtext "href" str
    (OtherA stra strv) -> stra ++ "=" ++ strv
    where showtext atn str = concat [" ", atn, "=", str]

-- takes a tag and prints it nicely, recursing into its children, and indenting as needed.
prettyPrint :: [Tag] -> String
prettyPrint [] = ""
prettyPrint ts = prettyPrint' ts 0
  where prettyPrint' :: [Tag] -> Int -> String
        prettyPrint' [] _ = ""
        prettyPrint' (t:ts) n = 
          case t of
            (Html attribs childTags)   -> showText "html" attribs childTags ++ prettyPrint' ts n
            (Head attribs childTags)   -> showText "head" attribs childTags ++ prettyPrint' ts n
            (Title attribs childTags)  -> showText "title" attribs childTags ++ prettyPrint' ts n
            (Body attribs childTags)   -> showText "body" attribs childTags ++ prettyPrint' ts n            
            (Div attribs childTags)    -> showText "div" attribs childTags ++ prettyPrint' ts n
            (A attribs childTags)      -> showText "a" attribs childTags ++ prettyPrint' ts n
            (OtherT tagname attribs childTags) -> showText tagname attribs childTags ++ prettyPrint' ts n            
            (Text str)                 -> replicate n ' ' ++ str ++ "\n" ++ prettyPrint' ts n
            where showText name a cT = concat [replicate n ' ', "<", name, printAttribs a, ">\n", prettyPrint' cT (n+2), replicate n ' ', "</", name, ">\n"]
                  printAttribs = (foldr (++) "") . (map show)

-- generates a Tag from a string of tagname, e.g. "div", and a list of attribs and tags
makeTag :: String -> [Attrib] -> [Tag] -> Tag
makeTag tagtype attribs tags =
  case tagtype of
    "html"  -> (Html attribs tags)
    "head"  -> (Head attribs tags)
    "title" -> (Title attribs tags)
    "body"  -> (Body attribs tags)
    "div"   -> (Div attribs tags)
    "a"     -> (A attribs tags)
    _       -> (OtherT tagtype attribs tags)
    
-- makes a Text Tag from a given string. String should have no nested tags
makeTextTag :: String -> Tag
makeTextTag = Text 

-- generates an Attrib from a name string and value string
makeAttribute name val = 
  case name of
    "id"    -> (Id val)
    "class" -> (Class val)
    "href"  -> (Href val)
    _       -> (OtherA name val)