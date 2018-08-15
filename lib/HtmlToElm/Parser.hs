{-# LANGUAGE OverloadedStrings #-}
module HtmlToElm.Parser
  ( parse
  )
where

import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T
import qualified HtmlToElm.HtmlAttributes      as HA
import qualified HtmlToElm.Util                as Util
import qualified HtmlToElm.WhiteList           as WL
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree

type TagAttribute = ([T.Text], [T.Text])

type ListResolved = [(T.Text, Bool)]

type TagAttributeResolved = (ListResolved, ListResolved)

data Parsed = Parsed
  { svgP  :: TagAttribute
  , htmlP :: TagAttribute
  } deriving (Show)

data Resolved = Resolved
  { svgR  :: TagAttributeResolved
  , htmlR :: TagAttributeResolved
  } deriving (Show)

data Source
  = Html
  | HtmlAttributes
  | Svg
  | SvgAttributes
  deriving (Show)

resolveInFavorOfFirst :: ListResolved -> ListResolved -> ListResolved
resolveInFavorOfFirst first second = fmap mapper second
  where mapper (a, v) = if a `elem` fmap fst first then (a, True) else (a, v)

produceTagAttr :: TagAttribute -> T.Text -> [Attribute T.Text] -> TagAttribute
produceTagAttr (tags, attributes) tName attrs =
  let t_ = union tags [tName]
      a_ = union (map fst attrs) attributes
  in  (t_, a_)

parseTag :: Tag T.Text -> (Parsed, Bool) -> (Parsed, Bool)
  -- svg open tag
parseTag (TagOpen "svg" a) (Parsed svg html, _) =
  (Parsed (produceTagAttr svg "svg" a) html, True)
  -- svg close tag
parseTag (TagClose t) (parsed, isInSvg) =
  if t == "svg" then (parsed, False) else (parsed, isInSvg)
  -- in svg tree
parseTag (TagOpen t a) (Parsed svg html, True) =
  (Parsed (produceTagAttr svg t a) html, True)
  -- in html tree
parseTag (TagOpen t a) (Parsed svg html, False) =
  (Parsed svg (produceTagAttr html t a), False)
  -- in svg
parseTag (TagText t) (Parsed svg html, True) = if T.strip t == ""
  then (Parsed svg html, True)
  else (Parsed (produceTagAttr svg "text" []) html, True)
  -- in html
parseTag (TagText t) (Parsed svg html, False) = if T.strip t == ""
  then (Parsed svg html, False)
  else (Parsed svg (produceTagAttr html "text" []), False)
parseTag _ parsed = parsed

parseTagsArray :: [Tag T.Text] -> Parsed
parseTagsArray = fst . foldl go (z, False)
 where
  go acc x = parseTag x acc
  taEmpty = ([], [])
  z       = Parsed {svgP = taEmpty, htmlP = taEmpty}

getResolved :: Parsed -> Resolved
getResolved (Parsed svg html) = Resolved
  { svgR  = (svgT, svgA)
  , htmlR = (htmlTags, htmlA)
  }
 where
  initResolved t = fmap (flip (,) False) t
  svgTags        = initResolved (fst svg)
  svgAttributes  = initResolved (snd svg)
  htmlTags       = initResolved (fst html)
  htmlAttributes = initResolved (snd html)
  htmlA          = resolveInFavorOfFirst htmlTags htmlAttributes
  svgA           = resolveInFavorOfFirst htmlAttributes
    $ resolveInFavorOfFirst svgTags svgAttributes
  svgT = resolveInFavorOfFirst htmlTags svgTags

transformAttributeName :: T.Text -> T.Text
transformAttributeName "in"   = "in_"
transformAttributeName "type" = "type_"
transformAttributeName x      = Util.toCamel x

parseAttributes :: [Attribute T.Text] -> Resolved -> Bool -> T.Text
parseAttributes [] _ _ = "[]"
parseAttributes xs (Resolved svg html) isInSvg =
  "[ " <> T.intercalate ", " p <> " ]"
 where
  prefixString = if isInSvg then "Svg.Attributes" else "Html.Attributes"
  source       = if isInSvg then svg else html
  transformed a b = if b then prefixString <> "." <> a else a
  prefix a = transformed a . fromMaybe True . lookup a $ snd source
  parseAttribute (a, v) =
    let a_ = transformAttributeName a
    in  if isInSvg
          then if a `elem` WL.svgAttributes
            then prefix a <> " " <> Util.wrap v
            else ""
          else if a == "style"
            then "style " <> Util.parseStyle v
            else if a_ `elem` HA.stringFunctions
              then prefix a_ <> " " <> Util.wrap v
              else if a_ `elem` HA.boolFunctions
                then prefix a_ <> " " <> "True"
                else if a_ `elem` HA.intFunctions
                  then prefix a_ <> " " <> Util.parseInt v
                  else "attribute " <> Util.wrap a <> " " <> Util.wrap v
  p = filter ((/=) "") $ map parseAttribute xs

parseTagNames :: Int -> Resolved -> [TagTree T.Text] -> Bool -> T.Text
parseTagNames depth resolved xs isInSvg = case filteredTags of
  [] -> indent_ <> "[]"
  _ ->
    indent_
      <> "[ "
      <> T.intercalate (indent_ <> ", ") filteredTags
      <> (indent_ <> "]")
 where
  filteredTags =
    filter ((/=) "") $ map (treeToString depth resolved isInSvg) xs
  indent_ = "\n" <> T.replicate (depth * 4) " "


prependedTagName :: T.Text -> Resolved -> Bool -> T.Text
prependedTagName tagName (Resolved svg html) isInSvg =
  transformed (nodeOrTag tagName) . fromMaybe True . lookup tagName $ fst source
 where
  source       = if isInSvg then svg else html
  whiteList    = if isInSvg then WL.svgTags else WL.htmlTags
  prefixString = if isInSvg then "Svg." else "Html."
  transformed a b = if b then prefixString <> a else a
  nodeOrTag t = if t `elem` whiteList then t else "node " <> Util.wrap t

treeToString :: Int -> Resolved -> Bool -> TagTree T.Text -> T.Text
treeToString depth resolved _ (TagBranch "svg" attributes nodes) =
  "svg "
    <> parseAttributes attributes resolved True
    <> parseTagNames (depth + 1) resolved nodes True
-- Svg
treeToString depth resolved True (TagBranch name attributes nodes) =
  prependedTagName name resolved True
    <> " "
    <> parseAttributes attributes resolved True
    <> parseTagNames (depth + 1) resolved nodes True
-- Html
treeToString depth resolved False (TagBranch name attributes nodes) =
  prependedTagName name resolved False
    <> " "
    <> parseAttributes attributes resolved False
    <> parseTagNames (depth + 1) resolved nodes False
-- Svg
treeToString _ resolved True (TagLeaf (TagText t)) = if T.strip t == ""
  then ""
  else prependedTagName "text" resolved True <> " " <> Util.wrap t
-- Html
treeToString _ resolved False (TagLeaf (TagText t)) = if T.strip t == ""
  then ""
  else prependedTagName "text" resolved False <> " " <> Util.wrap t
treeToString _ _ _ (TagLeaf _) = ""

generateImports :: Resolved -> T.Text
generateImports (Resolved (svgT, svgA) (htmlT, htmlA)) =
  gen Html htmlT
    <> gen HtmlAttributes htmlA
    <> gen Svg            svgT
    <> gen SvgAttributes  svgA
 where
  gen source list = if null result
    then ""
    else
      "import "
      <> prefixString
      <> " exposing ("
      <> T.intercalate ", " result
      <> ")\n"
   where
    result = case source of
        -- ignore invalid svg attributes
      SvgAttributes -> valid
      _ ->
        if length valid /= length exposed then buildFunction : valid else valid
    valid         = filter (`elem` whiteList) exposed
    exposed       = fmap fst . filter ((/= True) . snd) $ list
    buildFunction = case source of
      Html           -> "node"
      HtmlAttributes -> "attribute"
      Svg            -> "node"
      SvgAttributes  -> ""
    whiteList = case source of
      Html           -> WL.htmlTags
      HtmlAttributes -> WL.htmlAttributes
      Svg            -> WL.svgTags
      SvgAttributes  -> WL.svgAttributes
    prefixString = case source of
      Html           -> "Html"
      HtmlAttributes -> "Html.Attributes"
      Svg            -> "Svg"
      SvgAttributes  -> "Svg.Attributes"

parse :: T.Text -> T.Text
parse input = if i == ""
  then ""
  else generateImports resolved <> "\nview = \n    " <> body
 where
  i        = T.strip input
  tags     = parseTags i
  tree     = parseTree i
  treeHead = head tree
  resolved = getResolved $ parseTagsArray tags
  body     = treeToString 1 resolved False treeHead
