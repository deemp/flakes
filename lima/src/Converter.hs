-- |
-- == Motivation
--
-- Sometimes, it's nice to have a @README.md@ document or a @LaTeX@ document with runnable @Haskell@ code examples.
--
-- This library provides functions for converting between several 'Dialect's losslessly.
--
-- A typical workflow may be as follows:
--
-- 1. Edit the @Haskell@ code in a @.hs@ document with the full power of @Haskell Language Server@.
-- 1. Convert a @.hs@ document into a @.md@ document.
-- 1. Edit text in a @.md@ document.
-- 1. Format a @.md@ document with, e.g., [markdownlint](https://github.com/DavidAnson/markdownlint).
-- 1. Convert a @.md@ document into a @.hs@ document.
-- 1. Repeat the steps.
--
-- == Terms #terms#
--
-- * @document@ - a 'String' representing certain document.
-- * @dialect@ - a document dialect like @Haskell@ or @Markdown@. See 'Dialect'.
-- * 'Token' - an internal representation of a document block.
-- * 'Tokens' - a list of 'Token's.
-- * @parser@ - a function that reads document line by line and converts it into 'Token's. Example: 'hsIntoTokens'.
-- * @printer@ - a function that converts 'Tokens' into document.
-- * @tag@ - a marker that affects how 'Tokens' are parsed.
--     * Each parser recognized tags of a specific form.
--     * Tags can be represented as a wrapper and a name.
--       E.g., in @'% LIMA_DISABLE'@, a @TeX@ tag, the wrapper is @'% '@ and the name is @'LIMA_DISABLE'@.
--     * Usually, parsers recognize the tag names that __start with__ tag names specified in a 'Config'.
--     * Some tags have no specific name. E.g., @% some line@ is a @TeX@ comment line.
--     * When a parser is in a disabled 'State', it doesn't recognize any tags until it reaches an enabling tag.
--
-- == Assumptions #assumptions#
--
-- The following assumptions must hold for outputs of parsers and inputs of printers:
--
--     - 'Tokens' are in the same order as the corresponding blocks of document.
--     - Lines inside 'Tokens' are reversed compared to the document. Example:
--
--         - @Literate Haskell@ document:
--
--             @
--             line 1
--             line 2
--
--             % line 3
--             @
--
--         - 'Tokens':
--
--             @
--             [
--               Text {someLines = ["line2","line 1"]},
--               Comment {body = "line 3" :| []}
--             ]
--             @
module Converter (
  -- * Parser and printer config
  Mode,
  User,
  Internal,
  Config (..),

  -- ** Lenses
  disable,
  enable,
  indent,
  dedent,
  mdHaskellCodeStart,
  mdHaskellCodeEnd,
  texHaskellCodeStart,
  texHaskellCodeEnd,

  -- * Dialect
  Dialect (..),
  translateTo,
  showDialectExtension,
  showDialectName,

  -- * Internal representation
  Token (..),
  Tokens,

  -- * Printers
  hsFromTokens,
  hsFromTokens',
  lhsFromTokens,
  lhsFromTokens',
  mdFromTokens,
  mdFromTokens',
  texFromTokens,
  texFromTokens',

  -- * Parsers
  lhsIntoTokens,
  hsIntoTokens,
  texIntoTokens,
  mdIntoTokens,

  -- * Selectors
  selectFromTokens,
  selectIntoTokens,

  -- * Helpers
  toInternalConfig,
  fromInternalConfig,
  stripEmpties,
  mergeTokens,
  stripTokens,
  normalizeTokens,
  PrettyPrint (..),

  -- * Examples
  exampleNonTexTokens',
  exampleNonTexTokens,
  exampleTexTokens,

  -- * Instances
  def,
) where

import Data.Char (isAlpha)
import Data.Data (Data (toConstr), showConstr)
import Data.Default (Default (def))
import Data.List (dropWhileEnd, intercalate, intersperse, isPrefixOf, isSuffixOf)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList, (<|))
import GHC.Generics (Generic)
import Lens.Micro (non, (<&>), (^.), (^?))
import Lens.Micro.TH (makeLenses)
import Text.Read (readMaybe)

-- | A kind of data markers.
data Mode'
  = Internal
  | User

-- | Marks data for internal usage.
type Internal = 'Internal

-- | Marks data supplied by a user.
type User = 'User

-- | Calculates the mode for data.
type family Mode a b where
  Mode User b = Maybe b
  Mode Internal b = b

-- | Configuration of tag names.
--
-- When converted to 'Config Internal' the default values are:
--
-- >>> pp (def :: Config User)
-- Config {
--   _disable = Just "LIMA_DISABLE",
--   _enable = Just "LIMA_ENABLE",
--   _indent = Just "LIMA_INDENT",
--   _dedent = Just "LIMA_DEDENT",
--   _mdHaskellCodeStart = Just "```haskell",
--   _mdHaskellCodeEnd = Just "```",
--   _texHaskellCodeStart = Just "\\begin{code}",
--   _texHaskellCodeEnd = Just "\\end{code}"
-- }
data Config (a :: Mode') = Config
  { _disable :: Mode a String
  -- ^
  -- Make parser ignore tags and just copy the following lines verbatim.
  --
  -- Set indentation to @0@.
  , _enable :: Mode a String
  -- ^ Stop parser from ignoring tags.
  , _indent :: Mode a String
  -- ^ Set code indentation to a given 'Int'.
  , _dedent :: Mode a String
  -- ^ Set code indentation to @0@.
  , _mdHaskellCodeStart :: Mode a String
  -- ^ Mark the start of a @Haskell@ code block in @Markdown@.
  , _mdHaskellCodeEnd :: Mode a String
  -- ^ Mark the end of a @Haskell@ code block in @Markdown@.
  , _texHaskellCodeStart :: Mode a String
  -- ^ Mark the start of a @Haskell@ code block in @TeX@.
  , _texHaskellCodeEnd :: Mode a String
  -- ^ Mark the end of a @Haskell@ code block in @TeX@.
  }
  deriving (Generic)

makeLenses ''Config

deriving instance Show (Config User)
deriving instance Eq (Config User)
deriving instance Show (Config Internal)

newtype Pretty a = Pretty String

instance Show a => Show (Pretty a) where
  show :: Pretty a -> String
  show (Pretty s) = s

-- | A class for prettyprinting data on multiple lines in haddocks
class Show a => PrettyPrint a where
  pp :: a -> Pretty String

instance PrettyPrint String where
  pp :: String -> Pretty String
  pp = Pretty . dropWhileEnd (== '\n')

instance PrettyPrint (Config User) where
  pp :: Config User -> Pretty String
  pp (fromInternalConfig . toInternalConfig -> config) =
    Pretty $
      ( concatMap
          ( \(a, b) ->
              if
                  | [a, b] == " _" -> "\n  "
                  | [a, b] == "{_" -> "{\n  "
                  | otherwise -> [a]
          )
          $ (zip (show config) (tail $ show config))
      )
        <> "\n}"

instance Default (Config Internal) where
  def :: Config Internal
  def = Config{..}
   where
    _disable = "LIMA_DISABLE"
    _enable = "LIMA_ENABLE"
    _indent = "LIMA_INDENT"
    _dedent = "LIMA_DEDENT"
    _mdHaskellCodeStart = "```haskell"
    _mdHaskellCodeEnd = "```"
    _texHaskellCodeStart = "\\begin{code}"
    _texHaskellCodeEnd = "\\end{code}"

-- | Make a user 'Config' with default values from an internal 'Config'.
fromInternalConfig :: Config Internal -> Config User
fromInternalConfig conf = Config{..}
 where
  _disable = conf ^? disable
  _enable = conf ^? enable
  _indent = conf ^? indent
  _dedent = conf ^? dedent
  _mdHaskellCodeStart = conf ^? mdHaskellCodeStart
  _mdHaskellCodeEnd = conf ^? mdHaskellCodeEnd
  _texHaskellCodeStart = conf ^? texHaskellCodeStart
  _texHaskellCodeEnd = conf ^? texHaskellCodeEnd

instance Default (Config User) where
  def :: Config User
  def = fromInternalConfig def

-- | Convert a user 'Config' into an internal 'Config' with user-supplied values.
toInternalConfig :: Config User -> Config Internal
toInternalConfig conf =
  Config
    { _disable = conf ^. disable . non _disable
    , _enable = conf ^. enable . non _enable
    , _indent = conf ^. indent . non _indent
    , _dedent = conf ^. dedent . non _dedent
    , _mdHaskellCodeStart = conf ^. mdHaskellCodeStart . non _mdHaskellCodeStart
    , _mdHaskellCodeEnd = conf ^. mdHaskellCodeEnd . non _mdHaskellCodeEnd
    , _texHaskellCodeStart = conf ^. texHaskellCodeStart . non _texHaskellCodeStart
    , _texHaskellCodeEnd = conf ^. texHaskellCodeEnd . non _texHaskellCodeEnd
    }
 where
  Config{..} = def @(Config Internal)

-- | A dialect of a document.
data Dialect
  = -- | @Haskell@
    Hs
  | -- | @Literate Haskell@
    Lhs
  | -- | @Markdown@
    Md
  | -- | @TeX@
    TeX

-- | Internal representation of a document.
data Token
  = -- | Indent @Haskell@ code by @n@.
    Indent {n :: Int}
  | -- | Set @Haskell@ code indentation to @0@.
    Dedent
  | -- | 'String's copied verbatim while a parser was disabled.
    Disabled {someLines :: [String]}
  | -- | 'String's copied verbatim while a parser was in a @Haskell@ code block.
    --
    -- Must be normalized at some moment. See 'normalizeTokens'.
    HaskellCode {someLines :: [String]}
  | -- | 'String's copied verbatim while a parser was in a text block.
    Text {someLines :: [String]}
  | -- | 'String's copied verbatim while a parser was in a comment block.
    Comment {body :: NonEmpty String}
  deriving (Show, Data, Eq)

-- | A list of 'Token's.
type Tokens = [Token]

instance PrettyPrint (Tokens) where
  pp :: Tokens -> Pretty String
  pp ts =
    Pretty $
      ( concatMap
          ( \(a, b) ->
              if
                  | a == ',' && isAlpha b -> ",\n  "
                  | a == '[' && isAlpha b -> "[\n  "
                  | otherwise -> [a]
          )
          $ (zip (show ts) (tail $ show ts))
      )
        <> "\n]"

-- | Merge specific subsequent 'Tokens'.
--
-- >>> pp exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Dedent,
--   HaskellCode {someLines = ["  f b","a ="]},
--   Indent {n = 2},
--   Indent {n = 5},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Comment {body = "Hello," :| []},
--   Comment {body = "world!" :| []},
--   Text {someLines = ["Line 2","Line 1"]},
--   Text {someLines = ["Line 2","Line 1"]},
--   Disabled {someLines = ["Line 2","Line 1"]}
-- ]
--
-- >>> pp $ mergeTokens exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Dedent,
--   HaskellCode {someLines = ["  f b","a ="]},
--   Indent {n = 2},
--   Indent {n = 5},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Comment {body = "world!" :| ["","Hello,"]},
--   Text {someLines = ["Line 2","Line 1","","Line 2","Line 1"]},
--   Disabled {someLines = ["Line 2","Line 1"]}
-- ]
mergeTokens :: Tokens -> Tokens
mergeTokens (t1@Text{} : t2@Text{} : ts) = mergeTokens $ Text{someLines = someLines t2 <> [[]] <> someLines t1} : ts
mergeTokens (t1@Comment{} : t2@Comment{} : ts) = mergeTokens $ Comment{body = body t2 <> ([] <| body t1)} : ts
mergeTokens (t : ts) = t : mergeTokens ts
mergeTokens ts = ts

-- | Example non-@TeX@ 'Tokens'. See 'exampleTexTokens'.
--
-- When printed into a @TeX@ document, these 'Tokens' can't be correctly parsed.
-- This is because they don't have necessary tags surrounding @Haskell@ code blocks.
--
-- >>> pp $ exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Dedent,
--   HaskellCode {someLines = ["  f b","a ="]},
--   Indent {n = 2},
--   Indent {n = 5},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Comment {body = "Hello," :| []},
--   Comment {body = "world!" :| []},
--   Text {someLines = ["Line 2","Line 1"]},
--   Text {someLines = ["Line 2","Line 1"]},
--   Disabled {someLines = ["Line 2","Line 1"]}
-- ]
exampleNonTexTokens' :: Tokens
exampleNonTexTokens' =
  [ Indent 3
  , hsSnippet
  , Dedent
  , hsSnippet
  , Indent 2
  , Indent 5
  , hsSnippet
  , Comment ("Hello," :| [])
  , Comment ("world!" :| [])
  , someText
  , someText
  , Disabled{someLines = lines'}
  ]
 where
  hsSnippet = HaskellCode{someLines = ["  f b", "a ="]}
  someText = Text{someLines = lines'}
  lines' = ["Line 2", "Line 1"]

-- | Normalized 'exampleNonTexTokens''.
--
-- >>>pp $ exampleNonTexTokens
-- [
--   Indent {n = 3},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Dedent,
--   HaskellCode {someLines = ["  f b","a ="]},
--   Indent {n = 2},
--   Indent {n = 5},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Comment {body = "world!" :| ["","Hello,"]},
--   Text {someLines = ["Line 2","Line 1","","Line 2","Line 1"]},
--   Disabled {someLines = ["Line 2","Line 1"]}
-- ]
exampleNonTexTokens :: Tokens
exampleNonTexTokens = normalizeTokens exampleNonTexTokens'

-- | Select a printer function based on a given dialect.
selectFromTokens :: Config User -> Dialect -> Tokens -> String
selectFromTokens config dialect =
  ( case dialect of
      Hs -> hsFromTokens
      Lhs -> lhsFromTokens
      Md -> mdFromTokens
      TeX -> texFromTokens
  )
    config

-- | Select a parser function based on a given dialect.
selectIntoTokens :: Config User -> Dialect -> String -> Tokens
selectIntoTokens config dialect =
  ( case dialect of
      Hs -> hsIntoTokens
      Lhs -> lhsIntoTokens
      Md -> mdIntoTokens
      TeX -> texIntoTokens
  )
    config

-- | Compose a function that converts between documents in different 'Dialect's.
translateTo :: Dialect -> Dialect -> Config User -> String -> String
translateTo a b config src = selectFromTokens config b $ selectIntoTokens config a src

-- | Escaped hash character
escapedHash :: String
escapedHash = "\\#"

-- | Hash character
hash :: String
hash = "#"

-- | Drop a prefix of a line with length of a given line
dropLen :: String -> String -> String
dropLen x y = drop (length x) y

-- | Check if a list starts with a given list
startsWith :: Eq a => [a] -> [a] -> Bool
startsWith = flip isPrefixOf

-- | Check if a list ends with a given list
endsWith :: Eq a => [a] -> [a] -> Bool
endsWith = flip isSuffixOf

-- | Drop the given number of tokens from the end of a list
dropEnd :: Int -> [a] -> [a]
dropEnd n s = reverse (drop n (reverse s))

-- | Drop leading spaces and drop at each end of a 'String' the number of characters as in the supplied prefix and suffix.
stripEnds :: String -> String -> String -> String
stripEnds prefix suffix x = dropEnd (length suffix) (dropLen prefix (dropSpaces x))

-- | Replace "\\#" with "#" in a 'String' prefix.
lhsUnescapeHash :: String -> String
lhsUnescapeHash x = if x `startsWith` escapedHash then hash <> (dropLen escapedHash x) else x

-- | Replace "#" with "\\#" in a 'String' prefix.
lhsEscapeHash :: String -> String
lhsEscapeHash x = if x `startsWith` hash then escapedHash <> (dropLen hash x) else x

-- | State of a parser.
--
-- Only one flag can be enabled when processing a line.
--
-- Flags signify in what document block a converter is at the moment.
data State = State
  { inText :: Bool
  , inHaskellCode :: Bool
  , inDisabled :: Bool
  , inComment :: Bool
  }
  deriving (Generic)

instance Default State where
  def :: State
  def =
    State
      { inText = False
      , inHaskellCode = False
      , inDisabled = False
      , inComment = False
      }

-- | 'mergeTokens' and 'stripTokens'.
--
-- >>>pp $ normalizeTokens exampleNonTexTokens
-- [
--   Indent {n = 3},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Dedent,
--   HaskellCode {someLines = ["  f b","a ="]},
--   Indent {n = 2},
--   Indent {n = 5},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Comment {body = "world!" :| ["","Hello,"]},
--   Text {someLines = ["Line 2","Line 1","","Line 2","Line 1"]},
--   Disabled {someLines = ["Line 2","Line 1"]}
-- ]
normalizeTokens :: Tokens -> Tokens
normalizeTokens tokens = stripTokens $ mergeTokens $ tokens

-- | Compose a function from a 'String' to 'Tokens'.
mkIntoTokens :: (State -> [(Int, String)] -> [Token] -> [Token]) -> String -> Tokens
mkIntoTokens toTokens xs = normalizeTokens (drop 1 $ reverse $ toTokens def (zip [1 ..] (lines xs)) [Dedent])

-- | Parse into a token contents of a multiline comment written on a single line.
parseToken :: Config Internal -> Token -> String -> Int -> Tokens
parseToken Config{..} prev l lineNumber
  | l `startsWith` _indent =
      maybe
        (error $ "Expected a number at line: " <> show lineNumber)
        (\x -> [Indent (max 0 x), prev])
        (readMaybe @Int (dropLen _indent l))
  | l == _dedent = [Dedent, prev]
  | otherwise =
      case prev of
        Comment{..} -> [Comment{body = l <| body}]
        _ -> [Comment (l :| []), prev]

-- | Show error with line number for a token.
showTokenError :: (Show a, Data a) => Int -> a -> a -> String
showTokenError lineNumber lastToken expectedToken =
  ("I got into a wrong state at line: " <> show lineNumber <> "\n\n")
    <> ("Expected last token: " <> constructorName expectedToken <> "\n\n")
    <> ("Got last token: " <> show lastToken <> "\n\n")

-- | Strip the given value from the beginning and the end of a list.
stripValue :: Eq a => a -> [a] -> [a]
stripValue x = dropWhileEnd (== x) . dropWhile (== x)

-- | Pad a 'String' with a given number of spaces
indentN :: Int -> String -> String
indentN x s = concat (replicate x " ") <> s

-- | Compose a function from 'Tokens' to a 'String'.
mkFromTokens :: (Config User -> Tokens -> [String]) -> Config User -> Tokens -> String
mkFromTokens f' config = (<> "\n") . intercalate "\n" . f' config

-- | A data type for showing example strings
data Example = Example String

instance Show Example where
  show :: Example -> String
  show (Example s) = s

-- | 'exampleNonTexTokens' with @TeX@-specific tags that make @Haskell@ code blocks correctly parsable.
--
-- >>> pp $ exampleTexTokens
-- [
--   Indent {n = 3},
--   Text {someLines = ["\\begin{code}"]},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Text {someLines = ["\\end{code}"]},
--   Dedent,
--   Text {someLines = ["\\begin{code}"]},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Text {someLines = ["\\end{code}"]},
--   Indent {n = 2},
--   Indent {n = 5},
--   Text {someLines = ["\\begin{code}"]},
--   HaskellCode {someLines = ["  f b","a ="]},
--   Text {someLines = ["\\end{code}"]},
--   Comment {body = "world!" :| ["","Hello,"]},
--   Text {someLines = ["Line 2","Line 1","","Line 2","Line 1"]},
--   Disabled {someLines = ["Line 2","Line 1"]}
-- ]
exampleTexTokens :: Tokens
exampleTexTokens =
  normalizeTokens $
    concat $
      exampleNonTexTokens
        <&> ( \case
                x@HaskellCode{} ->
                  [ Text{someLines = ["\\begin{code}"]}
                  , x
                  , Text{someLines = ["\\end{code}"]}
                  ]
                x -> [x]
            )

-- | Convert 'Tokens' into @TeX@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
--
-- - These are the relations between lines and tags with the default 'Config' values.
--
--     - @'% LIMA_INDENT N'@ (@N@ is an 'Int') ~ 'Indent'
--     - @'% LIMA_DEDENT'@ ~ 'Dedent'.
--     - Lines between @'% LIMA_DISABLE'@ and @'% LIMA_ENABLE'@ ~ 'Disabled'.
--     - Subsequent lines, either empty or starting with @'% '@ ~ 'Comment'.
--
--         @
--         % Hello,
--         % world!
--
--         % Hello,
--         % user!
--         @
--
--     - Lines between possibly indented tags @'\\begin{code}'@ and @'\\end{code}'@ ~ 'HaskellCode'.
--
--     - Other lines ~ 'Text'.
--
-- === __Example__
--
-- >>> pp $ texFromTokens def exampleTexTokens
-- % LIMA_INDENT 3
-- <BLANKLINE>
-- \begin{code}
--    a =
--      f b
-- \end{code}
-- <BLANKLINE>
-- % LIMA_DEDENT
-- <BLANKLINE>
-- \begin{code}
-- a =
--   f b
-- \end{code}
-- <BLANKLINE>
-- % LIMA_INDENT 2
-- <BLANKLINE>
-- % LIMA_INDENT 5
-- <BLANKLINE>
-- \begin{code}
--      a =
--        f b
-- \end{code}
-- <BLANKLINE>
-- % Hello,
-- <BLANKLINE>
-- % world!
-- <BLANKLINE>
-- Line 1
-- Line 2
-- <BLANKLINE>
-- Line 1
-- Line 2
-- <BLANKLINE>
-- % LIMA_DISABLE
-- <BLANKLINE>
-- % Line 1
-- % Line 2
-- <BLANKLINE>
-- % LIMA_ENABLE
texFromTokens :: Config User -> Tokens -> String
texFromTokens = mkFromTokens texFromTokens'

-- | Drop leading newlines in the first string of a list
dropLeadingNewlines :: [String] -> [String]
dropLeadingNewlines (t : ts) = (dropWhile (== '\n') t) : ts
dropLeadingNewlines x = x

-- | Start a @TeX@ comment.
texComment :: String
texComment = "%"

-- | Start a @TeX@ comment plus a space.
texCommentSpace :: String
texCommentSpace = texComment <> " "

-- | Convert 'Tokens' into @TeX@ code.
--
-- Each 'Token' becomes a 'String' in a list.
--
-- These 'String's are concatenated in 'texFromTokens'.
texFromTokens' :: Config User -> Tokens -> [String]
texFromTokens' (toInternalConfig -> Config{..}) tokens =
  dropLeadingNewlines $
    reverse
      ( intercalate "\n" . reverse
          <$> ( fromTokens (Dedent : tokens) (0, [])
              )
      )
 where
  fromTokens :: Tokens -> (Int, [[String]]) -> [[String]]
  fromTokens bs'@(_ : cur : bs) (curIndent, rs) =
    fromTokens (cur : bs) (translate curIndent bs' rs)
  fromTokens [_] (_, rs) = rs
  fromTokens _ _ = error "Not enough tokens"
  translate curIndent (prev : cur : _) rs =
    case cur of
      Indent{..} -> (n,) $ [texCommentSpace <> _indent <> " " <> show n, []] : rs
      Dedent -> (0,) $ [texCommentSpace <> _dedent, []] : rs
      Disabled{..} -> (0,) $ [[texCommentSpace <> _enable, []], (prependTexComment <$> someLines), [[], texCommentSpace <> _disable, []]] <> rs
      HaskellCode{..} ->
        (curIndent,) $
          (indentN curIndent <$> someLines)
            : ( case prev of
                  Text{} -> rs
                  _ -> [] : rs
              )
      Text{..} ->
        (curIndent,) $
          someLines
            : ( case prev of
                  HaskellCode{} -> rs
                  _ -> [] : rs
              )
      Comment{body = t :| ts} -> (curIndent, (prependTexComment <$> (t : ts)) : [] : rs)
  translate _ _ _ = error "Not enough tokens"

-- | Drop spaces at the start and the end of a 'String'.
dropSpaces :: String -> String
dropSpaces = stripValue ' '

-- | Prepend start of a @TeX@ comment (@'% '@) to a 'String'.
prependTexComment :: String -> String
prependTexComment l
  | l == "" = l
  | otherwise = texCommentSpace <> l

-- | Drop start of a @TeX@ comment from a 'String'.
dropTexComment :: Show a => String -> a -> String
dropTexComment l lineNumber
  | l `startsWith` texCommentSpace = dropLen texCommentSpace l
  | l == "" = l
  | otherwise = error "The line " <> show lineNumber <> " should either be empty or start with '% '"

-- | Convert 'Tokens' into @TeX@ code.
--
-- Inverse of 'texFromTokens'.
--
-- >>> (texIntoTokens def $ texFromTokens def exampleTexTokens) == exampleTexTokens
-- True
texIntoTokens :: Config User -> String -> Tokens
texIntoTokens (toInternalConfig -> conf@Config{..}) xs = tokens
 where
  tokens = mkIntoTokens toTokens xs
  toTokens :: State -> [(Int, String)] -> Tokens -> Tokens
  toTokens State{..} ((lineNumber, l) : ls) result@(r : rs)
    | inDisabled =
        if
            | -- enable
              l `startsWith` (texCommentSpace <> _enable) ->
                toTokens def ls result
            | -- copy lines
              otherwise ->
                toTokens def{inDisabled} ls $
                  case r of
                    Disabled{..} -> (r{someLines = (dropTexComment l lineNumber) : someLines} : rs)
                    _ -> err Disabled{}
    | inHaskellCode =
        if
            | -- end of a snippet
              dropSpaces l `startsWith` _texHaskellCodeEnd ->
                toTokens def{inText = True} ls (Text{someLines = [l]} : result)
            | otherwise ->
                toTokens def{inHaskellCode} ls $
                  case r of
                    HaskellCode{..} -> (r{someLines = l : someLines} : rs)
                    _ -> err HaskellCode{}
    | dropSpaces l `startsWith` _texHaskellCodeStart =
        toTokens def{inHaskellCode = True} ls $
          HaskellCode{someLines = []}
            : case r of
              Text{..} -> Text{someLines = l : someLines} : rs
              _ -> Text{someLines = [l]} : result
    | -- comment on a single line
      l `startsWith` texCommentSpace =
        let l' = dropLen texCommentSpace l
         in if
                | -- disable
                  l' `startsWith` _disable ->
                    toTokens def{inDisabled = True} ls (Disabled [] : result)
                | otherwise ->
                    toTokens def ls $
                      parseToken conf r l' lineNumber <> rs
    | inText =
        toTokens def{inText} ls $
          case r of
            Text{..} -> Text{someLines = l : someLines} : rs
            _ -> err Text{}
    | -- a blank line
      null l =
        case r of
          Comment{body} -> toTokens def{inComment} ls (Comment{body = l <| body} : rs)
          _ -> toTokens def ls result
    | -- start of a text
      otherwise =
        toTokens def{inText = True} ls $ Text{someLines = [l]} : result
   where
    err = error . showTokenError lineNumber r
  toTokens _ _ res = res

-- | Convert 'Tokens' into @Literate Haskell@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
--
-- - These are the relations between lines and tags with the default 'Config' values.
--
--     - @'% LIMA_INDENT N'@ (@N@ is an 'Int') ~ 'Indent'.
--     - @'% LIMA_DEDENT'@ ~ 'Dedent'.
--     - Lines between @'% LIMA_DISABLE'@ and @'% LIMA_ENABLE'@ ~ 'Disabled'.
--     - Subsequent lines, either empty or starting with @'% '@ ~ 'Comment'.
--
--         @
--         % Hello,
--         % world!
--
--         % Hello,
--         % user!
--         @
--
--     - Subsequent lines starting with @'> '@ ~ 'HaskellCode'.
--
--         @
--         > a4 = 4
--         > a2 = 2
--         @
--
--     - Other lines ~ 'Text'.
--
-- === __Example__
--
-- >>> pp $ lhsFromTokens def exampleNonTexTokens
-- % LIMA_INDENT 3
-- <BLANKLINE>
-- >    a =
-- >      f b
-- <BLANKLINE>
-- % LIMA_DEDENT
-- <BLANKLINE>
-- >    a =
-- >      f b
-- <BLANKLINE>
-- % LIMA_INDENT 2
-- <BLANKLINE>
-- % LIMA_INDENT 5
-- <BLANKLINE>
-- >      a =
-- >        f b
-- <BLANKLINE>
-- % Hello,
-- <BLANKLINE>
-- % world!
-- <BLANKLINE>
-- Line 1
-- Line 2
-- <BLANKLINE>
-- Line 1
-- Line 2
-- <BLANKLINE>
-- % LIMA_DISABLE
-- <BLANKLINE>
-- Line 1
-- Line 2
-- <BLANKLINE>
-- % LIMA_ENABLE
lhsFromTokens :: Config User -> Tokens -> String
lhsFromTokens config tokens = mkFromTokens lhsFromTokens' config tokens

-- | Start a @Literate Haskell@ comment.
lhsComment :: String
lhsComment = "%"

-- | Start a @Literate Haskell@ comment plus a space.
lhsCommentSpace :: String
lhsCommentSpace = lhsComment <> " "

-- | Start a @Literate Haskell@ line of @Haskell@ code.
lhsHsCode :: String
lhsHsCode = ">"

-- | Start a @Literate Haskell@ line of @Haskell@ code plus a space.
lhsHsCodeSpace :: String
lhsHsCodeSpace = lhsHsCode <> " "

-- | Prepend start of a @TeX@ comment (@'% '@) to a 'String'.
prependLhsComment :: String -> String
prependLhsComment l
  | l == "" = l
  | otherwise = texCommentSpace <> l

-- | Convert 'Tokens' into @Literate Haskell@ code.
--
-- Each 'Token' becomes a 'String' in a list.
--
-- These 'String's are concatenated in 'lhsFromTokens'.
lhsFromTokens' :: Config User -> Tokens -> [String]
lhsFromTokens' (toInternalConfig -> Config{..}) blocks =
  dropLeadingNewlines $ reverse (intercalate "\n" . reverse <$> (fromTokens (Dedent : blocks) (0, [])))
 where
  fromTokens :: Tokens -> (Int, [[String]]) -> [[String]]
  fromTokens bs'@(_ : cur : bs) (curIndent, rs) =
    fromTokens (cur : bs) (translate curIndent bs' rs)
  fromTokens [_] (_, rs) = rs
  fromTokens _ _ = error "Not enough tokens"
  translate curIndent (prev : cur : _) rs =
    case cur of
      Indent{..} -> (n,) $ [lhsCommentSpace <> _indent <> " " <> show n, ""] : rs
      Dedent -> (curIndent,) $ [lhsCommentSpace <> _dedent, []] : rs
      Disabled{..} -> (0,) $ [lhsCommentSpace <> _enable, ""] : someLines : ["", lhsCommentSpace <> _disable] : [] : rs
      HaskellCode{..} ->
        (curIndent,) $
          ((lhsHsCodeSpace <>) . indentN curIndent <$> someLines)
            : ( case prev of
                  Text{} -> rs
                  _ -> [] : rs
              )
      Text{..} ->
        (curIndent,) $
          (lhsEscapeHash <$> someLines)
            : ( case prev of
                  HaskellCode{} -> rs
                  _ -> [] : rs
              )
      Comment{body = t :| ts} -> (curIndent, (prependLhsComment <$> t : ts) : [] : rs)
  translate _ _ _ = error "Not enough tokens"

-- | Convert 'Tokens' into @Markdown@ code.
--
-- Inverse of 'lhsFromTokens'.
--
-- >>> (lhsIntoTokens def $ lhsFromTokens def exampleNonTexTokens) == exampleNonTexTokens
-- True
lhsIntoTokens :: Config User -> String -> Tokens
lhsIntoTokens (toInternalConfig -> conf@Config{..}) xs = tokens
 where
  tokens = mkIntoTokens toTokens xs
  toTokens :: State -> [(Int, String)] -> Tokens -> Tokens
  toTokens State{..} ((lineNumber, lhsUnescapeHash -> l) : ls) result@(r : rs)
    | inDisabled =
        if
            | -- enable
              l `startsWith` (lhsCommentSpace <> _enable) ->
                toTokens def ls result
            | -- copy lines
              otherwise ->
                toTokens def{inDisabled} ls $
                  case r of
                    Disabled{..} -> (r{someLines = l : someLines} : rs)
                    _ -> err Disabled{}
    | -- comment on a single line
      l `startsWith` lhsCommentSpace =
        let l' = dropLen lhsCommentSpace l
         in if
                | -- disable
                  l' `startsWith` _disable ->
                    toTokens def{inDisabled = True} ls (Disabled [] : result)
                | otherwise ->
                    toTokens def ls $
                      parseToken conf r l' lineNumber <> rs
    | -- start of a snippet
      l `startsWith` lhsHsCodeSpace =
        toTokens def{inHaskellCode = True} ls $
          let l' = dropLen lhsHsCodeSpace l
           in case r of
                HaskellCode{..} -> (r{someLines = l' : someLines} : rs)
                _ -> HaskellCode{someLines = [l']} : result
    | inText =
        toTokens def{inText} ls $
          case r of
            Text{..} -> Text{someLines = l : someLines} : rs
            _ -> err Text{}
    | -- a blank line
      null l =
        case r of
          Comment{body} -> toTokens def{inComment} ls (Comment{body = l <| body} : rs)
          _ -> toTokens def ls result
    | -- start of a text
      otherwise =
        toTokens def{inText = True} ls $ Text{someLines = [l]} : result
   where
    err = error . showTokenError lineNumber r
  toTokens _ _ res = res

-- | Convert 'Tokens' into @Markdown@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
--
-- - These are the relations between lines and tags with the default 'Config' values.
--
--     - @'<!-- LIMA_INDENT N --\>'@ (@N@ is an 'Int') ~ 'Indent'
--     - @'<!-- LIMA_DEDENT --\>'@ ~ 'Dedent'.
--     - Multiline comment
--       starting with @'<!-- LIMA_DISABLE\\n'@
--       and ending with @'\\nLIMA_ENABLE --\>'@  ~ 'Disabled'.
--
--         @
--         <!-- LIMA_DISABLE
--         a4 = 4
--         a2 = 2
--         LIMA_ENABLE --\>
--         @
--
--     - Multiline comments starting with @'<!-- '@ ~ 'Comment'.
--
--         @
--         <!-- line 1
--         line 2
--         --\>
--         @
--
--     - Indented block starting with @\'```haskell\'@ and ending with @'```'@ ~ 'HaskellCode'.
--
--         @
--           ```haskell
--             a4 = 2
--           ```
--         @
--
--     - Other lines ~ 'Text'.
--
--         @
--         Hello, world!
--         @
--
-- === __Example__
--
-- >>> pp $ mdFromTokens def exampleNonTexTokens
--    <!-- LIMA_INDENT 3 -->
-- <BLANKLINE>
--    ```haskell
--    a =
--      f b
--    ```
-- <BLANKLINE>
-- <!-- LIMA_DEDENT -->
-- <BLANKLINE>
-- ```haskell
-- a =
--   f b
-- ```
-- <BLANKLINE>
--   <!-- LIMA_INDENT 2 -->
-- <BLANKLINE>
--      <!-- LIMA_INDENT 5 -->
-- <BLANKLINE>
--      ```haskell
--      a =
--        f b
--      ```
-- <BLANKLINE>
-- <!-- Hello,
-- <BLANKLINE>
-- world!
-- -->
-- <BLANKLINE>
-- Line 1
-- Line 2
-- <BLANKLINE>
-- Line 1
-- Line 2
-- <BLANKLINE>
-- <!-- LIMA_DISABLE
-- <BLANKLINE>
-- Line 1
-- Line 2
-- <BLANKLINE>
-- LIMA_ENABLE -->
mdFromTokens :: Config User -> Tokens -> String
mdFromTokens = mkFromTokens mdFromTokens'

-- | Open a @Markdown@ comment.
mdCommentOpen :: String
mdCommentOpen = "<!--"

-- | Close a @Markdown@ comment.
mdCommentClose :: String
mdCommentClose = "-->"

-- | Open a @Markdown@ comment plus a space.
mdCommentOpenSpace :: String
mdCommentOpenSpace = mdCommentOpen <> " "

-- | A space plus close a @Markdown@ comment.
mdCommentCloseSpace :: String
mdCommentCloseSpace = " " <> mdCommentClose

-- | Strip comment markers from a 'String'.
stripMdComment :: String -> String
stripMdComment = stripEnds mdCommentOpenSpace mdCommentCloseSpace

-- | Convert 'Tokens' into @Haskell@ code.
--
-- Each 'Token' becomes a 'String' in a list.
--
-- These 'String's are concatenated in 'mdFromTokens'.
mdFromTokens' :: Config User -> Tokens -> [String]
mdFromTokens' (toInternalConfig -> Config{..}) blocks =
  intersperse [] . reverse $ intercalate "\n" . reverse <$> fromTokens 0 blocks []
 where
  fromTokens :: Int -> Tokens -> [[String]] -> [[String]]
  fromTokens _ [] res = res
  fromTokens curIndent (b : bs) res =
    case b of
      Indent{..} -> fromTokens n bs ([indentN n $ mdCommentOpenSpace <> _indent <> " " <> show n <> mdCommentCloseSpace] : res)
      Dedent -> fromTokens 0 bs ([mdCommentOpenSpace <> _dedent <> mdCommentCloseSpace] : res)
      Disabled{..} -> fromTokens 0 bs ([[_enable <> mdCommentCloseSpace]] <> [someLines] <> [[mdCommentOpenSpace <> _disable]] <> res)
      HaskellCode{..} -> fromTokens curIndent bs ((indentN curIndent <$> ([_mdHaskellCodeEnd] <> someLines <> [_mdHaskellCodeStart])) : res)
      Text{..} -> fromTokens curIndent bs (someLines : res)
      Comment{body = t :| ts} ->
        let ts' = t : ts
         in fromTokens curIndent bs $ [mdCommentClose] <> init ts' <> [mdCommentOpenSpace <> last ts'] : res

-- | Show the name of a constructor.
constructorName :: Data a => a -> String
constructorName x = showConstr (toConstr x)

-- | Remove empty lines from the beginning and the end of a list.
stripEmpties :: Eq a => [[a]] -> [[a]]
stripEmpties = stripValue mempty

-- | Check if a line without leading spaces is enclosed into given 'String's.
isEnclosedWith :: String -> String -> String -> Bool
isEnclosedWith start end (dropSpaces -> x) = x `startsWith` start && x `endsWith` end

-- | Check if a line is a @Markdown@ comment.
isMdComment :: String -> Bool
isMdComment = isEnclosedWith mdCommentOpenSpace mdCommentCloseSpace

-- | Count leading spaces in a 'String'.
countSpaces :: String -> Int
countSpaces x = length $ takeWhile (== ' ') x

-- | Strip empty lines in 'Tokens'.
--
-- Shift lines in 'HaskellCode' to the left by the minimal number of leading spaces
-- in non-empty lines.
stripTokens :: Tokens -> Tokens
stripTokens xs =
  ( \case
      Disabled{..} -> Disabled{someLines = stripEmpties someLines}
      -- normalize snippet
      HaskellCode{..} ->
        let ls = stripEmpties someLines
         in HaskellCode{someLines = drop (minimum (countSpaces <$> filter (not . null) ls)) <$> ls}
      Text{..} -> Text{someLines = stripEmpties someLines}
      Comment{..} -> Comment{body = fromList $ stripEmpties (toList body)}
      x -> x
  )
    <$> xs

-- | Convert 'Tokens' into @Markdown@ code.
--
-- Inverse of 'mdFromTokens'.
--
-- >>> (mdIntoTokens def $ mdFromTokens def exampleNonTexTokens) == exampleNonTexTokens
-- True
mdIntoTokens :: Config User -> String -> Tokens
mdIntoTokens (toInternalConfig -> conf@Config{..}) xs = tokens
 where
  tokens = mkIntoTokens toTokens xs
  toTokens :: State -> [(Int, String)] -> Tokens -> Tokens
  toTokens State{..} ((lineNumber, l) : ls) res@(r : rs)
    | inDisabled =
        -- enable
        if l `startsWith` (_enable <> mdCommentCloseSpace)
          then toTokens def ls res
          else -- copy lines
          toTokens def{inDisabled} ls $
            case r of
              Disabled{..} -> (r{someLines = l : someLines} : rs)
              _ -> err Disabled{}
    | inComment =
        if l `startsWith` mdCommentClose
          then -- finish comment
            toTokens def ls res
          else -- copy lines
          toTokens def{inComment} ls $
            case r of
              Comment{..} -> (r{body = l <| body} : rs)
              _ -> err Comment{}
    | inHaskellCode =
        if dropSpaces l `startsWith` _mdHaskellCodeEnd
          then -- finish snippet
            toTokens def ls res
          else -- copy lines
          toTokens def{inHaskellCode} ls $
            case r of
              HaskellCode{..} -> (r{someLines = l : someLines} : rs)
              _ -> err HaskellCode{}
    -- Doesn't matter if in text

    | -- comment on a single line
      isMdComment l =
        toTokens def ls $ parseToken conf r (stripMdComment l) lineNumber <> rs
    | -- start of a comment on multiple lines
      l `startsWith` mdCommentOpenSpace =
        let l' = dropLen mdCommentOpenSpace l
         in if
                | l' == _disable ->
                    toTokens def{inDisabled = True} ls (Disabled [] : res)
                | otherwise ->
                    toTokens def{inComment = True} ls $
                      Comment (l' :| []) : res
    | -- start of a haskell snippet
      dropSpaces l `startsWith` _mdHaskellCodeStart =
        toTokens def{inHaskellCode = True} ls (HaskellCode [] : res)
    -- Again matters if in a text
    | inText =
        toTokens def{inText} ls $
          case r of
            Text{..} -> Text{someLines = l : someLines} : rs
            _ -> err Text{}
    | otherwise =
        if null l
          then -- a blank line
            toTokens def ls res
          else -- start of a text
            toTokens def{inText = True} ls $ Text{someLines = [l]} : res
   where
    err = error . showTokenError lineNumber r
  toTokens _ _ res = res

-- | Convert 'Tokens' into @Haskell@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
--
-- - These are the relations between lines and tags with the default 'Config' values.
--
--     - @'{- LIMA_INDENT N -}'@ (@N@ is an 'Int') ~ 'Indent'
--     - @'{- LIMA_DEDENT -}'@ ~ 'Dedent'.
--     - Lines between @'{- LIMA_DISABLE -}'@ and @'{- LIMA_ENABLE -}'@ ~ 'Disabled'.
--
--     - Multiline comments starting with @'{-\\n'@ ~ 'Text'.
--
--         @
--         {-
--         line 1
--         -}
--         @
--
--     - Multiline comments starting with @'{- '@ ~ 'Comment'.
--
--         @
--         {- line 1
--         line 2
--         -}
--         @
--
--         Certain multiline haddocks start with @'{- |'@ so they are parsed into 'Comment's.
--
--     - Other lines ~ 'HaskellCode'.
--
--         @
--         a = 42
--         @
--
-- === __Example__
--
-- >>> pp $ hsFromTokens def exampleNonTexTokens
-- {- LIMA_INDENT 3 -}
-- <BLANKLINE>
-- a =
--   f b
-- <BLANKLINE>
-- {- LIMA_DEDENT -}
-- <BLANKLINE>
-- a =
--   f b
-- <BLANKLINE>
-- {- LIMA_INDENT 2 -}
-- <BLANKLINE>
-- {- LIMA_INDENT 5 -}
-- <BLANKLINE>
-- a =
--   f b
-- <BLANKLINE>
-- {- Hello,
-- <BLANKLINE>
-- world!
-- -}
-- <BLANKLINE>
-- {-
-- Line 1
-- Line 2
-- <BLANKLINE>
-- Line 1
-- Line 2
-- -}
-- <BLANKLINE>
-- {- LIMA_DISABLE -}
-- <BLANKLINE>
-- Line 1
-- Line 2
-- <BLANKLINE>
-- {- LIMA_ENABLE -}
hsFromTokens :: Config User -> Tokens -> String
hsFromTokens = mkFromTokens hsFromTokens'

-- | Open a @Haskell@ multi-line comment.
hsCommentOpen :: String
hsCommentOpen = "{-"

-- | Open a @Haskell@ multi-line comment plus a space.
hsCommentOpenSpace :: String
hsCommentOpenSpace = hsCommentOpen <> " "

-- | Close a @Haskell@ multi-line comment.
hsCommentClose :: String
hsCommentClose = "-}"

-- | A space plus close a @Haskell@ multi-line comment.
hsCommentCloseSpace :: String
hsCommentCloseSpace = " " <> hsCommentClose

-- | Convert 'Tokens' into @Haskell@ code.
--
-- Each 'Token' becomes a 'String' in a list.
--
-- These 'String's are concatenated in 'hsFromTokens'.
hsFromTokens' :: Config User -> Tokens -> [String]
hsFromTokens' (toInternalConfig -> Config{..}) blocks =
  intersperse [] . reverse $ intercalate "\n" . reverse <$> toHs blocks []
 where
  toHs :: Tokens -> [[String]] -> [[String]]
  toHs [] res = res
  toHs (b : bs) res =
    toHs bs $
      case b of
        Indent{..} -> [hsCommentOpenSpace <> _indent <> " " <> show n <> hsCommentCloseSpace] : res
        Dedent -> [hsCommentOpenSpace <> _dedent <> hsCommentCloseSpace] : res
        Disabled{..} ->
          [[hsCommentOpenSpace <> _enable <> hsCommentCloseSpace]]
            <> [someLines]
            <> [[hsCommentOpenSpace <> _disable <> hsCommentCloseSpace]]
            <> res
        HaskellCode{..} -> someLines : res
        Text{..} -> [hsCommentClose] <> someLines <> [hsCommentOpen] : res
        Comment{body = t :| ts} ->
          let ts' = t : ts
           in [hsCommentClose] <> init ts' <> [hsCommentOpenSpace <> last ts'] : res

-- | Drop leading spaces and drop at each end of a 'String' the number of characters as in the supplied prefix and suffix.
stripHsComment :: String -> String
stripHsComment = stripEnds hsCommentOpenSpace hsCommentCloseSpace

-- | Check if a line without leading zeros is a multi-line @Haskell@ comment
isHsComment :: String -> Bool
isHsComment = isEnclosedWith hsCommentOpenSpace hsCommentCloseSpace

-- | Convert 'Tokens' into @Haskell@ code.
--
-- Inverse of 'hsFromTokens'.
--
-- >>> (hsIntoTokens def $ hsFromTokens def exampleNonTexTokens) == exampleNonTexTokens
-- True
hsIntoTokens :: Config User -> String -> Tokens
hsIntoTokens (toInternalConfig -> conf@Config{..}) xs = tokens
 where
  tokens = mkIntoTokens toTokens xs
  toTokens :: State -> [(Int, String)] -> Tokens -> Tokens
  toTokens State{..} ((lineNumber, l) : ls) res@(r : rs)
    | inText =
        if l `startsWith` hsCommentClose
          then -- finish text
            toTokens def ls res
          else -- copy lines
          toTokens (def{inText}) ls $
            case r of
              Text{..} -> (r{someLines = l : someLines} : rs)
              _ -> err Text{}
    | inDisabled =
        -- enable
        if isHsComment l && stripHsComment l `startsWith` _enable
          then -- split text
            toTokens def ls res
          else -- copy lines
          toTokens def{inDisabled} ls $
            case r of
              Disabled{..} -> (r{someLines = l : someLines} : rs)
              _ -> err Disabled{}
    | inComment =
        if l `startsWith` hsCommentClose
          then -- finish comment
            toTokens def ls res
          else -- copy lines
          toTokens def{inComment} ls $
            case r of
              Comment{..} -> (r{body = l <| body} : rs)
              _ -> err Comment{}
    -- Doesn't matter if in a snippet

    | -- start of text
      l == hsCommentOpen =
        toTokens def{inText = True} ls (Text{someLines = []} : res)
    | -- comment on a single line
      isHsComment l =
        let l' = stripHsComment l
         in if l' `startsWith` _disable
              then toTokens def{inDisabled = True} ls (Disabled [] : res)
              else
                toTokens def ls $
                  parseToken conf r l' lineNumber <> rs
    | -- start of a comment on multiple lines
      l `startsWith` hsCommentOpenSpace =
        toTokens def{inComment = True} ls $
          Comment ((dropLen hsCommentOpenSpace l) :| []) : res
    -- Again matters if in a snippet
    | inHaskellCode =
        toTokens def{inHaskellCode} ls $
          case r of
            HaskellCode{..} -> HaskellCode{someLines = l : someLines} : rs
            _ -> err HaskellCode{}
    | otherwise =
        if null l
          then -- a blank line
            toTokens def ls res
          else -- start of a snippet
            toTokens def{inHaskellCode = True} ls (HaskellCode [l] : res)
   where
    err = error . showTokenError lineNumber r
  toTokens _ _ res = res

-- | Show a 'Dialect' as a file extension.
--
-- >>>showDialectExtension Lhs
-- "lhs"
showDialectExtension :: Dialect -> String
showDialectExtension = \case
  Hs -> "hs"
  Md -> "md"
  Lhs -> "lhs"
  TeX -> "tex"

-- | Show a 'Dialect' as a full name.
--
-- >>>showDialectName Lhs
-- "Literate Haskell"
showDialectName :: Dialect -> String
showDialectName = \case
  Hs -> "Haskell"
  Md -> "Markdown"
  Lhs -> "Literate Haskell"
  TeX -> "TeX"
