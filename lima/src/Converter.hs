-- |
-- == Terms #terms#
--
-- * @format@ - specific encoding of some information. See 'Format'.
-- * @document@ - 'T.Text' in a specific format, e.g., @Haskell@ (@.hs@) file.
-- * @document block@ - consecutive lines of a document.
-- * 'Token' - a representation of a document block as a @Haskell@ type.
-- * 'Tokens' - a list of 'Token's.
-- * @parser@ - a function that reads a document line by line and converts it to 'Token's. Example: 'hsToTokens'.
-- * @printer@ - a function that converts 'Tokens' to a document.
-- * @tag@ - a marker that affects how 'Tokens' are parsed.
--
--     * Each parser recognizes tags of a specific form.
--     * Tags can be represented as a wrapper and a name.
--
--         E.g., in @'% LIMA_DISABLE some text'@, a @TeX@ tag, the wrapper is @'% '@ and the name is @'LIMA_DISABLE some text'@.
--
--     * Parsers recognize the tag names that /start with/ tag names specified in a 'Config'.
--
--         E.g., in the example above, a parser will recognize the [_disable](#v:_disable) tag and will become disabled.
--
--     * When a parser is disabled, it copies lines verbatim into a 'Disabled' 'Token' and doesn't recognize any tags until it finds an [_enable](#v:_enable) tag.
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
--
--             % line 4
--             @
--
--         - Corresponding 'Tokens':
--
--             @
--             [
--               Text {manyLines = ["line2","line 1"]},
--               Comment {someLines = "line 4" :| ["", "line 3"]}
--             ]
--             @
--
--      - There are no leading or trailing empty lines inside of 'Tokens'.
module Converter (
  -- * Config
  Mode,
  User,
  Internal,
  Config (..),
  def,
  toInternalConfig,
  fromInternalConfig,

  -- ** Lenses
  disable,
  enable,
  indent,
  dedent,
  mdHaskellCodeStart,
  mdHaskellCodeEnd,
  texHaskellCodeStart,
  texHaskellCodeEnd,

  -- * microlens
  (&),
  (?~),

  -- * Format
  Format (..),
  convertTo,
  showFormatExtension,
  showFormatName,

  -- * Tokens
  Token (..),
  Tokens,
  selectFromTokens,
  selectToTokens,
  mergeTokens,
  stripTokens,
  normalizeTokens,

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
  lhsToTokens,
  hsToTokens,
  texToTokens,
  mdToTokens,

  -- * Examples
  exampleNonTexTokens',
  exampleNonTexTokens,
  exampleTexTokens,

  -- * Helpers
  stripEmpties,
  PrettyPrint (..),
) where

import Data.Char (isAlpha)
import Data.Data (Data (toConstr), showConstr)
import Data.Default (Default (def))
import Data.List (dropWhileEnd, intersperse)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList, (<|))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Lens.Micro (non, (&), (?~), (^.), (^?))
import Lens.Micro.TH (makeLenses)
import Text.Read (readMaybe)
import Text.Show qualified as T

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
--
-- It's possible to override the default values:
--
-- >>> pp ((def :: Config User) & disable ?~ "off" & enable ?~ "on" & indent ?~ "indent" & dedent ?~ "dedent")
-- Config {
--   _disable = Just "off",
--   _enable = Just "on",
--   _indent = Just "indent",
--   _dedent = Just "dedent",
--   _mdHaskellCodeStart = Just "```haskell",
--   _mdHaskellCodeEnd = Just "```",
--   _texHaskellCodeStart = Just "\\begin{code}",
--   _texHaskellCodeEnd = Just "\\end{code}"
-- }
data Config (a :: Mode') = Config
  { _disable :: Mode a T.Text
  -- ^
  -- Make parser ignore tags and just copy the following lines verbatim.
  --
  -- Set indentation to @0@.
  , _enable :: Mode a T.Text
  -- ^ Stop parser from ignoring tags.
  , _indent :: Mode a T.Text
  -- ^ Set code indentation to a given 'Int'.
  , _dedent :: Mode a T.Text
  -- ^ Set code indentation to @0@.
  , _mdHaskellCodeStart :: Mode a T.Text
  -- ^ Mark the start of a @Haskell@ code block in @Markdown@.
  , _mdHaskellCodeEnd :: Mode a T.Text
  -- ^ Mark the end of a @Haskell@ code block in @Markdown@.
  , _texHaskellCodeStart :: Mode a T.Text
  -- ^ Mark the start of a @Haskell@ code block in @TeX@.
  , _texHaskellCodeEnd :: Mode a T.Text
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

-- >>> lhsFromTokens def ex
-- "line 1\nline2\n\n% line 3\n\n% line 4\n"

-- | A class for prettyprinting data on multiple lines in haddocks.
--
-- It's not meant to be used outside of this library.
class Show a => PrettyPrint a where
  pp :: a -> Pretty String

instance PrettyPrint String where
  pp :: String -> Pretty String
  pp = Pretty . dropWhileEnd (== '\n')

instance PrettyPrint T.Text where
  pp :: T.Text -> Pretty String
  pp = pp . T.unpack

instance PrettyPrint (Config User) where
  pp :: Config User -> Pretty String
  pp (fromInternalConfig . toInternalConfig -> config) =
    pp $
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

-- | Convert a user 'Config' to an internal 'Config' with user-supplied values.
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

-- | A format of a document.
data Format
  = -- | @Haskell@
    Hs
  | -- | @Literate Haskell@
    Lhs
  | -- | @Markdown@
    Md
  | -- | @TeX@
    TeX

-- | Internal representation of a document.
--
-- A printer processes a list of 'Token's one by one.
--
-- A 'Token' can have:
--
-- - Action - how this 'Token' affects the subsequent 'Tokens'.
-- - Target - a type of 'Token's that are affected by this 'Token'.
-- - Range - the nearest 'Token' until which this 'Token' affects the subsequent 'Token's.
data Token
  = -- |
    -- - Action: set indentation to @n@.
    --
    -- - Target: 'HaskellCode'.
    --
    -- - Range: until 'Indent', 'Dedent', or 'Disabled'.
    Indent {n :: Int}
  | -- |
    -- - Action: set indentation to @0@.
    --
    -- - Target: 'HaskellCode'.
    --
    -- - Range: until 'Indent', 'Dedent', or 'Disabled'.
    Dedent
  | -- | A block that should be invisible when rendered outside of @.hs@.
    --
    -- - Action: set indentation to @0@.
    --
    -- - Target: 'HaskellCode'.
    --
    -- - Range: until 'Indent', 'Dedent', or 'Disabled'.
    Disabled {manyLines :: [T.Text]}
  | -- | Lines copied verbatim while a parser was in a @Haskell@ code block.
    HaskellCode {manyLines :: [T.Text]}
  | -- | Lines copied verbatim while a parser was in a text block.
    Text {someLines :: NonEmpty T.Text}
  | -- | Lines copied verbatim while a parser was in a comment block.
    Comment {someLines :: NonEmpty T.Text}
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

-- | Merge specific consecutive 'Tokens'.
--
-- >>> pp exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["   b = a 4","   a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "Hello from comments," :| []},
--   Comment {someLines = "world!" :| []},
--   Text {someLines = "world!" :| ["Hello from text,"]},
--   Text {someLines = "here!" :| ["And from"]}
-- ]
--
-- >>> pp $ mergeTokens exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["   b = a 4","   a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "world!" :| ["","Hello from comments,"]},
--   Text {someLines = "here!" :| ["And from","","world!","Hello from text,"]}
-- ]
mergeTokens :: Tokens -> Tokens
mergeTokens (t1@Text{} : t2@Text{} : ts) = mergeTokens $ Text{someLines = someLines t2 <> (T.empty <| someLines t1)} : ts
mergeTokens (t1@Comment{} : t2@Comment{} : ts) = mergeTokens $ Comment{someLines = someLines t2 <> (T.empty <| someLines t1)} : ts
mergeTokens (t : ts) = t : mergeTokens ts
mergeTokens ts = ts

-- | Example non-@TeX@ 'Tokens'. See 'exampleTexTokens'.
--
-- When printed to a @TeX@ document, these 'Tokens' can't be correctly parsed.
-- This is because they don't have necessary tags surrounding @Haskell@ code blocks.
--
-- >>> pp $ exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["   b = a 4","   a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "Hello from comments," :| []},
--   Comment {someLines = "world!" :| []},
--   Text {someLines = "world!" :| ["Hello from text,"]},
--   Text {someLines = "here!" :| ["And from"]}
-- ]
exampleNonTexTokens' :: Tokens
exampleNonTexTokens' =
  [ Indent 3
  , Disabled{manyLines = ["-- What's the answer?"]}
  , Indent 1
  , Indent 2
  , Text ("- Intermediate results" :| [])
  , HaskellCode ["   b = a 4", "   a = const 3"]
  , Dedent
  , HaskellCode ["answer = b * 14"]
  , Comment ("Hello from comments," :| [])
  , Comment ("world!" :| [])
  , Text ("world!" :| ["Hello from text,"])
  , Text ("here!" :| ["And from"])
  ]

-- | Normalized 'exampleNonTexTokens''.
--
-- >>>pp $ exampleNonTexTokens
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["b = a 4","a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "world!" :| ["","Hello from comments,"]},
--   Text {someLines = "here!" :| ["And from","","world!","Hello from text,"]}
-- ]
exampleNonTexTokens :: Tokens
exampleNonTexTokens = normalizeTokens exampleNonTexTokens'

-- | Select a printer function based on a given format.
selectFromTokens :: Config User -> Format -> Tokens -> T.Text
selectFromTokens config format =
  ( case format of
      Hs -> hsFromTokens
      Lhs -> lhsFromTokens
      Md -> mdFromTokens
      TeX -> texFromTokens
  )
    config

-- | Select a parser function based on a given format.
selectToTokens :: Config User -> Format -> T.Text -> Tokens
selectToTokens config format =
  ( case format of
      Hs -> hsToTokens
      Lhs -> lhsToTokens
      Md -> mdToTokens
      TeX -> texToTokens
  )
    config

-- | Compose a function that converts a document in one 'Format' to a document in another 'Format'.
convertTo :: Format -> Format -> Config User -> T.Text -> T.Text
convertTo a b config src = selectFromTokens config b $ selectToTokens config a src

-- | Escaped hash character
escapedHash :: T.Text
escapedHash = "\\#"

-- | Hash character
hash :: T.Text
hash = "#"

-- | Drop a prefix of a line with length of a given line
dropLen :: T.Text -> T.Text -> T.Text
dropLen x y = T.drop (T.length x) y

-- | Check if a list starts with a given list
startsWith :: T.Text -> T.Text -> Bool
startsWith = flip T.isPrefixOf

-- | Check if a list ends with a given list
endsWith :: T.Text -> T.Text -> Bool
endsWith = flip T.isSuffixOf

-- | Drop leading spaces and drop at each end of a 'T.Text' the number of characters as in the supplied prefix and suffix.
stripEnds :: T.Text -> T.Text -> T.Text -> T.Text
stripEnds prefix suffix x = T.dropEnd (T.length suffix) (dropLen prefix (stripSpaces x))

-- | Replace "\\#" with "#" in a 'T.Text' prefix.
lhsUnescapeHash :: T.Text -> T.Text
lhsUnescapeHash x = if x `startsWith` escapedHash then hash <> (dropLen escapedHash x) else x

-- | Replace "#" with "\\#" in a 'T.Text' prefix.
lhsEscapeHash :: T.Text -> T.Text
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
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["b = a 4","a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "world!" :| ["","Hello from comments,"]},
--   Text {someLines = "here!" :| ["And from","","world!","Hello from text,"]}
-- ]
normalizeTokens :: Tokens -> Tokens
normalizeTokens tokens = stripTokens $ mergeTokens $ tokens

-- | Compose a function from a 'T.Text' to 'Tokens'.
mkIntoTokens :: (State -> [(Int, T.Text)] -> [Token] -> [Token]) -> T.Text -> Tokens
mkIntoTokens toTokens xs = normalizeTokens (drop 1 $ reverse $ toTokens def (zip [1 ..] (T.lines xs)) [Dedent])

-- | Parse to a token contents of a multiline comment written on a single line.
--
-- Merge consecutive 'Comment's
parseToken :: Config Internal -> Token -> T.Text -> Int -> Tokens
parseToken Config{..} prev l lineNumber
  | l `startsWith` _indent =
      maybe
        (error $ "Expected a number at line: " <> show lineNumber)
        (\x -> [Indent (max 0 x), prev])
        (readMaybe @Int (T.unpack (dropLen _indent l)))
  | l == _dedent = [Dedent, prev]
  | otherwise =
      case prev of
        Comment{..} -> [Comment{someLines = l <| someLines}]
        _ -> [Comment (l :| []), prev]

-- | Show error with line number for a token.
errorExpectedToken :: (Data a1, Show a2, Show a3) => a2 -> a3 -> a1 -> a4
errorExpectedToken lineNumber lastToken expectedToken =
  error $
    ("Wrong state at line: " <> show lineNumber <> ".\n\n")
      <> ("Please, create an issue in the package repository.\n\n")
      <> ("Expected last token: " <> constructorName expectedToken <> "\n\n")
      <> ("Got last token: " <> show lastToken <> "\n\n")

-- | Strip the given value from the beginning and the end of a list.
stripList :: Eq a => a -> [a] -> [a]
stripList x = dropWhileEnd (== x) . dropWhile (== x)

-- | Pad a 'T.Text' with a given number of spaces
indentN :: Int -> T.Text -> T.Text
indentN x s = T.concat (replicate x " ") <> s

-- | Compose a function from 'Tokens' to a 'T.Text'.
mkFromTokens :: (Config User -> Tokens -> [T.Text]) -> Config User -> Tokens -> T.Text
mkFromTokens f' config = (<> "\n") . T.intercalate "\n" . f' config

-- | same as 'exampleNonTexTokens', but with @TeX@-specific tags that make @Haskell@ code blocks correctly parsable.
--
-- >>> pp $ exampleTexTokens
-- [
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "\\begin{code}" :| ["","Intermediate results"]},
--   HaskellCode {manyLines = ["b = a 4","a = const 3"]},
--   Text {someLines = "\\end{code}" :| []},
--   Dedent,
--   Text {someLines = "\\begin{code}" :| []},
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Text {someLines = "\\end{code}" :| []},
--   Comment {someLines = "world!" :| ["","Hello from comments,"]},
--   Text {someLines = "world!" :| ["Hello from text,"]}
-- ]
exampleTexTokens :: Tokens
exampleTexTokens =
  normalizeTokens $
    [ Disabled{manyLines = ["-- What's the answer?"]}
    , Indent 1
    , Indent 2
    , Text{someLines = "Intermediate results" :| []}
    , Text{someLines = "\\begin{code}" :| []}
    , HaskellCode ["   b = a 4", "   a = const 3"]
    , Text{someLines = "\\end{code}" :| []}
    , Dedent
    , Text{someLines = "\\begin{code}" :| []}
    , HaskellCode ["answer = b * 14"]
    , Text{someLines = "\\end{code}" :| []}
    , Comment ("Hello from comments," :| [])
    , Comment ("world!" :| [])
    , Text{someLines = "world!" :| ["Hello from text,"]}
    ]

-- | Convert 'Tokens' to @TeX@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
-- - These are the relations between document blocks and tokens when the default 'Config' values are used.
--
--     - @'% LIMA_INDENT N'@ (@N@ is an 'Int') ~ 'Indent'
--     - @'% LIMA_DEDENT'@ ~ 'Dedent'.
--     - Lines between and including @'% LIMA_DISABLE'@ and @'% LIMA_ENABLE'@ ~ 'Disabled'.
--
--     - Consecutive lines, either empty or starting with @'% '@ ~ 'Comment'.
--
--         @
--         % Hello,
--         % world!
--
--         % Hello,
--         % user!
--         @
--
--         - At least one line must have nonempty text after @'% '@
--
--     - Lines between possibly indented tags @'\\begin{code}'@ and @'\\end{code}'@ ~ 'HaskellCode'.
--
--     - Other lines ~ 'Text'.
--
-- === __Example__
--
-- >>> pp $ texFromTokens def exampleTexTokens
-- % LIMA_DISABLE
-- <BLANKLINE>
-- % -- What's the answer?
-- <BLANKLINE>
-- % LIMA_ENABLE
-- <BLANKLINE>
-- % LIMA_INDENT 1
-- <BLANKLINE>
-- % LIMA_INDENT 2
-- <BLANKLINE>
-- Intermediate results
-- <BLANKLINE>
-- \begin{code}
--   a = const 3
--   b = a 4
-- \end{code}
-- <BLANKLINE>
-- % LIMA_DEDENT
-- <BLANKLINE>
-- \begin{code}
-- answer = b * 14
-- \end{code}
-- <BLANKLINE>
-- % Hello from comments,
-- <BLANKLINE>
-- % world!
-- <BLANKLINE>
-- Hello from text,
-- world!
texFromTokens :: Config User -> Tokens -> T.Text
texFromTokens = mkFromTokens texFromTokens'

-- | Start a @TeX@ comment.
texComment :: T.Text
texComment = "%"

-- | Start a @TeX@ comment plus a space.
texCommentSpace :: T.Text
texCommentSpace = texComment <> " "

-- | Convert 'Tokens' to @TeX@ code.
--
-- Each 'Token' becomes a 'T.Text' in a list.
--
-- These 'T.Text's are concatenated in 'texFromTokens'.
texFromTokens' :: Config User -> Tokens -> [T.Text]
texFromTokens' (toInternalConfig -> Config{..}) tokens =
  dropEmpties $ reverse $ (T.intercalate "\n" . reverse <$> (fromTokens (Dedent : tokens) (0, [])))
 where
  fromTokens :: Tokens -> (Int, [[T.Text]]) -> [[T.Text]]
  fromTokens bs'@(_ : cur : bs) (curIndent, rs) =
    fromTokens (cur : bs) (translate curIndent bs' rs)
  fromTokens [_] (_, rs) = rs
  fromTokens _ _ = errorNotEnoughTokens TeX
  translate curIndent (prev : cur : _) rs =
    case cur of
      Indent{..} -> (n,) $ [texCommentSpace <> _indent <> " " <> T.pack (show n)] : [] : rs
      Dedent -> (0,) $ [texCommentSpace <> _dedent] : [] : rs
      Disabled{..} -> (0,) $ [[texCommentSpace <> _enable], [], (prependTexComment <$> manyLines), [], [texCommentSpace <> _disable], []] <> rs
      HaskellCode{..} ->
        (curIndent,) $
          (indentN curIndent <$> manyLines)
            : ( case prev of
                  Text{} -> rs
                  _ -> [] : rs
              )
      Text{..} ->
        (curIndent,) $
          toList someLines
            : ( case prev of
                  HaskellCode{} -> rs
                  _ -> [] : rs
              )
      Comment{someLines = t :| ts} -> (curIndent, (prependTexComment <$> (t : ts)) : [] : rs)
  translate _ _ _ = errorNotEnoughTokens TeX

errorNotEnoughTokens :: Format -> a
errorNotEnoughTokens format = error $ "Got not enough tokens when converting 'Tokens' to " <> showFormatName format

-- | Drop spaces at the start and the end of a 'T.Text'.
stripSpaces :: T.Text -> T.Text
stripSpaces = T.strip

-- | Prepend start of a @TeX@ comment (@'% '@) to a 'T.Text'.
prependTexComment :: T.Text -> T.Text
prependTexComment l
  | l == T.empty = l
  | otherwise = texCommentSpace <> l

-- | Drop start of a @TeX@ comment from a 'T.Text'.
dropTexComment :: Show a => T.Text -> a -> T.Text
dropTexComment l lineNumber
  | l `startsWith` texCommentSpace = dropLen texCommentSpace l
  | l == T.empty = l
  | otherwise =
      error $
        "Lines in a 'Disabled' block must either be empty or start with '% '\n\n"
          <> "Note that each 'Disabled' block must have at least one line starting with '% ' and having nonempty text after '% ' "
          <> ("The line " <> show lineNumber <> " must either be empty or start with '% '")

-- | Convert 'Tokens' to @TeX@ code.
--
-- Inverse of 'texFromTokens'.
--
-- >>> (texToTokens def $ texFromTokens def exampleTexTokens) == exampleTexTokens
-- True
texToTokens :: Config User -> T.Text -> Tokens
texToTokens (toInternalConfig -> conf@Config{..}) xs = tokens
 where
  tokens = mkIntoTokens toTokens xs
  toTokens :: State -> [(Int, T.Text)] -> Tokens -> Tokens
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
                    Disabled{..} -> (r{manyLines = dropTexComment l lineNumber : manyLines} : rs)
                    _ -> errorExpected Disabled{}
    | inHaskellCode =
        if
            | -- end of a snippet
              stripSpaces l `startsWith` _texHaskellCodeEnd ->
                toTokens def{inText = True} ls (Text{someLines = l :| []} : result)
            | otherwise ->
                toTokens def{inHaskellCode} ls $
                  case r of
                    HaskellCode{..} -> (r{manyLines = l : manyLines} : rs)
                    _ -> errorExpected HaskellCode{}
    | stripSpaces l `startsWith` _texHaskellCodeStart =
        toTokens def{inHaskellCode = True} ls $
          HaskellCode{manyLines = []}
            : case r of
              Text{..} -> Text{someLines = l <| someLines} : rs
              _ -> Text{someLines = l :| []} : result
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
            Text{..} -> Text{someLines = l <| someLines} : rs
            _ -> errorExpected Text{}
    | -- a blank line
      T.null l =
        case r of
          Comment{someLines} -> toTokens def{inComment} ls (Comment{someLines = l <| someLines} : rs)
          _ -> toTokens def ls result
    | -- start of a text
      otherwise =
        toTokens def{inText = True} ls $ Text{someLines = l :| []} : result
   where
    errorExpected = errorExpectedToken lineNumber r
  toTokens _ _ res = res

-- | Convert 'Tokens' to @Literate Haskell@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
--
-- - These are the relations between document blocks and tokens when the default 'Config' values are used.
--
--     - @'% LIMA_INDENT N'@ (@N@ is an 'Int') ~ 'Indent'.
--     - @'% LIMA_DEDENT'@ ~ 'Dedent'.
--     - Lines between and including @'% LIMA_DISABLE'@ and @'% LIMA_ENABLE'@ ~ 'Disabled'.
--
--         - There must be at least one nonempty line between these tags.
--
--     - Consecutive lines, either empty or starting with @'% '@ ~ 'Comment'.
--
--         @
--         % Hello,
--         % world!
--
--         % Hello,
--         % user!
--         @
--
--         - At least one line must have nonempty text after @'% '@
--
--     - Consecutive lines starting with @'> '@ ~ 'HaskellCode'.
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
-- % LIMA_DISABLE
-- <BLANKLINE>
-- % -- What's the answer?
-- <BLANKLINE>
-- % LIMA_ENABLE
-- <BLANKLINE>
-- % LIMA_INDENT 1
-- <BLANKLINE>
-- % LIMA_INDENT 2
-- <BLANKLINE>
-- - Intermediate results
-- >   a = const 3
-- >   b = a 4
-- <BLANKLINE>
-- % LIMA_DEDENT
-- <BLANKLINE>
-- > answer = b * 14
-- <BLANKLINE>
-- % Hello from comments,
-- <BLANKLINE>
-- % world!
-- <BLANKLINE>
-- Hello from text,
-- world!
-- <BLANKLINE>
-- And from
-- here!
lhsFromTokens :: Config User -> Tokens -> T.Text
lhsFromTokens config tokens = mkFromTokens lhsFromTokens' config tokens

-- | Start a @Literate Haskell@ comment.
lhsComment :: T.Text
lhsComment = "%"

-- | Start a @Literate Haskell@ comment plus a space.
lhsCommentSpace :: T.Text
lhsCommentSpace = lhsComment <> " "

-- | Start a @Literate Haskell@ line of @Haskell@ code.
lhsHsCode :: T.Text
lhsHsCode = ">"

-- | Start a @Literate Haskell@ line of @Haskell@ code plus a space.
lhsHsCodeSpace :: T.Text
lhsHsCodeSpace = lhsHsCode <> " "

-- | Prepend start of a @TeX@ comment (@'% '@) to a 'T.Text'.
prependLhsComment :: T.Text -> T.Text
prependLhsComment l
  | l == T.empty = l
  | otherwise = texCommentSpace <> l

-- | Convert 'Tokens' to @Literate Haskell@ code.
--
-- Each 'Token' becomes a 'T.Text' in a list.
--
-- These 'T.Text's are concatenated in 'lhsFromTokens'.
lhsFromTokens' :: Config User -> Tokens -> [T.Text]
lhsFromTokens' (toInternalConfig -> Config{..}) blocks =
  dropEmpties $ reverse (T.intercalate "\n" . reverse <$> (fromTokens (Dedent : blocks) (0, [])))
 where
  fromTokens :: Tokens -> (Int, [[T.Text]]) -> [[T.Text]]
  fromTokens bs'@(_ : cur : bs) (curIndent, rs) =
    fromTokens (cur : bs) (translate curIndent bs' rs)
  fromTokens [_] (_, rs) = rs
  fromTokens _ _ = errorNotEnoughTokens Lhs
  translate curIndent (prev : cur : _) rs =
    case cur of
      Indent{..} -> (n,) $ [lhsCommentSpace <> _indent <> " " <> T.pack (show n)] : [] : rs
      Dedent -> (0,) $ [lhsCommentSpace <> _dedent] : [] : rs
      Disabled{..} -> (0,) $ [lhsCommentSpace <> _enable] : [] : (prependLhsComment <$> manyLines) : [] : [lhsCommentSpace <> _disable] : [] : rs
      HaskellCode{..} ->
        (curIndent,) $
          ((lhsHsCodeSpace <>) . indentN curIndent <$> manyLines)
            : ( case prev of
                  Text{} -> rs
                  _ -> [] : rs
              )
      Text{..} ->
        (curIndent,) $
          toList (lhsEscapeHash <$> someLines)
            : ( case prev of
                  HaskellCode{} -> rs
                  _ -> [] : rs
              )
      Comment{someLines = t :| ts} -> (curIndent, (prependLhsComment <$> t : ts) : [] : rs)
  translate _ _ _ = errorNotEnoughTokens Lhs

-- | Drop start of a @TeX@ comment from a 'T.Text'.
dropLhsComment :: Show a => T.Text -> a -> T.Text
dropLhsComment l lineNumber
  | l `startsWith` lhsCommentSpace = dropLen lhsCommentSpace l
  | l == T.empty = l
  | otherwise = error $ "The line " <> show lineNumber <> " must either be empty or start with '% '"

-- | Convert 'Tokens' to @Markdown@ code.
--
-- Inverse of 'lhsFromTokens'.
--
-- >>> (lhsToTokens def $ lhsFromTokens def exampleNonTexTokens) == exampleNonTexTokens
-- True
lhsToTokens :: Config User -> T.Text -> Tokens
lhsToTokens (toInternalConfig -> conf@Config{..}) xs = tokens
 where
  tokens = mkIntoTokens toTokens xs
  toTokens :: State -> [(Int, T.Text)] -> Tokens -> Tokens
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
                    Disabled{..} -> (r{manyLines = dropLhsComment l lineNumber : manyLines} : rs)
                    _ -> errorExpected Disabled{}
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
                HaskellCode{..} -> (r{manyLines = l' : manyLines} : rs)
                _ -> HaskellCode{manyLines = [l']} : result
    | inText =
        toTokens def{inText} ls $
          case r of
            Text{..} -> Text{someLines = l <| someLines} : rs
            _ -> errorExpected Text{}
    | -- a blank line
      T.null l =
        case r of
          Comment{someLines} -> toTokens def{inComment} ls (Comment{someLines = l <| someLines} : rs)
          _ -> toTokens def ls result
    | -- start of a text
      otherwise =
        toTokens def{inText = True} ls $ Text{someLines = l :| []} : result
   where
    errorExpected = error . errorExpectedToken lineNumber r
  toTokens _ _ res = res

-- | Convert 'Tokens' to @Markdown@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
--
-- - These are the relations between document blocks and tokens when the default 'Config' values are used.
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
--     - Multiline comments starting with @'<!-- {text}'@ where @{text}@ is nonempty text ~ 'Comment'.
--
--         @
--         <!-- line 1
--         line 2
--         --\>
--         @
--
--         - Consecutive 'Comment's are merged into a single 'Comment'.
--
--     - Possibly indented block starting with @\'```haskell\'@ and ending with @'```'@ ~ 'HaskellCode'.
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
-- <!-- LIMA_DISABLE
-- <BLANKLINE>
-- -- What's the answer?
-- <BLANKLINE>
-- LIMA_ENABLE -->
-- <BLANKLINE>
--  <!-- LIMA_INDENT 1 -->
-- <BLANKLINE>
--   <!-- LIMA_INDENT 2 -->
-- <BLANKLINE>
-- - Intermediate results
-- <BLANKLINE>
--   ```haskell
--   a = const 3
--   b = a 4
--   ```
-- <BLANKLINE>
-- <!-- LIMA_DEDENT -->
-- <BLANKLINE>
-- ```haskell
-- answer = b * 14
-- ```
-- <BLANKLINE>
-- <!-- Hello from comments,
-- <BLANKLINE>
-- world!
-- -->
-- <BLANKLINE>
-- Hello from text,
-- world!
-- <BLANKLINE>
-- And from
-- here!
mdFromTokens :: Config User -> Tokens -> T.Text
mdFromTokens = mkFromTokens mdFromTokens'

-- | Open a @Markdown@ comment.
mdCommentOpen :: T.Text
mdCommentOpen = "<!--"

-- | Close a @Markdown@ comment.
mdCommentClose :: T.Text
mdCommentClose = "-->"

-- | Open a @Markdown@ comment plus a space.
mdCommentOpenSpace :: T.Text
mdCommentOpenSpace = mdCommentOpen <> " "

-- | A space plus close a @Markdown@ comment.
mdCommentCloseSpace :: T.Text
mdCommentCloseSpace = " " <> mdCommentClose

-- | Strip comment markers from a 'T.Text'.
stripMdComment :: T.Text -> T.Text
stripMdComment = stripEnds mdCommentOpenSpace mdCommentCloseSpace

-- | Convert 'Tokens' to @Haskell@ code.
--
-- Each 'Token' becomes a 'T.Text' in a list.
--
-- These 'T.Text's are concatenated in 'mdFromTokens'.
mdFromTokens' :: Config User -> Tokens -> [T.Text]
mdFromTokens' (toInternalConfig -> Config{..}) blocks =
  intersperse T.empty . reverse $ T.intercalate "\n" . reverse <$> fromTokens 0 blocks []
 where
  fromTokens :: Int -> Tokens -> [[T.Text]] -> [[T.Text]]
  fromTokens _ [] res = res
  fromTokens curIndent (b : bs) res =
    case b of
      Indent{..} -> fromTokens n bs ([indentN n $ mdCommentOpenSpace <> _indent <> " " <> T.pack (show n) <> mdCommentCloseSpace] : res)
      Dedent -> fromTokens 0 bs ([mdCommentOpenSpace <> _dedent <> mdCommentCloseSpace] : res)
      Disabled{..} -> fromTokens 0 bs ([[_enable <> mdCommentCloseSpace]] <> [manyLines] <> [[mdCommentOpenSpace <> _disable]] <> res)
      HaskellCode{..} -> fromTokens curIndent bs ((indentN curIndent <$> ([_mdHaskellCodeEnd] <> manyLines <> [_mdHaskellCodeStart])) : res)
      Text{..} -> fromTokens curIndent bs (toList someLines : res)
      Comment{someLines = t :| ts} ->
        let ts' = t : ts
         in fromTokens curIndent bs $ [mdCommentClose] <> init ts' <> [mdCommentOpenSpace <> last ts'] : res

-- | Show the name of a constructor.
constructorName :: Data a => a -> String
constructorName x = showConstr (toConstr x)

-- | Remove empty lines from the beginning and the end of a list.
stripEmpties :: [T.Text] -> [T.Text]
stripEmpties = stripList T.empty

dropEmpties :: [T.Text] -> [T.Text]
dropEmpties = dropWhile (== T.empty)

-- | Check if a line without leading spaces is surrounded by the given 'T.Text's.
isEnclosedWith :: T.Text -> T.Text -> T.Text -> Bool
isEnclosedWith start end (stripSpaces -> x) = x `startsWith` start && x `endsWith` end

-- | Check if a line is a @Markdown@ comment.
isMdComment :: T.Text -> Bool
isMdComment = isEnclosedWith mdCommentOpenSpace mdCommentCloseSpace

-- | Count leading spaces in a 'T.Text'.
countSpaces :: T.Text -> Int
countSpaces x = T.length $ T.takeWhile (== ' ') x

-- | Strip empty lines an leading spaces in 'Tokens'.
--
-- - Remove empty lines in 'Tokens'.
-- - Shift lines in 'HaskellCode' to the left by the minimal number of leading spaces in nonempty lines.
--
-- >>> pp exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["   b = a 4","   a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "Hello from comments," :| []},
--   Comment {someLines = "world!" :| []},
--   Text {someLines = "world!" :| ["Hello from text,"]},
--   Text {someLines = "here!" :| ["And from"]}
-- ]
--
-- >>> pp $ stripTokens exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["b = a 4","a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "Hello from comments," :| []},
--   Comment {someLines = "world!" :| []},
--   Text {someLines = "world!" :| ["Hello from text,"]},
--   Text {someLines = "here!" :| ["And from"]}
-- ]
stripTokens :: Tokens -> Tokens
stripTokens xs =
  ( \case
      Disabled{..} -> Disabled{manyLines = stripEmpties manyLines}
      HaskellCode{..} ->
        let ls = stripEmpties manyLines
         in HaskellCode{manyLines = T.drop (minimum (countSpaces <$> filter (not . T.null) ls)) <$> ls}
      Text{..} -> Text{someLines = fromList $ stripEmpties (toList someLines)}
      Comment{..} -> Comment{someLines = fromList $ stripEmpties (toList someLines)}
      x -> x
  )
    <$> xs

-- | Convert 'Tokens' to @Markdown@ code.
--
-- Inverse of 'mdFromTokens'.
--
-- >>> (mdToTokens def $ mdFromTokens def exampleNonTexTokens) == exampleNonTexTokens
-- True
mdToTokens :: Config User -> T.Text -> Tokens
mdToTokens (toInternalConfig -> conf@Config{..}) xs = tokens
 where
  tokens = mkIntoTokens toTokens xs
  toTokens :: State -> [(Int, T.Text)] -> Tokens -> Tokens
  toTokens State{..} ((lineNumber, l) : ls) res@(r : rs)
    | inDisabled =
        -- enable
        if l `startsWith` (_enable <> mdCommentCloseSpace)
          then toTokens def ls res
          else -- copy lines
          toTokens def{inDisabled} ls $
            case r of
              Disabled{..} -> (r{manyLines = l : manyLines} : rs)
              _ -> errorExpected Disabled{}
    | inComment =
        if l `startsWith` mdCommentClose
          then -- finish comment
            toTokens def ls res
          else -- copy lines
          toTokens def{inComment} ls $
            case r of
              Comment{..} -> (r{someLines = l <| someLines} : rs)
              _ -> errorExpected Comment{}
    | inHaskellCode =
        if stripSpaces l `startsWith` _mdHaskellCodeEnd
          then -- finish snippet
            toTokens def ls res
          else -- copy lines
          toTokens def{inHaskellCode} ls $
            case r of
              HaskellCode{..} -> (r{manyLines = l : manyLines} : rs)
              _ -> errorExpected HaskellCode{}
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
                | T.null l' -> error $ errorEmptyCommentAt lineNumber
                | otherwise ->
                    toTokens def{inComment = True} ls $
                      Comment (l' :| []) : res
    | -- start of a haskell snippet
      stripSpaces l `startsWith` _mdHaskellCodeStart =
        toTokens def{inHaskellCode = True} ls (HaskellCode [] : res)
    -- Again matters if in a text
    | inText =
        toTokens def{inText} ls $
          case r of
            Text{..} -> Text{someLines = l <| someLines} : rs
            _ -> errorExpected Text{}
    | otherwise =
        if
            | T.null l ->
                -- skip
                toTokens def ls res
            | otherwise ->
                -- start a text
                toTokens def{inText = True} ls $ Text{someLines = l :| []} : res
   where
    errorExpected = error . errorExpectedToken lineNumber r
  toTokens _ _ res = res

-- | Convert 'Tokens' to @Haskell@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
--
-- - These are the relations between document blocks and tokens when the default 'Config' values are used.
--
--     - @'{- LIMA_INDENT N -}'@ (@N@ is an 'Int') ~ 'Indent'.
--     - @'{- LIMA_DEDENT -}'@ ~ 'Dedent'.
--     - Lines between and including @'{- LIMA_DISABLE -}'@ and @'{- LIMA_ENABLE -}'@ ~ 'Disabled'.
--
--     - Multiline comment starting with @'{-\\n'@ ~ 'Text'.
--
--         @
--         {-
--         line 1
--         -}
--         @
--
--         - Consecutive 'Text's are merged into a single 'Text'.
--         - There must be at list one nonempty line inside this comment.
--
--     - Multiline comment starting with @'{- '@ where @<text>@ is nonempty text ~ 'Comment'.
--
--         @
--         {- line 1
--         line 2
--         -}
--         @
--
--         - Consecutive 'Comment's are merged into a single 'Comment'.
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
-- {- LIMA_DISABLE -}
-- <BLANKLINE>
-- -- What's the answer?
-- <BLANKLINE>
-- {- LIMA_ENABLE -}
-- <BLANKLINE>
-- {- LIMA_INDENT 1 -}
-- <BLANKLINE>
-- {- LIMA_INDENT 2 -}
-- <BLANKLINE>
-- {-
-- - Intermediate results
-- -}
-- <BLANKLINE>
-- a = const 3
-- b = a 4
-- <BLANKLINE>
-- {- LIMA_DEDENT -}
-- <BLANKLINE>
-- answer = b * 14
-- <BLANKLINE>
-- {- Hello from comments,
-- <BLANKLINE>
-- world!
-- -}
-- <BLANKLINE>
-- {-
-- Hello from text,
-- world!
-- <BLANKLINE>
-- And from
-- here!
-- -}
hsFromTokens :: Config User -> Tokens -> T.Text
hsFromTokens = mkFromTokens hsFromTokens'

-- | Open a @Haskell@ multi-line comment.
hsCommentOpen :: T.Text
hsCommentOpen = "{-"

-- | Open a @Haskell@ multi-line comment plus a space.
hsCommentOpenSpace :: T.Text
hsCommentOpenSpace = hsCommentOpen <> " "

-- | Close a @Haskell@ multi-line comment.
hsCommentClose :: T.Text
hsCommentClose = "-}"

-- | A space plus close a @Haskell@ multi-line comment.
hsCommentCloseSpace :: T.Text
hsCommentCloseSpace = " " <> hsCommentClose

-- | Convert 'Tokens' to @Haskell@ code.
--
-- Each 'Token' becomes a 'T.Text' in a list.
--
-- These 'T.Text's are concatenated in 'hsFromTokens'.
hsFromTokens' :: Config User -> Tokens -> [T.Text]
hsFromTokens' (toInternalConfig -> Config{..}) blocks =
  intersperse T.empty . reverse $ T.intercalate "\n" . reverse <$> toHs blocks []
 where
  toHs :: Tokens -> [[T.Text]] -> [[T.Text]]
  toHs [] res = res
  toHs (b : bs) res =
    toHs bs $
      case b of
        Indent{..} -> [hsCommentOpenSpace <> _indent <> " " <> T.pack (T.show n) <> hsCommentCloseSpace] : res
        Dedent -> [hsCommentOpenSpace <> _dedent <> hsCommentCloseSpace] : res
        Disabled{..} ->
          [[hsCommentOpenSpace <> _enable <> hsCommentCloseSpace]]
            <> [manyLines]
            <> [[hsCommentOpenSpace <> _disable <> hsCommentCloseSpace]]
            <> res
        HaskellCode{..} -> manyLines : res
        Text{..} -> [hsCommentClose] <> toList someLines <> [hsCommentOpen] : res
        Comment{someLines = t :| ts} ->
          let ts' = t : ts
           in [hsCommentClose] <> init ts' <> [hsCommentOpenSpace <> last ts'] : res

-- | Drop leading spaces and drop at each end of a 'T.Text' the number of characters as in the supplied prefix and suffix.
stripHsComment :: T.Text -> T.Text
stripHsComment = stripEnds hsCommentOpenSpace hsCommentCloseSpace

-- | Check if a line without leading zeros is a multi-line @Haskell@ comment
isHsComment :: T.Text -> Bool
isHsComment = isEnclosedWith hsCommentOpenSpace hsCommentCloseSpace

-- | Show error with line number for a token.
-- errorEmptyCommentAt :: Int -> String
errorEmptyCommentAt :: Show a1 => a1 -> a2
errorEmptyCommentAt lineNumber =
  error $
    ("Expected a 'Comment' at line " <> show lineNumber <> ".\n\n")
      <> "However, there are no characters after '{- '.\n\n"
      <> "Please, write there something after '{- '."

-- | Convert 'Tokens' to @Haskell@ code.
--
-- Inverse of 'hsFromTokens'.
--
-- >>> (hsToTokens def $ hsFromTokens def exampleNonTexTokens) == exampleNonTexTokens
-- True
hsToTokens :: Config User -> T.Text -> Tokens
hsToTokens (toInternalConfig -> conf@Config{..}) xs = tokens
 where
  tokens = mkIntoTokens toTokens xs
  toTokens :: State -> [(Int, T.Text)] -> Tokens -> Tokens
  toTokens State{..} ((lineNumber, l) : ls) res@(r : rs)
    | inText =
        if
            | l `startsWith` hsCommentClose ->
                case r of
                  Text{someLines}
                    | stripEmpties (toList someLines) == [] ->
                        error $
                          ("No text in a 'Text' token ending at line " <> show lineNumber <> ".\n\n")
                            <> "Please, write some text between '{-\\n' and '\\n-}'."
                    | otherwise -> toTokens def ls res
                  _ -> errorExpected Text{}
            | otherwise ->
                -- copy lines
                toTokens (def{inText}) ls $
                  case r of
                    Text{..} -> (r{someLines = l <| someLines} : rs)
                    _ -> errorExpected Text{}
    | inDisabled =
        if
            | isHsComment l && stripHsComment l `startsWith` _enable ->
                -- enable
                toTokens def ls res
            | otherwise ->
                -- copy lines
                toTokens def{inDisabled} ls $
                  case r of
                    Disabled{..} -> (r{manyLines = l : manyLines} : rs)
                    _ -> errorExpected Disabled{}
    | inComment =
        if
            | -- finish comment
              l `startsWith` hsCommentClose ->
                case r of
                  Comment{someLines}
                    | stripEmpties (toList someLines) == [] ->
                        error $
                          ("No text in a 'Comment' token ending at line " <> show lineNumber <> ".\n\n")
                            <> "Please, write some text between '{- ' and '\\n-}'."
                    | otherwise -> toTokens def ls res
                  _ -> errorExpected Comment{}
            | -- copy lines
              otherwise ->
                toTokens def{inComment} ls $
                  case r of
                    Comment{..} -> (r{someLines = l <| someLines} : rs)
                    _ -> errorExpected Comment{}
    -- Doesn't matter if in a snippet

    | -- start of text
      l == hsCommentOpen =
        toTokens def{inText = True} ls (Text{someLines = T.empty :| []} : res)
    | -- comment on a single line
      isHsComment l =
        let l' = stripHsComment l
         in if
                | l' `startsWith` _disable -> toTokens def{inDisabled = True} ls (Disabled [] : res)
                -- \| null l' -> error
                | otherwise -> toTokens def ls $ parseToken conf r l' lineNumber <> rs
    | -- start of a comment on multiple lines
      l `startsWith` hsCommentOpenSpace =
        let l' = dropLen hsCommentOpenSpace l
         in if
                | T.null l' -> errorEmptyCommentAt lineNumber
                | otherwise ->
                    toTokens def{inComment = True} ls $
                      Comment (l' :| []) : res
    -- Again matters if in a snippet
    | inHaskellCode =
        toTokens def{inHaskellCode} ls $
          case r of
            HaskellCode{..} -> HaskellCode{manyLines = l : manyLines} : rs
            _ -> errorExpected HaskellCode{}
    | -- a blank line
      T.null l =
        toTokens def ls res
    | -- start of a snippet
      otherwise =
        toTokens def{inHaskellCode = True} ls (HaskellCode [l] : res)
   where
    errorExpected = error . errorExpectedToken lineNumber r
  toTokens _ _ res = res

-- | Show a 'Format' as a file extension.
--
-- >>>showFormatExtension Lhs
-- "lhs"
showFormatExtension :: Format -> String
showFormatExtension = \case
  Hs -> "hs"
  Md -> "md"
  Lhs -> "lhs"
  TeX -> "tex"

-- | Show a 'Format' as a full name.
--
-- >>>showFormatName Lhs
-- "Literate Haskell"
showFormatName :: Format -> String
showFormatName = \case
  Hs -> "Haskell"
  Md -> "Markdown"
  Lhs -> "Literate Haskell"
  TeX -> "TeX"
