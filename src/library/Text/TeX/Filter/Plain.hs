----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Filter.Plain
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Data maps containing symbols and diacritics defined in the
-- Plain TeX format.
----------------------------------------------------------------------

module Text.TeX.Filter.Plain
  ( symbols
  , diacritics
  , dbldiacritics
  ) where

import Data.Map.Strict (Map, fromList)


-- | Symbols defined in the Plain TeX format.
--
-- We do not distinguish math mode from text mode at this point (yet).
symbols :: Map String String
symbols = fromList $
  [ -- text symbols
    ("%", "%")                -- PERCENT SIGN
  , ("&", "&")                -- AMPERSAND
  , ("#", "#")                -- NUMBER SIGN
  , ("$", "$")                -- DOLLAR SIGN
  , ("_", "_")                -- LOW LINE
  , ("i", "\x0131")           -- LATIN SMALL LETTER DOTLESS I
  , ("j", "\x0237")           -- LATIN SMALL LETTER DOTLESS J
  , ("ss", "\x00DF")          -- LATIN SMALL LETTER SHARP S
  , ("aa", "\x00E5")          -- LATIN SMALL LETTER A WITH RING ABOVE
  , ("AA", "\x00C5")          -- LATIN CAPITAL LETTER A WITH RING ABOVE
  , ("ae", "\x00E6")          -- LATIN SMALL LETTER AE
  , ("AE", "\x00C6")          -- LATIN CAPITAL LETTER AE
  , ("oe", "\x0153")          -- LATIN SMALL LIGATURE OE
  , ("OE", "\x0152")          -- LATIN CAPITAL LIGATURE OE
  , ("o", "\x00D8")           -- LATIN CAPITAL LETTER O WITH STROKE
  , ("O", "\x00F8")           -- LATIN SMALL LETTER O WITH STROKE
  , ("l", "\x0142")           -- LATIN SMALL LETTER L WITH STROKE
  , ("L", "\x0141")           -- LATIN CAPITAL LETTER L WITH STROKE
  , ("dag", "\x2020")         -- DAGGER
  , ("ddag", "\x2021")        -- DOUBLE DAGGER
  , ("S", "\x00A7")           -- SECTION SIGN
  , ("P", "\x00B6")           -- PILCROW SIGN
  , ("Orb", "\x25CB")         -- WHITE CIRCLE
  , ("copyright", "\x00A9")   -- COPYRIGHT SIGN
  , ("dots", "\x2026")        -- HORIZONTAL ELLIPSIS
    -- text symbols, multiple characters
  , ("TeX", "TeX")
    -- math symbols: greek letters
  , ("alpha", "\x03B1")       -- GREEK SMALL LETTER ALPHA
  , ("beta", "\x03B2")        -- GREEK SMALL LETTER BETA
  , ("gamma", "\x03B3")       -- GREEK SMALL LETTER GAMMA
  , ("delta", "\x03B4")       -- GREEK SMALL LETTER DELTA
  , ("epsilon", "\x03F5")     -- GREEK LUNATE EPSILON SYMBOL
  , ("varepsilon", "\x03B5")  -- GREEK SMALL LETTER EPSILON
  , ("zeta", "\x03B6")        -- GREEK SMALL LETTER ZETA
  , ("eta", "\x03B7")         -- GREEK SMALL LETTER ETA
  , ("theta", "\x03B8")       -- GREEK SMALL LETTER THETA
  , ("vartheta", "\x03D1")    -- GREEK THETA SYMBOL
  , ("iota", "\x03B9")        -- GREEK SMALL LETTER IOTA
  , ("kappa", "\x03BA")       -- GREEK SMALL LETTER KAPPA
  , ("lambda", "\x03BB")      -- GREEK SMALL LETTER LAMDA
  , ("mu", "\x03BC")          -- GREEK SMALL LETTER MU
  , ("nu", "\x03BD")          -- GREEK SMALL LETTER NU
  , ("xi", "\x03BE")          -- GREEK SMALL LETTER XI
  , ("pi", "\x03C0")          -- GREEK SMALL LETTER PI
  , ("varpi", "\x03D6")       -- GREEK PI SYMBOL
  , ("rho", "\x03C1")         -- GREEK SMALL LETTER RHO
  , ("varrho", "\x03F1")      -- GREEK RHO SYMBOL
  , ("sigma", "\x03C3")       -- GREEK SMALL LETTER SIGMA
  , ("varsigma", "\x03C2")    -- GREEK SMALL LETTER FINAL SIGMA
  , ("tau", "\x03C4")         -- GREEK SMALL LETTER TAU
  , ("upsilon", "\x03C5")     -- GREEK SMALL LETTER UPSILON
  , ("phi", "\x03D5")         -- GREEK PHI SYMBOL
  , ("varphi", "\x03C6")      -- GREEK SMALL LETTER PHI
  , ("chi", "\x03C7")         -- GREEK SMALL LETTER CHI
  , ("psi", "\x03C8")         -- GREEK SMALL LETTER PSI
  , ("omega", "\x03C9")       -- GREEK SMALL LETTER OMEGA
  , ("Gamma", "\x0393")       -- GREEK CAPITAL LETTER GAMMA
  , ("Delta", "\x0394")       -- GREEK CAPITAL LETTER DELTA
  , ("Theta", "\x0398")       -- GREEK CAPITAL LETTER THETA
  , ("Lambda", "\x039B")      -- GREEK CAPITAL LETTER LAMDA
  , ("Xi", "\x039E")          -- GREEK CAPITAL LETTER XI
  , ("Pi", "\x03A0")          -- GREEK CAPITAL LETTER PI
  , ("Sigma", "\x03A3")       -- GREEK CAPITAL LETTER SIGMA
  , ("Upsilon", "\x03A5")     -- GREEK CAPITAL LETTER UPSILON
  , ("Phi", "\x03A6")         -- GREEK CAPITAL LETTER PHI
  , ("Psi", "\x03A8")         -- GREEK CAPITAL LETTER PSI
  , ("Omega", "\x03A9")       -- GREEK CAPITAL LETTER OMEGA
  -- math symbols
  , ("aleph", "\x2135")       -- ALEF SYMBOL
  , ("imath", "\x0131")       -- LATIN SMALL LETTER DOTLESS I
  , ("jmath", "\x0237")       -- LATIN SMALL LETTER DOTLESS J
  , ("ell", "\x2113")         -- SCRIPT SMALL L
  , ("hbar", "\x210F")        -- PLANCK CONSTANT OVER TWO PI
  , ("surd", "\x221A")        -- SQUARE ROOT
  , ("angle", "\x2220")       -- ANGLE
  , ("wp", "\x2118")          -- SCRIPT CAPITAL P
  , ("Re", "\x211C")          -- BLACK-LETTER CAPITAL R
  , ("Im", "\x2111")          -- BLACK-LETTER CAPITAL I
  , ("partial", "\x2202")     -- PARTIAL DIFFERENTIAL
  , ("infty", "\x221E")       -- INFINITY
  , ("prime", "\x2032")       -- PRIME
  , ("emptyset", "\x2205")    -- EMPTY SET
  , ("nabla", "\x2207")       -- NABLA
  , ("top", "\x22A4")         -- DOWN TACK
  , ("bot", "\x22A5")         -- UP TACK
  , ("forall", "\x2200")      -- FOR ALL
  , ("exists", "\x2203")      -- THERE EXISTS
  , ("neg", "\x00AC")         -- NOT SIGN
  , ("lnot", "\x00AC")        -- NOT SIGN
  , ("coprod", "\x2210")      -- N-ARY COPRODUCT
  , ("bigvee", "\x22C1")      -- N-ARY LOGICAL OR
  , ("bigwedge", "\x22C0")    -- N-ARY LOGICAL AND
  , ("biguplus", "\x228E")    -- MULTISET UNION
  , ("bigcap", "\x22C2")      -- N-ARY INTERSECTION
  , ("bigcup", "\x22C3")      -- N-ARY UNION
  , ("int", "\x222B")         -- INTEGRAL
  , ("intop", "\x222B")       -- INTEGRAL
  , ("prod", "\x220F")        -- N-ARY PRODUCT
  , ("sum", "\x2211")         -- N-ARY SUMMATION
  , ("bigotimes", "\x2297")   -- CIRCLED TIMES
  , ("bigoplus", "\x2295")    -- CIRCLED PLUS
  , ("bigodot", "\x2299")     -- CIRCLED DOT OPERATOR
  , ("oint", "\x222E")        -- CONTOUR INTEGRAL
  , ("ointop", "\x222E")      -- CONTOUR INTEGRAL
  , ("bigsqcup", "\x2294")    -- SQUARE CUP
  , ("smallint", "\x222B")    -- INTEGRAL
  , ("wedge", "\x2227")       -- LOGICAL AND
  , ("land", "\x2227")        -- LOGICAL AND
  , ("vee", "\x2228")         -- LOGICAL OR
  , ("lor", "\x2228")         -- LOGICAL OR
  , ("cap", "\x2229")         -- INTERSECTION
  , ("cup", "\x222A")         -- UNION
  , ("dagger", "\x2020")      -- DAGGER
  , ("ddagger", "\x2021")     -- DOUBLE DAGGER
  , ("sqcap", "\x2293")       -- SQUARE CAP
  , ("sqcup", "\x2294")       -- SQUARE CUP
  , ("uplus", "\x228E")       -- MULTISET UNION
  , ("amalg", "\x2210")       -- N-ARY COPRODUCT
  , ("diamond", "\x22C4")     -- DIAMOND OPERATOR
  , ("bullet", "\x2219")      -- BULLET OPERATOR
  , ("wr", "\x2240")          -- WREATH PRODUCT
  , ("div", "\x00F7")         -- DIVISION SIGN
  , ("odot", "\x2299")        -- CIRCLED DOT OPERATOR
  , ("oslash", "\x2298")      -- CIRCLED DIVISION SLASH
  , ("otimes", "\x2297")      -- CIRCLED TIMES
  , ("ominus", "\x2296")      -- CIRCLED MINUS
  , ("oplus", "\x2295")       -- CIRCLED PLUS
  , ("mp", "\x2213")          -- MINUS-OR-PLUS SIGN
  , ("pm", "\x00B1")          -- PLUS-MINUS SIGN
  , ("circ", "\x2218")        -- RING OPERATOR
  , ("bigcirc", "\x25CB")     -- WHITE CIRCLE
  , ("setminus", "\x2216")    -- SET MINUS
  , ("cdot", "\x22C5")        -- DOT OPERATOR
  , ("ast", "\x2217")         -- ASTERISK OPERATOR
  , ("times", "\x00D7")       -- MULTIPLICATION SIGN
  , ("star", "\x22C6")        -- STAR OPERATOR
  , ("propto", "\x221D")      -- PROPORTIONAL TO
  , ("sqsubseteq", "\x228F")  -- SQUARE IMAGE OF
  , ("sqsupseteq", "\x2291")  -- SQUARE IMAGE OF OR EQUAL TO
  , ("parallel", "\x2225")    -- PARALLEL TO
  , ("mid", "\x2223")         -- DIVIDES
  , ("dashv", "\x22A3")       -- LEFT TACK
  , ("vdash", "\x22A2")       -- RIGHT TACK
  , ("nearrow", "\x2197")     -- NORTH EAST ARROW
  , ("searrow", "\x2198")     -- SOUTH EAST ARROW
  , ("nwarrow", "\x2196")     -- NORTH WEST ARROW
  , ("swarrow", "\x2199")     -- SOUTH WEST ARROW
  , ("neq", "\x2260")         -- NOT EQUAL TO
  , ("ne", "\x2260")          -- NOT EQUAL TO
  , ("leq", "\x2264")         -- LESS-THAN OR EQUAL TO
  , ("le", "\x2264")          -- LESS-THAN OR EQUAL TO
  , ("geq", "\x2265")         -- GREATER-THAN OR EQUAL TO
  , ("ge", "\x2265")          -- GREATER-THAN OR EQUAL TO
  , ("succ", "\x227B")        -- SUCCEEDS
  , ("prec", "\x227A")        -- PRECEDES
  , ("approx", "\x2248")      -- ALMOST EQUAL TO
  , ("succeq", "\x2AB0")      -- SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN
  , ("preceq", "\x2AAF")      -- PRECEDES ABOVE SINGLE-LINE EQUALS SIGN
  , ("supset", "\x2283")      -- SUPERSET OF
  , ("subset", "\x2282")      -- SUBSET OF
  , ("supseteq", "\x2287")    -- SUPERSET OF OR EQUAL TO
  , ("subseteq", "\x2286")    -- SUBSET OF OR EQUAL TO
  , ("in", "\x2208")          -- ELEMENT OF
  , ("ni", "\x220B")          -- CONTAINS AS MEMBER
  , ("owns", "\x220B")        -- CONTAINS AS MEMBER
  , ("gg", "\x226B")          -- MUCH GREATER-THAN
  , ("ll", "\x226A")          -- MUCH LESS-THAN
  , ("sim", "\x223C")         -- TILDE OPERATOR
  , ("simeq", "\x2243")       -- ASYMPTOTICALLY EQUAL TO
  , ("perp", "\x27C2")        -- PERPENDICULAR
  , ("equiv", "\x2261")       -- IDENTICAL TO
  , ("asymp", "\x224D")       -- EQUIVALENT TO
  , ("smile", "\x2323")       -- SMILE
  , ("frown", "\x2322")       -- FROWN
  , ("bowtie", "\x22C8")      -- BOWTIE
  , ("models", "\x22A7")      -- MODELS
  , ("iff", "\x21D4")         -- LEFT RIGHT DOUBLE ARROW
  , ("ldotp", "\x002E")       -- FULL STOP
  , ("cdotp", "\x22C5")       -- DOT OPERATOR
  , ("colon", "\x003A")       -- COLON
  , ("ldots", "\x2026")       -- HORIZONTAL ELLIPSIS
  , ("cdots", "\x22EF")       -- MIDLINE HORIZONTAL ELLIPSIS
  , ("vdots", "\x22EE")       -- VERTICAL ELLIPSIS
  , ("ddots", "\x22F1")       -- DOWN RIGHT DIAGONAL ELLIPSIS
  , ("lgroup", "\x0028")      -- LEFT PARENTHESIS
  , ("rgroup", "\x0029")      -- RIGHT PARENTHESIS
  , ("{", "\x007B")           -- LEFT CURLY BRACKET
  , ("}", "\x007D")           -- RIGHT CURLY BRACKET
  , ("lbrace", "\x007B")      -- LEFT CURLY BRACKET
  , ("rbrace", "\x007D")      -- RIGHT CURLY BRACKET
  , ("langle", "\x27E8")      -- MATHEMATICAL LEFT ANGLE BRACKET
  , ("rangle", "\x27E9")      -- MATHEMATICAL RIGHT ANGLE BRACKET
  , ("lceil", "\x2308")       -- LEFT CEILING
  , ("rceil", "\x2309")       -- RIGHT CEILING
  , ("lfloor", "\x230A")      -- LEFT FLOOR
  , ("rfloor", "\x230B")      -- RIGHT FLOOR
  , ("lmoustache", "\x23B0")  -- UPPER LEFT OR LOWER RIGHT CURLY BRACKET SECTION
  , ("rmoustache", "\x23B1")  -- UPPER RIGHT OR LOWER LEFT CURLY BRACKET SECTION
  , ("arrowvert", "\x007C")   -- VERTICAL LINE
  , ("Arrowvert", "\x2225")   -- PARALLEL TO
  , ("bracevert", "\x007C")   -- VERTICAL LINE
  , ("Vert", "\x2225")        -- PARALLEL TO
  , ("vert", "\x007C")        -- VERTICAL LINE
  , ("backslash", "\x005C")   -- REVERSE SOLIDUS
  , ("sqrt", "\x221A")        -- SQUARE ROOT
  , ("cong", "\x2245")        -- APPROXIMATELY EQUAL TO
  , ("notin", "\x2209")       -- NOT AN ELEMENT OF
  , ("doteq", "\x2250")       -- APPROACHES THE LIMIT
  , ("flat", "\x266D")        -- MUSIC FLAT SIGN
  , ("natural", "\x266E")     -- MUSIC NATURAL SIGN
  , ("sharp", "\x266F")       -- MUSIC SHARP SIGN
  , ("clubsuit", "\x2663")            -- BLACK CLUB SUIT
  , ("diamondsuit", "\x2662")         -- WHITE DIAMOND SUIT
  , ("heartsuit", "\x2661")           -- WHITE HEART SUIT
  , ("spadesuit", "\x2660")           -- BLACK SPADE SUIT
  , ("triangle", "\x25B3")            -- WHITE UP-POINTING TRIANGLE
  , ("triangleleft", "\x25C1")        -- WHITE LEFT-POINTING TRIANGLE
  , ("triangleright", "\x25B7")       -- WHITE RIGHT-POINTING TRIANGLE
  , ("bigtriangleup", "\x25B3")       -- WHITE UP-POINTING TRIANGLE
  , ("bigtriangledown", "\x25BD")     -- WHITE DOWN-POINTING TRIANGLE
  , ("leftarrow", "\x2190")           -- LEFTWARDS ARROW
  , ("gets", "\x2190")                -- LEFTWARDS ARROW
  , ("Leftarrow", "\x21D0")           -- LEFTWARDS DOUBLE ARROW
  , ("longleftarrow", "\x27F5")       -- LONG LEFTWARDS ARROW
  , ("Longleftarrow", "\x27F8")       -- LONG LEFTWARDS DOUBLE ARROW
  , ("rightarrow", "\x2192")          -- RIGHTWARDS ARROW
  , ("to", "\x2192")                  -- RIGHTWARDS ARROW
  , ("mapsto", "\x21A6")              -- RIGHTWARDS ARROW FROM BAR
  , ("Rightarrow", "\x21D2")          -- RIGHTWARDS DOUBLE ARROW
  , ("longrightarrow", "\x27F6")      -- LONG RIGHTWARDS ARROW
  , ("longmapsto", "\x27FC")          -- LONG RIGHTWARDS ARROW FROM BAR
  , ("Longrightarrow", "\x27F9")      -- LONG RIGHTWARDS DOUBLE ARROW
  , ("leftrightarrow", "\x2194")      -- LEFT RIGHT ARROW
  , ("Leftrightarrow", "\x21D4")      -- LEFT RIGHT DOUBLE ARROW
  , ("longleftrightarrow", "\x27F7")  -- LONG LEFT RIGHT ARROW
  , ("Longleftrightarrow", "\x27FA")  -- LONG LEFT RIGHT DOUBLE ARROW
  , ("leftharpoonup", "\x21BC")       -- LEFTWARDS HARPOON WITH BARB UPWARDS
  , ("leftharpoondown", "\x21BD")     -- LEFTWARDS HARPOON WITH BARB DOWNWARDS
  , ("rightharpoonup", "\x21C0")      -- RIGHTWARDS HARPOON WITH BARB UPWARDS
  , ("rightharpoondown", "\x21C1")    -- RIGHTWARDS HARPOON WITH BARB DOWNWARDS
  , ("rightleftharpoons", "\x21CC")   -- RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
  , ("hookleftarrow", "\x21A9")       -- LEFTWARDS ARROW WITH HOOK
  , ("hookrightarrow", "\x21AA")      -- RIGHTWARDS ARROW WITH HOOK
  , ("uparrow", "\x2191")             -- UPWARDS ARROW
  , ("Uparrow", "\x21D1")             -- UPWARDS DOUBLE ARROW
  , ("downarrow", "\x2193")           -- DOWNWARDS ARROW
  , ("Downarrow", "\x21D3")           -- DOWNWARDS DOUBLE ARROW
  , ("updownarrow", "\x2195")         -- UP DOWN ARROW
  , ("Updownarrow", "\x21D5")         -- UP DOWN DOUBLE ARROW
  ] ++ map (\x -> (x,x)) namedFunctions

namedFunctions :: [String]
namedFunctions =
  [ "arccos"
  , "arcsin"
  , "arctan"
  , "arg"
  , "cos"
  , "cosh"
  , "cot"
  , "coth"
  , "csc"
  , "deg"
  , "det"
  , "dim"
  , "exp"
  , "gcd"
  , "hom"
  , "inf"
  , "ker"
  , "lg"
  , "lim"
  , "liminf"
  , "limsup"
  , "ln"
  , "log"
  , "max"
  , "min"
  , "Pr"
  , "sec"
  , "sin"
  , "sinh"
  , "sup"
  , "tan"
  , "tanh"
  ]

-- | Diacritics (accents) defined in the Plain TeX format.
--
-- We do not distinguish math mode from text mode at this point (yet).
diacritics :: Map String String
diacritics = fromList
  [ -- diacritics in text mode
    ("`", "\x0300")           -- COMBINING GRAVE ACCENT
  , ("'", "\x0301")           -- COMBINING ACUTE ACCENT
  , ("^", "\x0302")           -- COMBINING CIRCUMFLEX ACCENT
  , ("~", "\x0303")           -- COMBINING TILDE
  , ("=", "\x0304")           -- COMBINING MACRON
  , ("u", "\x0306")           -- COMBINING BREVE
  , (".", "\x0307")           -- COMBINING DOT ABOVE
  , ("\"", "\x0308")          -- COMBINING DIAERESIS
  , ("r", "\x030A")           -- COMBINING RING ABOVE
  , ("H", "\x030B")           -- COMBINING DOUBLE ACUTE ACCENT
  , ("v", "\x030C")           -- COMBINING CARON
  , ("d", "\x0323")           -- COMBINING DOT BELOW
  , ("c", "\x0327")           -- COMBINING CEDILLA
  , ("k", "\x0328")           -- COMBINING OGONEK
  , ("b", "\x0331")           -- COMBINING MACRON BELOW
    -- diacritics in math mode
  , ("grave", "\x0300")       -- COMBINING GRAVE ACCENT
  , ("acute", "\x0301")       -- COMBINING ACUTE ACCENT
  , ("hat", "\x0302")         -- COMBINING CIRCUMFLEX ACCENT
  , ("tilde", "\x0303")       -- COMBINING TILDE
  , ("bar", "\x0304")         -- COMBINING MACRON
  , ("breve", "\x0306")       -- COMBINING BREVE
  , ("dot", "\x0307")         -- COMBINING DOT ABOVE
  , ("ddot", "\x0308")        -- COMBINING DIAERESIS
  , ("check", "\x030C")       -- COMBINING CARON
  , ("vec", "\x20d7")         -- COMBINING RIGHT ARROW ABOVE
    -- combining symbols in math mode
  , ("not", "\x0338")         -- COMBINING LONG SOLIDUS OVERLAY
  ]

-- More relevant tie-like commands:
--   widetilde, widehat,
--   overrightarrow, overleftarrow,
--   overbrace, underbrace.
--
-- | Double diacritics defined in the Plain TeX format.
--
-- Double diacritics (or ties) are diacritics that combine two
-- adjacent base characters.
--
-- We do not distinguish math mode from text mode at this point (yet).
dbldiacritics :: Map String String
dbldiacritics = fromList
  [ ("t", "\x0361")           -- COMBINING DOUBLE INVERTED BREVE
  ]
