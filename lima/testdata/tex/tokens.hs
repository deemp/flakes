[ Indent
    { n = 3 }
, Text
    { someLines = [ "\\begin{code}" ] }
, HaskellCode
    { someLines =
        [ "  f b"
        , "a ="
        ]
    }
, Text
    { someLines = [ "\\end{code}" ] }
, Dedent
, Text
    { someLines = [ "\\begin{code}" ] }
, HaskellCode
    { someLines =
        [ "  f b"
        , "a ="
        ]
    }
, Text
    { someLines = [ "\\end{code}" ] }
, Indent
    { n = 2 }
, Indent
    { n = 5 }
, Text
    { someLines = [ "\\begin{code}" ] }
, HaskellCode
    { someLines =
        [ "  f b"
        , "a ="
        ]
    }
, Text
    { someLines = [ "\\end{code}" ] }
, Comment
    { body = "world!" :|
        [ ""
        , "Hello,"
        ]
    }
, Text
    { someLines =
        [ "Line 2"
        , "Line 1"
        , ""
        , "Line 2"
        , "Line 1"
        ]
    }
, Disabled
    { someLines =
        [ "Line 2"
        , "Line 1"
        ]
    }
]
