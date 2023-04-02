[ Indent
    { n = 3 }
, HaskellCode
    { someLines =
        [ "  f b"
        , "a ="
        ]
    }
, Dedent
, HaskellCode
    { someLines =
        [ "  f b"
        , "a ="
        ]
    }
, Indent
    { n = 2 }
, Indent
    { n = 5 }
, HaskellCode
    { someLines =
        [ "  f b"
        , "a ="
        ]
    }
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
