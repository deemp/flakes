[ Disabled
    { manyLines = [ "-- What's the answer?" ] }
, Indent
    { n = 1 }
, Indent
    { n = 2 }
, Text
    { someLines = "\\begin{code}" :|
        [ ""
        , "Intermediate results"
        ]
    }
, HaskellCode
    { manyLines =
        [ "b = a 4"
        , "a = const 3"
        ]
    }
, Text
    { someLines = "\\end{code}" :| [] }
, Dedent
, Text
    { someLines = "\\begin{code}" :| [] }
, HaskellCode
    { manyLines = [ "answer = b * 14" ] }
, Text
    { someLines = "\\end{code}" :| [] }
, Comment
    { someLines = "world!" :|
        [ ""
        , "Hello from comments,"
        ]
    }
, Text
    { someLines = "world!" :| [ "Hello from text," ] }
]
