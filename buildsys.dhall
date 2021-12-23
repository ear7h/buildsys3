let B = ./prelude.dhall
let cmd = λ(cmd : Text) → λ(args : List Text) → { cmd, args } : B.Command
let bash = λ(expr : Text) → cmd "bash" [ "-c", expr ]
let example
    : B.Item
    = B.generated
      { results = [ "hello_world.txt" ]
      , command = bash "cat {hello,world}.txt > hello_world.txt"
      , prereqs =
        [ B.generated
            { results = [] : List Text
            , command = cmd "echo" ["hello"]
            , prereqs = [] : List B.Item
            }
        , B.generated
            { results = [ "hello.txt" ]
            , command = bash "echo hello > hello.txt"
            -- , command = bash "echo hello no write"
            , prereqs = [] : List B.Item
            }
        -- , source "hello.txt"
        , B.generated
            { results = [ "world.txt" ]
            , command = bash "echo world > world.txt"
            , prereqs = [] : List B.Item
            }
        ]
    }


in { default = B.build example }
