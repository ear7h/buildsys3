let B = ../prelude.dhall
let cmd = λ(cmd : Text) → λ(args : List Text) → { cmd, args } : B.Command
let bash = λ(expr : Text) → cmd "bash" [ "-c", expr ]
let default
  : B.Item
  = B.generated
  { results = [ "hello_world.txt" ]
  , command = bash "cat {hello,world}.txt > hello_world.txt"
  , prereqs =
    [ B.source "hello.txt"
    , B.generated
      { results = [ "world.txt" ]
      , command = bash "echo world > world.txt"
      , prereqs = [] : List B.Item
      }
    ]
  }

in
  { default = B.build default
  , clean = B.build (B.generated
    { results = [] : List Text
    , command = cmd "rm" [ "-rf", "world.txt", "hello_world.txt" ]
    , prereqs = [] : List B.Item
    })
  }

