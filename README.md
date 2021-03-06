# buildsys3

A build system

## installing

```
cabal install
```

## example

Firstly, define some build items in a `buildsys.dhall` file:

```dhall
let B = https://raw.githubusercontent.com/ear7h/buildsys3/main/prelude.dhall
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
    -- Note that this item has no result files, this is similar a
    -- .PHONY rule in make, and will run every time. It will also cause
    -- any parent items to rerun even if they have do have results.
    { results = [] : List Text
    , command = cmd "rm" [ "-rf", "world.txt", "hello_world.txt" ]
    , prereqs = [] : List B.Item
    })
  }
```

You can then build the items with `buildsys3 [attr]`. Simply running
`buildsys3` will build the `default` item.
