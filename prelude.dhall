let Prelude =  https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/package.dhall
let List/map = Prelude.List.map
let List/concat = Prelude.List.concat
let Command =
    { cmd : Text
    , args : List Text
    }

let Token =
    < TOpen
    | TClose
    | TGenerated :
        { _1 : List Text -- artifacts
        , _2 : Command -- exec
        }
    | TSource : Text
    >

let Item
    : Type
    = ∀ (MkItem :
        { source : Text → List Token
        , generated :
          { results : List Text
          , command : Command
          , prereqs : List (List Token)
          } → List Token
        }
      ) → List Token

let Args =
  { results : List Text
  , command : Command
  , prereqs : List Item
  } : Type

let generated
  : { results : List Text
    , command : Command
    , prereqs : List Item
    } → Item
  = λ(args : Args) →
    λ(MkItem :
      { source : Text → List Token
      , generated :
        { results : List Text
        , command : Command
        , prereqs : List (List Token)
        } → List Token
      }
    ) →
    MkItem.generated
      { results = args.results
      , command = args.command
      , prereqs =
        List/map
          Item
          (List Token)
          (λ(x : Item) → x MkItem)
          args.prereqs
      }

let source
  : Text → Item
  = λ(file : Text) →
    λ(MkItem :
      { source : Text → List Token
      , generated :
        { results : List Text
        , command : Command
        , prereqs : List (List Token)
        } → List Token
      }
    ) →
    MkItem.source file


let build
    : Item → List Token
    = λ(x : Item) →
      x
        { source = λ(file : Text) → [ Token.TSource file ]
        , generated = λ(p :
          { results : List Text
          , command : Command
          , prereqs : List (List Token)
          }
        ) →
          [ Token.TGenerated { _1 = p.results, _2 = p.command }
          , Token.TOpen
          ] # (List/concat Token p.prereqs) # [ Token.TClose ]
        }


in { Item, Args, Command, build, source, generated }
