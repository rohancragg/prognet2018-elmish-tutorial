module App.View

open Elmish
open Fulma

type Model =
    { PokemonDisplayModel : Pokemon.Model
      PokemonDisplayModel2 : Pokemon.Model }

type Msg =
    | UpdatePokemonDisplay of Pokemon.Msg
    | UpdatePokemonDisplay2 of Pokemon.Msg

let init _ =
    let pokemonModel, pokemonCmds =
        Pokemon.init ()
    let pokemonModel2, pokemonCmds2 =
        Pokemon.init()
    { PokemonDisplayModel = pokemonModel
      PokemonDisplayModel2 = pokemonModel2  },
    Cmd.batch [
        Cmd.map UpdatePokemonDisplay pokemonCmds
        Cmd.map UpdatePokemonDisplay2 pokemonCmds2
    ]

let update msg model =
    match msg with
    | UpdatePokemonDisplay pmsg ->
        let pokemonModel, pokemonCmds =
            Pokemon.update pmsg model.PokemonDisplayModel
        { model with PokemonDisplayModel = pokemonModel },
        Cmd.map UpdatePokemonDisplay pokemonCmds
    | UpdatePokemonDisplay2 pmsg ->
        let pokemonModel, pokemonCmds =
            Pokemon.update pmsg model.PokemonDisplayModel2
        { model with PokemonDisplayModel2 = pokemonModel },
        Cmd.map UpdatePokemonDisplay2 pokemonCmds

let view model dispatch =
    let pokemonView =
        Pokemon.view model.PokemonDisplayModel
            (fun m -> dispatch (UpdatePokemonDisplay m))
    let pokemonView2 =
        Pokemon.view model.PokemonDisplayModel2
            (fun m -> dispatch (UpdatePokemonDisplay2 m))
    Container.container [] [
        pokemonView
        pokemonView2  ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
