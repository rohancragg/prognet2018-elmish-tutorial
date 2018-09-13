module Pokemon

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma

open Fable.PowerPack
open Fable.PowerPack.Fetch

type Pokemon =
    { name : string
      weight : int
      id : int }

type Model =
    { Loading : bool
      PokemonData : Option<Result<Pokemon, string>>
      PokemonId : int }

type Msg =
    | LoadPokemon
    | PokemonLoaded of Pokemon
    | LoadingFailed of string
    | IncreasePokemonId
    | DecreasePokemonId

let fetchPokemon pokemonId =
    promise {
        let url = (sprintf "https://pokeapi.co/api/v2/pokemon/%d/" pokemonId)

        let props =
            [ RequestProperties.Method HttpMethod.GET
              requestHeaders [ HttpRequestHeaders.Accept "application/json" ] ]
        return! fetchAs<Pokemon> url props
    }

let init _ =
    let model =
        { Loading = false
          PokemonData = None
          PokemonId = 333 }
    model, Cmd.ofMsg LoadPokemon

let update msg model =
    match msg with
    | LoadPokemon ->
        { model with Loading = true
                     PokemonData = None },
        Cmd.ofPromise fetchPokemon model.PokemonId PokemonLoaded
            (fun e -> LoadingFailed e.Message)
    | IncreasePokemonId ->
        { model with PokemonId = model.PokemonId + 1 }, Cmd.none
    | DecreasePokemonId ->
        { model with PokemonId = model.PokemonId - 1 }, Cmd.none
    | PokemonLoaded data ->
        { model with PokemonData = Some(Ok data)
                     Loading = false }, Cmd.none
    | LoadingFailed error ->
        { model with PokemonData = Some(Error error)
                     Loading = false }, Cmd.none

let idSelector model dispatch =
    [ Level.level []
          [ Level.item [ Level.Item.HasTextCentered ]
                [ Button.button
                      [ Button.Color IsDanger

                        Button.Props
                            [ OnClick(fun _ -> dispatch DecreasePokemonId) ] ]
                      [ str "-" ] ]

            Level.item [ Level.Item.HasTextCentered ]
                [ str (sprintf "%d" model.PokemonId) ]

            Level.item [ Level.Item.HasTextCentered ]
                [ Button.button
                      [ Button.Color IsSuccess

                        Button.Props
                            [ OnClick(fun _ -> dispatch IncreasePokemonId) ] ]
                      [ str "+" ] ] ]

      Level.level []
          [ Level.item [ Level.Item.HasTextCentered ]
                [ Button.button
                      [ Button.IsActive(not model.Loading)
                        Button.Color IsPrimary
                        Button.Props [ OnClick(fun _ -> dispatch LoadPokemon) ] ]
                      [ str "Load Pokemon!" ] ] ] ]

let view model dispatch =
    let content =
        if model.Loading then [ p [] [ str "Loooading!" ] ]
        else
            match model.PokemonData with
            | Some(Ok data) ->
                [ Level.level []
                    [ Level.item [ Level.Item.HasTextCentered ]
                        [ div [ ]
                            [ Level.heading [ ] [ str "Name" ]
                              Level.title [ ] [ str data.name ] ] ]
                      Level.item [ Level.Item.HasTextCentered ]
                        [ div [ ]
                            [ Level.heading [ ] [ str "Weight" ]
                              Level.title [ ] [ str (sprintf "%d" data.weight) ] ] ]
                      Level.item [ Level.Item.HasTextCentered ]
                        [ div [ ]
                            [ Level.heading [ ] [ str "Id" ]
                              Level.title [ ] [ str (sprintf "%d" data.id) ] ] ] ] ]
            | Some(Error message) ->
                [ p [] [ str "Oh noes: it went wrong! The error was:" ]
                  p [] [ str message ] ]
            | None -> [ p [] [ str "No Pokemon loaded." ] ]
    Box.box' []
        [ Content.content [] (List.append content (idSelector model dispatch)) ]