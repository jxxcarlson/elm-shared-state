module Routing.Router exposing (Model, Msg(..), initialModel, init, pageView, update, updateHome, updateSettings, view)

import Browser
import Browser.Navigation exposing (Key)
import Pages.Home as Home
import Pages.Settings as Settings
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Url exposing (Url)
import Html exposing (Html)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Common.Style as Style


type alias Model =
    { homeModel : Home.Model
    , settingsModel : Settings.Model
    , route : Route
    }


type Msg
    = UrlChange Url
    | NavigateTo Route
    | HomeMsg Home.Msg
    | SettingsMsg Settings.Msg


initialModel : Url -> Model
initialModel url =
    let
        ( homeModel, homeCmd ) =
            Home.init

        settingsModel =
            Settings.initModel
    in
        { homeModel = homeModel
        , settingsModel = settingsModel
        , route = parseUrl url
        }


init : Url -> ( Model, Cmd Msg )
init url =
    let
        ( homeModel, homeCmd ) =
            Home.init

        settingsModel =
            Settings.initModel
    in
        ( { homeModel = homeModel
          , settingsModel = settingsModel
          , route = parseUrl url
          }
        , Cmd.map HomeMsg homeCmd
        )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UrlChange location ->
            ( { model | route = parseUrl location }
            , Cmd.none
            , NoUpdate
            )

        NavigateTo route ->
            ( model
            , Browser.Navigation.pushUrl sharedState.navKey (reverseRoute route)
            , NoUpdate
            )

        HomeMsg homeMsg ->
            updateHome model homeMsg

        SettingsMsg settingsMsg ->
            updateSettings sharedState model settingsMsg


updateHome : Model -> Home.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateHome model homeMsg =
    let
        ( nextHomeModel, homeCmd ) =
            Home.update homeMsg model.homeModel
    in
        ( { model | homeModel = nextHomeModel }
        , Cmd.map HomeMsg homeCmd
        , NoUpdate
        )


updateSettings : SharedState -> Model -> Settings.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateSettings sharedState model settingsMsg =
    let
        ( nextSettingsModel, settingsCmd, sharedStateUpdate ) =
            Settings.update sharedState settingsMsg model.settingsModel
    in
        ( { model | settingsModel = nextSettingsModel }
        , Cmd.map SettingsMsg settingsCmd
        , sharedStateUpdate
        )


view : (Msg -> msg) -> SharedState -> Model -> { body : List (Html.Html msg), title : String }
view msgMapper sharedState model =
    let
        title =
            case model.route of
                HomeRoute ->
                    "Home"

                SettingsRoute ->
                    "Settings"

                NotFoundRoute ->
                    "404"

        body_ =
            column [ padding 40 ]
                [ el [ paddingXY 0 20, Font.bold ]
                    (text "Shared State Demo")
                , row
                    [ spacing 24 ]
                    [ Input.button (Style.activeButton (model.route == HomeRoute))
                        { onPress = Just (NavigateTo HomeRoute)
                        , label = el [] (text "Home")
                        }
                    , Input.button (Style.activeButton (model.route == SettingsRoute))
                        { onPress = Just (NavigateTo SettingsRoute)
                        , label = el [] (text "Settings")
                        }
                    ]
                , pageView sharedState model
                ]
    in
        { title = "Elm Shared State Demo"
        , body = body_ |> Element.layout [] |> Html.map msgMapper |> \x -> [ x ]
        }


pageView : SharedState -> Model -> Element Msg
pageView sharedState model =
    row []
        [ case model.route of
            HomeRoute ->
                Home.view sharedState model.homeModel
                    |> Element.map HomeMsg

            SettingsRoute ->
                Settings.view sharedState model.settingsModel
                    |> Element.map SettingsMsg

            NotFoundRoute ->
                el [] (text "404 :(")
        ]
