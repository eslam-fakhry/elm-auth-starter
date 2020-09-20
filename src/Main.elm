module Main exposing (Model, init, main, update, view)

import Auth exposing (AuthError(..), Flow(..), UserInfo, configuration)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode
import OAuth
import OAuth.AuthorizationCode.PKCE as AuthCode
import Ports
import Task
import Url exposing (Url)



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program (Maybe (List Int)) Model Msg
main =
    Browser.application
        { init = Maybe.andThen Auth.convertBytes >> init
        , update = update
        , view =
            \m ->
                { title = "elmWithAuth"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Ports.randomBytes GotRandomBytes
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        }



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { flow : Flow
    , redirectUri : Url
    , navigationKey : Nav.Key
    }


type Msg
    = NoOp
    | SigninRequested
    | GotRandomBytes (List Int)
    | AccessTokenRequested
    | GotAccessToken (Result Http.Error AuthCode.AuthenticationSuccess)
    | UserInfoRequested
    | GotUserInfo (Result Http.Error UserInfo)
    | SignoutRequested


init : Maybe { state : String, codeVerifier : AuthCode.CodeVerifier } -> Url -> Nav.Key -> ( Model, Cmd Msg )
init mflags origin navigationKey =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }

        flow =
            Auth.getInitialFlow mflags origin
    in
    ( { flow = flow
      , redirectUri = redirectUri
      , navigationKey = navigationKey
      }
    , Cmd.batch
        [ ensureUrlIsClear flow redirectUri navigationKey
        , requestAccessTokenIfAuthorized flow
        ]
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( model.flow, message ) of
        ( _, NoOp ) ->
            noOp model

        ( Idle, SigninRequested ) ->
            signInRequested model

        ( Errored _, SigninRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Authorized code codeVerifier, AccessTokenRequested ) ->
            accessTokenRequested model code codeVerifier

        ( Authorized _ _, GotAccessToken authenticationResponse ) ->
            gotAccessToken model authenticationResponse

        ( Authenticated token, UserInfoRequested ) ->
            userInfoRequested model token

        ( Authenticated _, GotUserInfo userInfoResponse ) ->
            gotUserInfo model userInfoResponse

        ( Done _, SignoutRequested ) ->
            signoutRequested model

        ( Idle, AccessTokenRequested ) ->
            noOp model

        ( Idle, GotAccessToken _ ) ->
            noOp model

        ( Idle, UserInfoRequested ) ->
            noOp model

        ( Idle, GotUserInfo _ ) ->
            noOp model

        ( Idle, SignoutRequested ) ->
            noOp model

        _ ->
            noOp model


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model
    , Cmd.none
    )


signInRequested : Model -> ( Model, Cmd Msg )
signInRequested model =
    ( { model | flow = Idle }
    , Auth.genRandomBytesForStateAndVerifier ()
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    case Auth.convertBytes bytes of
        Nothing ->
            ( { model | flow = Errored Auth.FailedByteConversion }
            , Cmd.none
            )

        Just { state, codeVerifier } ->
            ( { model | flow = Idle }
            , Auth.createAuthorization model.redirectUri state codeVerifier
                |> AuthCode.makeAuthorizationUrl
                |> Url.toString
                |> Nav.load
            )


accessTokenRequested : Model -> String -> AuthCode.CodeVerifier -> ( Model, Cmd Msg )
accessTokenRequested model code codeVerifier =
    ( { model | flow = Authorized code codeVerifier }
    , getAccessToken model.redirectUri code codeVerifier
    )


getAccessToken : Url -> String -> AuthCode.CodeVerifier -> Cmd Msg
getAccessToken redirectUri code codeVerifier =
    Auth.createAuthentication redirectUri code codeVerifier
        |> AuthCode.makeTokenRequest GotAccessToken
        |> Http.request


gotAccessToken : Model -> Auth.AuthenticationResponse -> ( Model, Cmd Msg )
gotAccessToken model response =
    let
        flow =
            Auth.flowFromAuthenticationResponse response
    in
    ( { model | flow = flow }
    , requestUserInfoIfAuthenticated flow
    )


userInfoRequested : Model -> OAuth.Token -> ( Model, Cmd Msg )
userInfoRequested model token =
    ( { model | flow = Authenticated token }
    , getUserInfo configuration token
    )


getUserInfo : Auth.Configuration -> OAuth.Token -> Cmd Msg
getUserInfo { userInfoEndpoint, userInfoDecoder } token =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , url = Url.toString userInfoEndpoint
        , expect = Http.expectJson GotUserInfo userInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


gotUserInfo : Model -> Auth.UserInfoResponse -> ( Model, Cmd Msg )
gotUserInfo model response =
    let
        flow =
            Auth.flowFromUserInfoResponse response
    in
    ( { model | flow = flow }
    , Cmd.none
    )


signoutRequested : Model -> ( Model, Cmd Msg )
signoutRequested model =
    ( { model | flow = Idle }
    , Nav.load (Url.toString model.redirectUri)
    )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ button
            [ class "pure-button pure-button-primary"
            , onClick SigninRequested
            ]
            [ text "Login" ]
        , button
            [ class "pure-button pure-button-primary"
            , onClick SignoutRequested
            ]
            [ text "Logout" ]
        ]


run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())


ensureUrlIsClear : Flow -> Url -> Nav.Key -> Cmd msg
ensureUrlIsClear flow redirectUri navigationKey =
    case flow of
        Idle ->
            Cmd.none

        _ ->
            Nav.replaceUrl navigationKey (Url.toString redirectUri)


requestAccessTokenIfAuthorized : Flow -> Cmd Msg
requestAccessTokenIfAuthorized flow =
    case flow of
        Authorized _ _ ->
            run AccessTokenRequested

        _ ->
            Cmd.none


requestUserInfoIfAuthenticated : Flow -> Cmd Msg
requestUserInfoIfAuthenticated flow =
    case flow of
        Authenticated _ ->
            run UserInfoRequested

        _ ->
            Cmd.none
