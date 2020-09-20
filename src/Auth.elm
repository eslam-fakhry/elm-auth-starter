module Auth exposing (..)

import Base64.Encode as Base64
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Http
import Json.Decode as Decode
import OAuth
import OAuth.AuthorizationCode.PKCE as AuthCode
import Ports
import Url exposing (Url)


type Flow
    = Idle
    | Authorized String AuthCode.CodeVerifier
    | Authenticated OAuth.Token
    | Done UserInfo
    | Errored AuthError


type alias Configuration =
    { authorizationEndpoint : Url
    , accessTokenEndpoint : Url
    , userInfoEndpoint : Url
    , userInfoDecoder : Decode.Decoder UserInfo
    , clientId : String
    , scope : List String
    }


type AuthError
    = ErrStateMismatch
    | ErrAuthorization AuthCode.AuthorizationError
    | ErrAuthentication AuthCode.AuthenticationError
    | ErrHTTPGetAccessToken
    | FailedByteConversion


type alias UserInfo =
    { name : String
    , picture : String
    }


type alias AuthenticationResponse =
    Result Http.Error AuthCode.AuthenticationSuccess


type alias UserInfoResponse =
    Result Http.Error UserInfo


defaulthttpsurl : Url
defaulthttpsurl =
    { protocol = Url.Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


configuration : Configuration
configuration =
    let
        httpUrl =
            { defaulthttpsurl | host = "" }
    in
    { authorizationEndpoint = { httpUrl | path = "/authorize/" }
    , accessTokenEndpoint = { httpUrl | path = "/oauth/token" }
    , userInfoEndpoint = { httpUrl | path = "/userinfo" }
    , userInfoDecoder =
        Decode.map2 UserInfo
            (Decode.field "name" Decode.string)
            (Decode.field "picture" Decode.string)
    , clientId = ""
    , scope = [ "openid", "profile" ]
    }


cSTATE_SIZE : Int
cSTATE_SIZE =
    8


cCODE_VERIFIER_SIZE : Int
cCODE_VERIFIER_SIZE =
    32


getInitialFlow : Maybe { state : String, codeVerifier : AuthCode.CodeVerifier } -> Url -> Flow
getInitialFlow mflags originUrl =
    case AuthCode.parseCode originUrl of
        AuthCode.Empty ->
            Idle

        AuthCode.Error err ->
            Errored <| ErrAuthorization err

        AuthCode.Success { code, state } ->
            mflags
                |> Maybe.map
                    (\flags ->
                        if state == Just flags.state then
                            Authorized code flags.codeVerifier

                        else
                            Errored ErrStateMismatch
                    )
                |> Maybe.withDefault (Errored ErrStateMismatch)


genRandomBytesForStateAndVerifier : () -> Cmd msg
genRandomBytesForStateAndVerifier () =
    Ports.genRandomBytes (cSTATE_SIZE + cCODE_VERIFIER_SIZE)


convertBytes : List Int -> Maybe { state : String, codeVerifier : AuthCode.CodeVerifier }
convertBytes bytes =
    if List.length bytes < (cSTATE_SIZE + cCODE_VERIFIER_SIZE) then
        Nothing

    else
        let
            state =
                bytes
                    |> List.take cSTATE_SIZE
                    |> toBytes
                    |> toBase64

            codeVerifier =
                bytes
                    |> List.drop cSTATE_SIZE
                    |> toBytes
                    |> AuthCode.codeVerifierFromBytes
        in
        codeVerifier
            |> Maybe.map
                (\cv ->
                    { state = state
                    , codeVerifier = cv
                    }
                )


createAuthorization : Url -> String -> AuthCode.CodeVerifier -> AuthCode.Authorization
createAuthorization redirectUri state codeVerifier =
    { clientId = configuration.clientId
    , url = configuration.authorizationEndpoint
    , redirectUri = redirectUri
    , scope = configuration.scope
    , state = Just state
    , codeChallenge = AuthCode.mkCodeChallenge codeVerifier
    }


createAuthentication : Url -> String -> AuthCode.CodeVerifier -> AuthCode.Authentication
createAuthentication redirectUri code codeVerifier =
    { credentials =
        { clientId = configuration.clientId
        , secret = Nothing
        }
    , code = code
    , codeVerifier = codeVerifier
    , redirectUri = redirectUri
    , url = configuration.accessTokenEndpoint
    }


flowFromAuthenticationResponse : AuthenticationResponse -> Flow
flowFromAuthenticationResponse response =
    case response of
        Ok { token } ->
            Authenticated token

        Err (Http.BadBody body) ->
            decodeBodyAuthenticationError body
                |> Result.map (\err -> Errored <| ErrAuthentication err)
                |> Result.withDefault (Errored ErrHTTPGetAccessToken)

        Err _ ->
            Errored ErrHTTPGetAccessToken


flowFromUserInfoResponse : UserInfoResponse -> Flow
flowFromUserInfoResponse response =
    response
        |> Result.map (\userInfo -> Done userInfo)
        |> Result.withDefault (Errored ErrHTTPGetAccessToken)


decodeBodyAuthenticationError : String -> Result Decode.Error AuthCode.AuthenticationError
decodeBodyAuthenticationError body =
    Decode.decodeString AuthCode.defaultAuthenticationErrorDecoder body


toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8
        >> Bytes.sequence
        >> Bytes.encode


toBase64 : Bytes -> String
toBase64 =
    Base64.bytes
        >> Base64.encode
