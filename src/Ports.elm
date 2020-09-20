port module Ports exposing (..)

port genRandomBytes : Int -> Cmd msg

port randomBytes : (List Int -> msg) -> Sub msg