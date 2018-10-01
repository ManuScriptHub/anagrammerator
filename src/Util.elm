module Util exposing (httpErrorToString)

import Http


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "Bad url: " ++ url

        Http.Timeout ->
            "Connection has timed out"

        Http.BadStatus resp ->
            if resp.status.code == 401 then
                "Wrong username or password"

            else
                "Some error has occurred on the server"

        Http.NetworkError ->
            "A network error has occurred"

        Http.BadPayload string resp ->
            "Unknown error has occurred: " ++ string
