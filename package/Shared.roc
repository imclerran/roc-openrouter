module [
    ApiError,
    TimeoutConfig,
    ErrorResponse,
    RequestObject,
    ResponseFormat,
    dropLeadingGarbage,
    decodeErrorResponse,
    optionToStr,
    optionToList,
    urlEncode,
    strToOption,
    listToOption,
]

import json.Json
import json.Option exposing [Option]

## Redefinition of TimeoutConfig from the basic-cli Http module
TimeoutConfig : [TimeoutMilliseconds U64, NoTimeout]

## The request object to be sent with basic-cli's Http.send
RequestObject : {
    method : [Post],
    headers : List { key : Str, value : Str },
    mimeType : Str,
    url : Str,
    body : List U8,
    timeout : TimeoutConfig,
}

## The structure of the JSON error response from the OpenAI API
ErrorResponse : {
    error : ApiError,
}

## The API error status code and description
ApiError : {
    code : U16,
    message : Str,
}

## Tells the LLM how to respond to the user. Should be either "text" or "json_object"
ResponseFormat : {
    type : Str,
}

## Drop leading garbage characters from the response body
dropLeadingGarbage : List U8 -> List U8
dropLeadingGarbage = \bytes ->
    when List.findFirstIndex bytes \elem -> elem > ' ' is
        Ok idx -> List.dropFirst bytes idx
        Err _ -> bytes

## Decode the JSON response body of an API error message
decodeErrorResponse : List U8 -> Result ErrorResponse _
decodeErrorResponse = \bodyBytes ->
    cleanedBody = dropLeadingGarbage bodyBytes
    decoder = Json.utf8With { fieldNameMapping: SnakeCase }
    decoded : Decode.DecodeResult ErrorResponse
    decoded = Decode.fromBytesPartial cleanedBody decoder
    decoded.result

## Convert an Option to a string
optionToStr : Option Str -> Str
optionToStr = \opt ->
    when Option.get opt is
        Some str -> str
        None -> ""

## Convert a string to an Option
strToOption : Str -> Option Str
strToOption = \str ->
    when str is
        "" -> Option.none {}
        _ -> Option.some str

## Convert an Option to a List
optionToList : Option (List a) -> List a
optionToList = \opt ->
    when Option.get opt is
        Some list -> list
        None -> []

## Convert a List to an Option
listToOption : List a -> Option (List a)
listToOption = \list ->
    when list is
        [] -> Option.none {}
        _ -> Option.some list

## URL-encode a string
urlEncode : Str -> Str
urlEncode = \str ->
    str
    |> Str.toUtf8
    |> List.map \char ->
        Dict.get urlEncodeDict char
        |> Result.withDefault
            ([char] |> Str.fromUtf8 |> Result.withDefault "")
    |> Str.joinWith ""

## Dictionary of characters to URL-encoded strings
urlEncodeDict : Dict U8 Str
urlEncodeDict =
    Dict.empty {}
    |> Dict.insert ' ' "%20"
    |> Dict.insert '!' "%21"
    |> Dict.insert '"' "%22"
    |> Dict.insert '#' "%23"
    |> Dict.insert '$' "%24"
    |> Dict.insert '%' "%25"
    |> Dict.insert '&' "%26"
    |> Dict.insert '\'' "%27"
    |> Dict.insert '(' "%28"
    |> Dict.insert ')' "%29"
    |> Dict.insert '*' "%2A"
    |> Dict.insert '+' "%2B"
    |> Dict.insert ',' "%2C"
    |> Dict.insert '-' "%2D"
    |> Dict.insert '.' "%2E"
    |> Dict.insert '/' "%2F"
    |> Dict.insert ':' "%3A"
    |> Dict.insert ';' "%3B"
    |> Dict.insert '<' "%3C"
    |> Dict.insert '=' "%3D"
    |> Dict.insert '>' "%3E"
    |> Dict.insert '?' "%3F"
    |> Dict.insert '@' "%40"
    |> Dict.insert '[' "%5B"
    |> Dict.insert '\\' "%5C"
    |> Dict.insert ']' "%5D"
    |> Dict.insert '^' "%5E"
    |> Dict.insert '_' "%5F"
    |> Dict.insert '`' "%60"
    |> Dict.insert '{' "%7B"
    |> Dict.insert '|' "%7C"
    |> Dict.insert '}' "%7D"
    |> Dict.insert '~' "%7E"
