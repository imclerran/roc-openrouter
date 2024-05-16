module [TimeoutConfig, ErrorResponse, RequestObject, ResponseFormat, dropLeadingGarbage, decodeErrorResponse]

import json.Json

## Redefinition of TimeoutConfig from the basic-cli Http module
TimeoutConfig : [TimeoutMilliseconds U64, NoTimeout]

## The request object to be sent with basic-cli's Http.send 
RequestObject : {
    method : [Post],
    headers : List [Header Str Str],
    mimeType : Str,
    url : Str,
    body : List U8,
    timeout : TimeoutConfig,
}

## The structure of the JSON error response from the OpenAI API
ErrorResponse : {
    error : {
        code : U16,
        message : Str,
    },
}

ResponseFormat : {
    type: Str,
    #extra: U8, # This field only to avoid a compiler bug which occurs when for a record holding a single string
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