## A collection of prebuilt tools for interacting with Wikipedia.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [wikipediaSearch, wikipediaParse]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [
##     (wikipediaSearch.name, wikipediaSearch.handler),
##     (wikipediaParse.name, wikipediaParse.handler),
## ]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.appendUserMessage previousMessages newMessage
## response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
## updatedMessages = updateMessagesFromResponse response messages 
##     |> Tools.handleToolCalls! client toolHandlerMap
## ```
module { sendHttpReq } -> [wikipediaSearch, wikipediaParse]

import json.Json
import InternalTools exposing [Tool]
import Shared exposing [urlEncode]

baseUrl = "https://en.wikipedia.org/w/api.php"

## Expose name, handler and tool for the wikipediaSarch.
##
## This tool allows the model to search Wikipedia for a given query.
wikipediaSearch : { name : Str, handler : Str -> Task Str *, tool : Tool }
wikipediaSearch = {
    name: wikipediaSearchTool.function.name,
    handler: wikipediaSearchHandler,
    tool: wikipediaSearchTool,
}

## Tool definition for the wikepedia search function.
wikipediaSearchTool : Tool
wikipediaSearchTool =
    queryParam = {
        name: "search",
        type: "string",
        description: 
            """
            The search query to use. This can be a single word or a phrase.
            """,
        required: Bool.true,
    }
    limitParam = {
        name: "limit",
        type: "number",
        description: 
            """
            The number of results to return. This must be a positive integer.
            """,
        required: Bool.true,
    }
    InternalTools.buildTool 
        "wikipediaSearch" 
        """
        Search Wikipedia for a given query. This will return a list of articles that match the query.
        """ 
        [queryParam, limitParam]
                    
## Handler for the wikipedia search tool
wikipediaSearchHandler : Str -> Task Str _
wikipediaSearchHandler = \args ->
    decoded : Decode.DecodeResult { search : Str, limit : U32 }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { search, limit } ->
            request = {
                method: Get,
                headers: [],
                url: "$(baseUrl)?action=opensearch&search=$(urlEncode search)&limit=$(Num.toStr limit)&namespace=0&format=json",
                mimeType: "",
                body: [],
                timeout: NoTimeout,
            }
            when sendHttpReq request |> Task.result! is
                Ok response ->
                    response.body
                    |> Str.fromUtf8
                    |> Result.withDefault "Failed to decode API response"
                    |> Task.ok

                Err _ ->
                    "Failed to get response from Wikipedia"
                    |> Task.ok

## Expose name, handler and tool for the wikipediaParse tool.
##
## This tool allows the model to parse a Wikipedia article.
wikipediaParse : { name : Str, handler : Str -> Task Str *, tool : Tool }
wikipediaParse = {
    name: wikipediaParseTool.function.name,
    handler: wikipediaParseHandler,
    tool: wikipediaParseTool,
}
                    
## Tool definition for the wikipedia parse function
wikipediaParseTool : Tool
wikipediaParseTool =
    titleParam = {
        name: "page",
        type: "string",
        description: 
            """
            The title of the article to parse. This must be a valid Wikipedia article title, with underscores replacing spaces.
            """,
        required: Bool.true,
    }
    InternalTools.buildTool 
        "wikipediaParse" 
        """
        Parse a Wikipedia article. This will return the plaintext content of the article.
        """ 
        [titleParam]
                    
## Handler for the wikipedia parse tool
wikipediaParseHandler : Str -> Task Str _
wikipediaParseHandler = \args ->
    decoded : Decode.DecodeResult { page : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { page } ->
            request = {
                method: Get,
                headers: [],
                url: "$(baseUrl)?action=parse&page=$(page)&prop=text&format=json",
                mimeType: "",
                body: [],
                timeout: NoTimeout,
            }
            when sendHttpReq request |> Task.result! is
                Ok response ->
                    response.body
                    |> Str.fromUtf8
                    |> Result.withDefault "Failed to decode API response"
                    |> Task.ok

                Err _ ->
                    "Failed to get response from Wikipedia"
                    |> Task.ok