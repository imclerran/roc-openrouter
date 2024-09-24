module { sendHttpReq } -> [wikipediaSearch, wikipediaSearchTool, wikipediaParse, wikipediaParseTool]

import json.Json
import InternalTools exposing [Tool]

baseUrl = "https://en.wikipedia.org/w/api.php"

## Tool for the wikepedia search function
wikipediaSearchTool : Tool
wikipediaSearchTool =
    queryParam = {
        name: "search",
        type: "string",
        description: 
            """
            The search query to use. This can be a single word or a phrase, however it must be URL encoded.
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
wikipediaSearch : Str -> Task Str _
wikipediaSearch = \args ->
    decoded : Decode.DecodeResult { search : Str, limit : U32 }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { search, limit } ->
            request = {
                method: Get,
                headers: [],
                url: "$(baseUrl)?action=opensearch&search=$(search)&limit=$(Num.toStr limit)&namespace=0&format=json",
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
                    
## Tool for the wikipedia parse function
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
wikipediaParse : Str -> Task Str _
wikipediaParse = \args ->
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