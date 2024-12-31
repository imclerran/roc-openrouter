## A prebuilt tool for interacting with the WorldTimeApi.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [currentTime]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [(currentTime.name, currentTime.handler)]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.appendUserMessage previousMessages newMessage
## response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
## updatedMessages = updateMessagesFromResponse response messages 
##     |> Tools.handleToolCalls! client toolHandlerMap
## ```
module { sendHttpReq } -> [currentTime]

import InternalTools exposing [Tool]
import json.Json

## Expose name, handler and tool for the currentTime.
##
## This tool allows the model to get the current time data for a given timezone.
currentTime : { name : Str, handler : Str -> Task Str *, tool : Tool }
currentTime = {
    name: tool.function.name,
    handler,
    tool,
}

## Tool definition for the currentTime function
tool : Tool
tool =
    tzParam = {
        name: "tz",
        type: "string",
        description: 
            """
            The timezone to get the current time for. Must be a valid canonical timezone name. Eg: 'America/Chicago'
            """,
        required: Bool.true,
    }
    InternalTools.buildTool 
        "currentTime" 
        """
        Get the current time data for a given timezone. This includes: utc_offset, timezone, day_of_week, day_of_year,
        datetime, utc_datetime, unixtime, raw_offset, week_number, dst, abbreviation, dst_offset, dst_from, dst_until.
        """ 
        [tzParam]

## Handler for the currentTime tool
handler : Str -> Task Str _
handler = \args ->
    decoded : Decode.DecodeResult { tz : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { tz } ->
            request = {
                method: Get,
                headers: [],
                url: "http://worldtimeapi.org/api/timezone/$(tz)",
                mimeType: "application/json",
                body: [],
                timeout: NoTimeout,
            }
            when sendHttpReq request |> Task.result! is
                Ok response ->
                    response.body
                    |> removeIp
                    |> Str.fromUtf8
                    |> Result.withDefault "Failed to decode API response"
                    |> Task.ok

                Err _ ->
                    "Failed to get response from worldtimeapi.org"
                    |> Task.ok

## WorldTimeApi response, with client ip removed
ApiResponse : {
    utcOffset: Str,
    timezone: Str,
    dayOfWeek: U32,
    dayOfYear: U32,
    datetime: Str,
    utcDatetime: Str,
    unixtime: U32,
    rawOffset: I32,
    weekNumber: U32,
    dst: Bool,
    abbreviation: Str,
    dstOffset: I32,
    dstFrom: Str,
    dstUntil: Str,
}

## Remove the client ip from the response to ensure no personal data sent to the model
removeIp : List U8 -> List U8
removeIp = \bytes ->
    decoded : Decode.DecodeResult ApiResponse
    decoded = Decode.fromBytesPartial bytes (Json.utf8With { fieldNameMapping: SnakeCase })
    when decoded.result is
        Ok response -> response |> Encode.toBytes (Json.utf8With { fieldNameMapping: SnakeCase })
        Err _ -> "Failed to decode response" |> Str.toUtf8