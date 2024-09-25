module { sendHttpReq } -> [currentTime]

import InternalTools exposing [Tool]
import json.Json

currentTime = {
    name: tool.function.name,
    handler,
    tool,
}

tool : Tool
tool =
    tzParam = {
        name: "tz",
        type: "string",
        description: 
            """
            The timezone to get the current time for. Must be a valid canonical timezone name. Eg: "America/Chicago"
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
                    |> Str.fromUtf8
                    |> Result.withDefault "Failed to decode API response"
                    |> Task.ok

                Err _ ->
                    "Failed to get response from worldtimeapi.org"
                    |> Task.ok