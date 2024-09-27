module { sendHttpReq, getEnvVar } -> [geocoding, currentWeather]

import json.Json
import InternalTools exposing [Tool]

## Expose name, handler and tool for geocoding
geocoding = {
    name: geocodingTool.function.name,
    handler: geocodingHandler,
    tool: geocodingTool,
}

## Tool definition for the geocoding function
geocodingTool : Tool
geocodingTool =
    queryParam = {
        name: "q",
        type: "string",
        description:
        """
        City name, state code (only for the US) and country code divided by comma. 
        Please use ISO 3166 country codes. city name, state code, and country code
        are all required for US cities, and city name and country code are required
        for all other cities. Do not leave out the 2 letter state code for US cities.
        For example, to get the location of New York City, you would use 
        "Chicago,IL,US". For London, you would use "London,GB". You should also make sure 
        that any spaces in city names are URL encoded. Last, make sure there are no spaces
        between the city name, state code, and country code.
        """,
        required: Bool.true,
    }
    InternalTools.buildTool "geocoding" "Geocode a location using the openweathermap.org API" [queryParam]

## Handler for the geocoding tool
geocodingHandler : Str -> Task Str _
geocodingHandler = \args ->
    decoded : Decode.DecodeResult { q : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { q } ->
            apiKey = getEnvVar! "OPENWEATHERMAP_API_KEY"
            request = {
                method: Get,
                headers: [],
                url: "http://api.openweathermap.org/geo/1.0/direct?q=$(q)&appid=$(apiKey)",
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
                    "Failed to get response from openweathermap.org"
                    |> Task.ok

## Expose name, handler and tool for currentWeather
currentWeather = {
    name: currentWeatherTool.function.name,
    handler: currentWeatherHandler,
    tool: currentWeatherTool,
}

## Tool definition for the currentWeather function
currentWeatherTool : Tool
currentWeatherTool =
    latParam = {
        name: "lat",
        type: "number",
        description: "The latitude of the location to get the current weather for. Decimal from -90 to 90.",
        required: Bool.true,
    }
    lonParam = {
        name: "lon",
        type: "number",
        description: "The longitude of the location to get the current weather for. Decimal from -180 to 180.",
        required: Bool.true,
    }
    unitsParam = {
        name: "units",
        type: "string",
        description: 
        """
        The units to return the weather in. Can be 'standard', 'metric', or 'imperial'. 
        Standard is Kelvin, metric is Celsius, and imperial is Fahrenheit.
        """,
        required: Bool.true,
    }
    InternalTools.buildTool "currentWeather" "Get the current weather for a location using the openweathermap.org API" [latParam, lonParam, unitsParam]

## Handler for the currentWeather tool
currentWeatherHandler : Str -> Task Str _
currentWeatherHandler = \args ->
    decoded : Decode.DecodeResult { lat : F32, lon : F32, units : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { lat, lon, units } ->
            exclude = "minutely,hourly,daily,alerts"
            apiKey = getEnvVar! "OPENWEATHERMAP_API_KEY"
            request = {
                method: Get,
                headers: [],
                url: "https://api.openweathermap.org/data/3.0/onecall?lat=$(Num.toStr lat)&lon=$(Num.toStr lon)&exclude=$(exclude)&units=$(units)&appid=$(apiKey)",
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
                    "Failed to get response from openweathermap.org"
                    |> Task.ok

