module [
    Tool,
    FunctionParameter,
    ToolCall,
    ToolChoice,
    injectTools,
    injectToolChoice,
    buildTool,
    callTools,
]

import json.Json
import json.Option exposing [Option]

## A tool that can be called by the AI model.
Tool : {
    type : Str,
    function : {
        name : Str,
        description : Str,
        parameters : {
            type : Str,
            properties : Dict Str FunctionParameter,
        },
        required : List Str,
    },
}

## A parameter for a tool function.
FunctionParameter : {
    type : Str,
    description : Str,
}

## A call from the model to a tool.
ToolCall : {
    id : Str,
    type : Str,
    function : {
        name : Str,
        arguments : Str,
    },
}

## Set the tool the model should use to process the current message.
ToolChoice : [None, Auto, ToolName Str]

## The OpenAI ChatML standard message used to query the AI model.
Message : {
    role : Str,
    content : Str,
    toolCalls : Option (List ToolCall),
    toolCallId : Option Str,
    name : Option Str,
}

## Using the given toolHandlerMap, call all the tools in the tool call list,
## and return the list of tool messages to send to the model.
##
## The toolHandlerMap is a dictionary mapping tool function names to functions
## that take the arguments as a JSON string, parse the json, and return the tool's response.
callTools : List ToolCall, Dict Str (Str -> Task Str _) -> Task (List Message) _
callTools = \toolCallList, toolHandlerMap ->
    Task.loop { toolCalls: toolCallList, toolMessages: [] } \{ toolCalls, toolMessages } ->
        when List.first toolCalls is
            Ok toolCall ->
                when toolHandlerMap |> Dict.get toolCall.function.name is
                    Ok parseAndCall ->
                        toolMessage = callTool! toolCall parseAndCall
                        updatedToolMessages = List.append toolMessages toolMessage
                        Task.ok (Step { toolCalls: List.dropFirst toolCalls 1, toolMessages: updatedToolMessages })

                    _ ->
                        Task.ok (Step { toolCalls: List.dropFirst toolCalls 1, toolMessages })

            Err ListWasEmpty -> Task.ok (Done toolMessages)

## Call the given tool function with the given arguments and return the tool message.
callTool : ToolCall, (Str -> Task Str err) -> Task Message err
callTool = \toolCall, parseArgsAndCall ->
    Task.map (parseArgsAndCall toolCall.function.arguments) \content -> {
        role: "tool",
        content,
        toolCalls: Option.none {},
        toolCallId: Option.some toolCall.id,
        name: Option.some toolCall.function.name,
    }

## Build a tool object with the given name, description, and parameters.
buildTool : Str, Str, List { name : Str, type : Str, description : Str, required : Bool } -> Tool
buildTool = \name, description, parameters ->
    properties =
        parameters
        |> List.map \{ name: n, type: t, description: d } -> (n, { type: t, description: d })
        |> Dict.fromList
    {
        type: "function",
        function: {
            name,
            description,
            parameters: {
                type: "object",
                properties,
            },
            required: parameters
            |> List.dropIf \param -> !param.required
            |> List.map \param -> param.name,
        },
    }

## Inject the tools list into the request body. This is necessary because Encode does not support Dict types.
injectTools : List U8, List Tool -> List U8
injectTools = \requestBody, tools ->
    { before, others } = List.split requestBody ((List.len requestBody) - 1)
    toolsJson = encodeTools tools
    [before, (", \"tools\": " |> Str.toUtf8), toolsJson, others] |> List.join

## Inject the tool choice into the request body. This is necessary because Encode does not support Dict types.
injectToolChoice : List U8, ToolChoice -> List U8
injectToolChoice = \requestBody, toolChoice ->
    { before, others } = List.split requestBody ((List.len requestBody) - 1)
    toolChoiceJson =
        when toolChoice is
            None -> "none" |> Str.toUtf8
            Auto -> "auto" |> Str.toUtf8
            ToolName toolName ->
                """
                {"type": "function", "function": { "name": "$(toolName)"}}
                """
                |> Str.toUtf8
    [before, (", \"toolChoice\": " |> Str.toUtf8), toolChoiceJson, others] |> List.join

## Encode the tools list into JSON UTF-8 bytes
encodeTools : List Tool -> List U8
encodeTools = \tools ->
    tools
    |> List.map toolToJson
    |> Str.joinWith ", "
    |> \toolsContent -> "[$(toolsContent)]"
    |> Str.toUtf8

## Convert a tool object to a JSON string.
toolToJson : Tool -> Str
toolToJson = \tool ->
    required = Encode.toBytes tool.function.required (Json.utf8With { fieldNameMapping: SnakeCase }) |> Str.fromUtf8 |> Result.withDefault "[]"
    """
    {"type": "$(tool.type)", "function": {"name": "$(tool.function.name)", "description": "$(tool.function.description)", "parameters": {"type": "$(tool.function.parameters.type)", "properties": $(propertiesToJson tool.function.parameters.properties)}, "required": $(required)}}
    """

## Convert a dictionary of function parameters to a JSON string.
propertiesToJson : Dict Str FunctionParameter -> Str
propertiesToJson = \properties ->
    Dict.toList properties
    |> List.map \(paramName, parameter) ->
        """
        "$(paramName)": {"type": "$(parameter.type)", "description": "$(parameter.description)"}
        """
    |> Str.joinWith ", "
    |> \dictContent -> "{$(dictContent)}"
