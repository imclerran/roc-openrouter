module { sendHttpReq } -> [Tool, ToolCall, buildTool, callTools, dispatchToolCalls] 

import json.Option exposing [Option]
import InternalTools
import Chat
import Client exposing [Client]

Tool : InternalTools.Tool
ToolCall : InternalTools.ToolCall

## The OpenAI ChatML standard message used to query the AI model.
Message : {
    role : Str,
    content : Str,
    toolCalls : Option (List ToolCall),
    toolCallId : Option Str,
    name : Option Str,
}

## Represents an HTTP response.
HttpResponse : {
    url : Str,
    statusCode : U16,
    statusText : Str,
    headers : List HttpHeader,
    body : List U8,
}

HttpHeader : {
    key : Str,
    value : Str,
}

## Using the given toolHandlerMap, check the last message for tool calls, call all
## the tools in the tool call list, send the results back to the model, and handle 
## any additional tool calls that may have been generated. When no more tool calls
## are present, return the updated list of messages.
## 
## The toolHandlerMap is a dictionary mapping tool function names to functions 
## that take the arguments as a JSON string, parse the json, and return the tool's response.
callTools : List Message, Client, Dict Str (Str -> Task Str _) -> Task (List Message) _
callTools = \messages, client, toolHandlerMap ->
    when List.last messages is
        Ok { role, toolCalls: tcs } if role == "assistant" ->
            when Option.get tcs is
                Some toolCalls ->
                    toolMessages = dispatchToolCalls! toolCalls toolHandlerMap
                    messagesWithTools = List.join [messages, toolMessages]
                    response = sendHttpReq (Chat.buildHttpRequest client messagesWithTools {}) |> Task.result!
                    messagesWithResponse = getMessagesFromResponse messagesWithTools response
                    callTools messagesWithResponse client toolHandlerMap

                None -> Task.ok messages

        _ -> Task.ok messages

## Dispatch the tool calls to the appropriate tool handler functions and return the list of tool messages.
##
## The toolHandlerMap is a dictionary mapping tool function names to functions 
## that take the arguments as a JSON string, parse the json, and return the tool's response.
dispatchToolCalls : List ToolCall, Dict Str (Str -> Task Str _) -> Task (List Message) _
dispatchToolCalls = \toolCallList, toolHandlerMap ->
    Task.loop { toolCalls: toolCallList, toolMessages: [] } \{ toolCalls, toolMessages } ->
        when List.first toolCalls is
            Ok toolCall ->
                when toolHandlerMap |> Dict.get toolCall.function.name is
                    Ok parseAndCall ->
                        toolMessage = callTool! toolCall parseAndCall
                        updatedToolMessages = List.append toolMessages toolMessage
                        Task.ok (Step { toolCalls: (List.dropFirst toolCalls 1), toolMessages: updatedToolMessages })
                    
                    _ -> 
                        Task.ok (Step { toolCalls: (List.dropFirst toolCalls 1), toolMessages })

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

## Get the messages from the response and return the updated list of messages.
getMessagesFromResponse : List Message, Result HttpResponse _ -> List Message
getMessagesFromResponse = \messages, responseRes ->
    when responseRes is
        Ok response ->
            when Chat.decodeTopMessageChoice response.body is
                Ok message -> List.append messages message
                _ -> messages

        Err (HttpErr _) -> messages