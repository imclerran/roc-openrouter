module { sendHttpReq } -> [
    Tool,
    ToolCall,
    buildTool,
    handleToolCalls,
    dispatchToolCalls,
]

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
## any additional tool calls that may have been generated. If or when no more tool
## calls are present, return the updated list of messages.
##
## The toolHandlerMap is a dictionary mapping tool function names to functions
## that take the arguments as a JSON string, parse the json, and return the tool's response.
# handleToolCalls : List Message, Client, Dict Str (Str -> Task Str _) -> Task (List Message) _
# handleToolCalls = \messages, client, toolHandlerMap ->
#     handleToolCallsLogged messages client toolHandlerMap (\_ -> Task.ok {}) None
# when List.last messages is
#     Ok { role, toolCalls: tcs } if role == "assistant" ->
#         when Option.get tcs is
#             Some toolCalls ->
#                 toolMessages = dispatchToolCalls! toolCalls toolHandlerMap
#                 messagesWithTools = List.join [messages, toolMessages]
#                 response = sendHttpReq (Chat.buildHttpRequest client messagesWithTools {}) |> Task.result!
#                 messagesWithResponse = getMessagesFromResponse messagesWithTools response
#                 handleToolCalls messagesWithResponse client toolHandlerMap

#             None -> Task.ok messages

#     _ -> Task.ok messages

# Logger err : Str -> Task {} err
LogLevel : [None, Basic, Verbose]
LogMessage : [Basic Str, Verbose Str]

## Using the given toolHandlerMap, check the last message for tool calls, call all
## the tools in the tool call list, send the results back to the model, and handle
## any additional tool calls that may have been generated. If or when no more tool
## calls are present, return the updated list of messages.
##
## The toolHandlerMap is a dictionary mapping tool function names to functions
## that take the arguments as a JSON string, parse the json, and return the tool's response.
handleToolCalls : List Message, Client, Dict Str (Str -> Task Str _), { logger ? (Str -> Task {} _), logLevel ? LogLevel } -> Task (List Message) _
handleToolCalls = \messages, client, toolHandlerMap, { logger ? (\_ -> Task.ok {}), logLevel ? None } ->
    when List.last messages is
        Ok { role, toolCalls: tcs } if role == "assistant" ->
            when Option.get tcs is
                Some toolCalls ->
                    toolMessages = dispatchToolCalls! toolCalls toolHandlerMap { logger,  logLevel }
                    messagesWithTools = List.join [messages, toolMessages]
                    response = sendHttpReq (Chat.buildHttpRequest client messagesWithTools {}) |> Task.result!
                    messagesWithResponse = getMessagesFromResponse messagesWithTools response
                    handleToolCalls messagesWithResponse client toolHandlerMap { logger, logLevel }

                None -> Task.ok messages

        _ -> Task.ok messages

## Dispatch the tool calls to the appropriate tool handler functions and return the list of tool messages.
##
## The toolHandlerMap is a dictionary mapping tool function names to functions
## that take the arguments as a JSON string, parse the json, and return the tool's response.
dispatchToolCalls : List ToolCall, Dict Str (Str -> Task Str _), { logger ? (Str -> Task {} _), logLevel ? LogLevel} -> Task (List Message) _
dispatchToolCalls = \toolCallList, toolHandlerMap, { logger ? (\_ -> Task.ok {}), logLevel ? None } ->
    Task.loop { toolCalls: toolCallList, toolMessages: [] } \{ toolCalls, toolMessages } ->
        when List.first toolCalls is
            Ok toolCall ->
                toolName = toolCall.function.name
                when toolHandlerMap |> Dict.get toolName is
                    Ok handler ->
                        log! logger logLevel (Basic "Calling tool: $(toolName)")
                        message = callTool! toolCall handler
                        log! logger logLevel (Verbose "Tool answered with:\n $(message.content)")
                        updatedToolMessages = List.append toolMessages message
                        Task.ok (Step { toolCalls: List.dropFirst toolCalls 1, toolMessages: updatedToolMessages })

                    _ ->
                        Task.ok (Step { toolCalls: List.dropFirst toolCalls 1, toolMessages })

            Err ListWasEmpty -> Task.ok (Done toolMessages)

## Log the given message based on the specified log level.
log : (Str -> Task {} _), LogLevel, LogMessage -> Task {} _
log = \logger, logLevel, logMessage ->
    when logMessage is
        Basic text if logLevel != None -> logger text
        Verbose text if logLevel == Verbose -> logger text
        _ -> Task.ok {}

## Call the given tool function with the given arguments and return the tool message.
callTool : ToolCall, (Str -> Task Str err) -> Task Message err
callTool = \toolCall, handler ->
    Task.map (handler toolCall.function.arguments) \content -> {
        role: "tool",
        content,
        toolCalls: Option.none {},
        toolCallId: Option.some toolCall.id,
        name: Option.some toolCall.function.name,
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

## Build a tool object with the given name, description, and parameters.
buildTool = InternalTools.buildTool
