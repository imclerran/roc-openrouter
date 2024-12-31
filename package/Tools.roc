module { sendHttpReq } -> [Tool, ToolCall, buildTool, handleToolCalls, handleToolCallsLogged, dispatchToolCalls, dispatchToolCallsLogged] 

# import json.Option exposing [Option]
import InternalTools
import Chat
import Client exposing [Client]

## A tool that can be called by the AI model.
## ```
## Tool : {
##     type : Str,
##     function : {
##         name : Str,
##         description : Str,
##         parameters : {
##             type : Str,
##             properties : Dict Str FunctionParameter,
##         },
##         required : List Str,
##     },
## }
## ```
Tool : InternalTools.Tool

## A call from the model to a tool.
## ```
## ToolCall : {
##     id : Str,
##     type : Str,
##     function : {
##         name : Str,
##         arguments : Str,
##     },
## }
## ```
ToolCall : InternalTools.ToolCall

## The OpenAI ChatML standard message used to query the AI model.
Message : {
    role : Str,
    content : Str,
    toolCalls : List ToolCall,
    name : Str,
    toolCallId : Str,
    cached: Bool,
}

## Represents an HTTP response.
HttpResponse : {
    url : Str,
    statusCode : U16,
    statusText : Str,
    headers : List { key : Str, value : Str },
    body : List U8,
}

## Using the given toolHandlerMap, check the last message for tool calls, call all the tools in the tool call list, send the results back to the model, and handle any additional tool calls that may have been generated. If or when no more tool calls are present, return the updated list of messages.
## 
## The Dict maps function tool names strings to roc functions that take their arguments as a JSON string, parse the json, and return the tool's response.
handleToolCalls : List Message, Client, Dict Str (Str -> Task Str *) -> Task (List Message) _
handleToolCalls = \messages, client, toolHandlerMap ->
    when List.last messages is
        Ok { role, toolCalls } if role == "assistant" ->
            if List.isEmpty toolCalls then
                Task.ok messages
            else
                toolMessages = dispatchToolCalls! toolCalls toolHandlerMap
                messagesWithTools = List.join [messages, toolMessages]
                response = sendHttpReq (Chat.buildHttpRequest client messagesWithTools {}) |> Task.result!
                messagesWithResponse = updateMessagesFromResponse messagesWithTools response
                handleToolCalls messagesWithResponse client toolHandlerMap

        _ -> Task.ok messages

handleToolCallsLogged : List Message, Client, Dict Str (Str -> Task Str a), { logger ? Str -> Task {} a } -> Task (List Message) _
handleToolCallsLogged = \messages, client, toolHandlerMap, { logger ? \_ -> Task.ok {} } ->
    when List.last messages is
        Ok { role, toolCalls } if role == "assistant" ->
            if List.isEmpty toolCalls then
                Task.ok messages
            else
                toolMessages = dispatchToolCalls! toolCalls toolHandlerMap
                messagesWithTools = List.join [messages, toolMessages]
                response = sendHttpReq (Chat.buildHttpRequest client messagesWithTools {}) |> Task.result!
                messagesWithResponse = updateMessagesFromResponse messagesWithTools response
                handleToolCallsLogged messagesWithResponse client toolHandlerMap { logger }
        
        _ -> Task.ok messages

## Dispatch the tool calls to the appropriate tool handler functions and return the list of tool messages.
##
## The Dict maps function tool names strings to roc functions that take their arguments as a JSON string, parse the json, and return the tool's response.
dispatchToolCalls : List ToolCall, Dict Str (Str -> Task Str *) -> Task (List Message) _
dispatchToolCalls = \toolCallList, toolHandlerMap ->
    Task.loop { toolCalls: toolCallList, toolMessages: [] } \{ toolCalls, toolMessages } ->
        when List.first toolCalls is
            Ok toolCall ->
                when toolHandlerMap |> Dict.get toolCall.function.name is
                    Ok handler ->
                        toolMessage = callTool! toolCall handler
                        updatedToolMessages = List.append toolMessages toolMessage
                        Task.ok (Step { toolCalls: (List.dropFirst toolCalls 1), toolMessages: updatedToolMessages })
                    
                    _ -> 
                        Task.ok (Step { toolCalls: (List.dropFirst toolCalls 1), toolMessages })

            Err ListWasEmpty -> Task.ok (Done toolMessages)

dispatchToolCallsLogged : List ToolCall, Dict Str (Str -> Task Str a), { logger ? Str -> Task {} a } -> Task (List Message) _
dispatchToolCallsLogged = \toolCallList, toolHandlerMap, { logger ? \_ -> Task.ok {} } ->
    Task.loop { toolCalls: toolCallList, toolMessages: [] } \{ toolCalls, toolMessages } ->
        when List.first toolCalls is
            Ok toolCall ->
                toolName = toolCall.function.name
                when toolHandlerMap |> Dict.get toolCall.function.name is
                    Ok handler ->
                        {} = logger! "Calling tool: $(toolName)"
                        toolMessage = callTool! toolCall handler
                        updatedToolMessages = List.append toolMessages toolMessage
                        Task.ok (Step { toolCalls: (List.dropFirst toolCalls 1), toolMessages: updatedToolMessages })
                    
                    _ -> 
                        logger! "Couldn't find tool: $(toolName)"
                        Task.ok (Step { toolCalls: (List.dropFirst toolCalls 1), toolMessages })

            Err ListWasEmpty -> Task.ok (Done toolMessages)

## Call the given tool function with the given arguments and return the tool message.
callTool : ToolCall, (Str -> Task Str err) -> Task Message err
callTool = \toolCall, handler ->
    Task.map (handler toolCall.function.arguments) \text -> {
        role: "tool",
        content: text,
        toolCalls: [],
        toolCallId: toolCall.id,
        name: toolCall.function.name,
        cached: Bool.false,
    }

## Get the messages from the response and return the updated list of messages.
updateMessagesFromResponse : List Message, Result HttpResponse _ -> List Message
updateMessagesFromResponse = \messages, responseRes ->
    when responseRes is
        Ok response ->
            when Chat.decodeTopMessageChoice response.body is
                Ok message -> List.append messages message
                _ -> messages

        Err (HttpErr _) -> messages

## decode the response from the OpenRouter API and append the first message to the list of messages
# updateMessagesFromResponse : List Message, Result HttpResponse _ -> List Message
# updateMessagesFromResponse = \messages, responseRes->
#     when responseRes is
#         Ok response ->
#             when Chat.decodeTopMessageChoice response.body is
#                 Ok message -> List.append messages message
#                 Err (ApiError err) -> Chat.appendSystemMessage messages "API error: $(err.message)" {}
#                 Err NoChoices -> Chat.appendSystemMessage messages "No choices in API response" {}
#                 Err (BadJson str) -> Chat.appendSystemMessage messages "Could not decode JSON response:\n$(str)" {}
#                 Err DecodingError -> Chat.appendSystemMessage messages "Error decoding API response" {}

#         Err (HttpErr _) -> messages

## Build a tool object with the given name, description, and parameters.
## ```
## buildTool = \name, description, parameters -> ...
## ```
## Parameters:
## - `name : Str` : The name of the tool.
## - `description : Str` : The description of the tool.
## - `parameters : List { ... }` : The parameters for the tool.
##     - `name : Str` : The name of the parameter.
##     - `type : Str` : The type of the parameter.
##     - `description : Str` : The description of the parameter.
##     - `required : Bool` : Whether the parameter is required.
## 
## Returns:
## - `Tool` : The tool object.
buildTool : Str, Str, List { name : Str, type : Str, description : Str, required : Bool } -> Tool
buildTool = InternalTools.buildTool
