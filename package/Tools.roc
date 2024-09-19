module [Tool, ToolCall, buildTool, callTools]

import InternalTools

Tool : InternalTools.Tool
ToolCall : InternalTools.ToolCall

buildTool = InternalTools.buildTool
callTools = InternalTools.callTools
