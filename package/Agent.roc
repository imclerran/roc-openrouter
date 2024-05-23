module [Agent]

import Client exposing [Client]

Agent : {
    role: Str,
    goal: Str,
    backstory: Str,
    client: Client,
}
