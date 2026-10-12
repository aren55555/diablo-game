port module Ports exposing (frame, gamepads, music, sprites)

{-| The boundary with the TypeScript platform layer (platform/main.ts).
-}

import Json.Decode as D
import Json.Encode as E


{-| Every animation frame: { draws, sfx, muted }.
-}
port frame : E.Value -> Cmd msg


{-| Sent once at startup: { palette, sprites }.
-}
port sprites : E.Value -> Cmd msg


{-| Background chiptune: "title" | "climb" | "boss" | "win" | "none".
-}
port music : String -> Cmd msg


{-| Gamepad snapshots: [{ left, right, up, down, start }] for pads 0-3.
-}
port gamepads : (D.Value -> msg) -> Sub msg
