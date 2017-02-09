module MouseMovement exposing (Drag)

import Mouse exposing (Position)


type alias Drag =
    { start : Position
    , current : Position
    , topLeftPos : Position
    }
