module Page.Call.ScenarioConfig exposing (prepare)

{-| The scenarios configuration.

Scenario 1:
presenter
mobileFlag = false -> VNC
/call

Scenario 2:
guest
mobileFlag = true -> VNC
/call?scenario=guest

Scenario 3:
presenter
mobileFlag = false -> WebRTC
/call?scenario=WebRTC

Scenario 4:
guest
mobileFlag = false -> WebRTC
/call?scenario=WebRTC

Scenario 5:
presenter
mobileFlag = false -> VNC
/call?scenario=error
ERROR

-}


prepare : Maybe String -> { error : Bool, mobileFlag : Bool, rtcPriority : Bool }
prepare scenarioParam =
    case scenarioParam of
        --  if has param => Guest else => Presenter
        Nothing ->
            { mobileFlag = False, rtcPriority = False, error = False }

        Just p ->
            case String.toLower p of
                "guest" ->
                    { mobileFlag = True, rtcPriority = False, error = False }

                "webrtc" ->
                    { mobileFlag = False, rtcPriority = True, error = False }

                "error" ->
                    { mobileFlag = False, rtcPriority = False, error = True }

                _ ->
                    { mobileFlag = False, rtcPriority = False, error = False }
