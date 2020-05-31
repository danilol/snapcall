module Page.Call.ScenarioConfig exposing (prepare)


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
