module Design exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Msg exposing (Msg)


numberThumb : Int -> Input.Thumb
numberThumb number =
    Input.thumb
        [ Element.height (Element.px 25)
        , Element.width (Element.px 25)
        , Element.inFront
            (el
                [ Background.color (rgb255 255 255 255)
                , Border.width 2
                , Border.rounded 10
                , Element.centerY
                , Element.centerX
                ]
                (Element.text (String.fromInt number))
            )
        , Element.centerY
        , Element.centerX
        ]



--make one with editable ends next


numberSlider : String -> Float -> Float -> Maybe Float -> Float -> (Float -> Msg) -> Element Msg
numberSlider label min_ max_ step_ val cmd =
    Input.slider
        [ Element.height (Element.px 30)
        , Element.behindContent
            (Element.row [ Element.width Element.fill, Element.centerY ]
                [ Element.text (String.fromFloat min_)
                , Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Background.color (rgb255 128 128 128)
                    , Border.rounded 2
                    ]
                    Element.none
                , Element.text (String.fromFloat max_)
                ]
            )
        ]
        { onChange = cmd
        , label =
            Input.labelAbove [ Element.centerX ]
                (text label)
        , min = min_
        , max = max_
        , step = step_
        , value = val
        , thumb =
            numberThumb (floor val)
        }



--maxMinAveSlider : String -> Float -> Float -> Maybe Float -> Float -> (Float -> Msg) -> Element Msg
--maxMinAveSlider label min_ max_ step_ val cmd =
--    Input.slider
--        [ Element.height (Element.px 30)
--        , Element.behindContent
--            (Element.row [ Element.width Element.fill, Element.centerY ]
--                [ Element.text (String.fromFloat min_)
--                , Element.el
--                    [ Element.width Element.fill
--                    , Element.height (Element.px 2)
--                    , Element.centerY
--                    , Background.color (rgb255 128 128 128)
--                    , Border.rounded 2
--                    ]
--                    Element.none
--                , Element.text (String.fromFloat max_)
--                ]
--            )
--        ]
--        { onChange = cmd
--        , label =
--            Input.labelAbove [ Element.centerX ]
--                (text label)
--        , min = min_
--        , max = max_
--        , step = step_
--        , value = val
--        , thumb =
--            numberThumb (floor val)
--        }
--eventually move more stuff here, like the slider implem, etc.
