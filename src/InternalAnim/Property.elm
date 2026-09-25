module InternalAnim.Property exposing (Prop(..))

{-| Declarative CSS attributes shared by Animator and the renderer. Rendering
itself lives exclusively in InternalAnim.Render.
-}

import Color
import InternalAnim.Css.Props as Props
import InternalAnim.Move as Move


type Prop
    = Prop Int String (Move.Move Float) Props.Format
    | ColorProp String (Move.Move Color.Color)
