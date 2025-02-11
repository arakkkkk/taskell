module Taskell.Events.Actions.Insert
    ( event
    ) where

import ClassyPrelude

import Control.Lens ((&), (.~), (^.))

import           Graphics.Vty.Input.Events       (Event (EvKey), Key (KEnter, KEsc, KChar), Modifier (MCtrl))
import           Taskell.Events.State
import           Taskell.Events.State.Types
import           Taskell.Events.State.Types.Mode (InsertMode (..), InsertType (..), Mode (Insert))
import qualified Taskell.UI.Draw.Field           as F (event)

event :: Event -> Stateful
event (EvKey KEnter _) state =
    case state ^. mode of
        Insert IList ICreate _ ->
            (write =<<) . (startCreate =<<) . (newItem =<<) . (store =<<) $ createList state
        Insert IList IEdit _ -> (write =<<) . (normalMode =<<) $ finishListTitle state
        Insert ITask ICreate _ ->
            (write =<<) . (below =<<) . (removeBlank =<<) . (store =<<) $ finishTask state
        Insert ITask IEdit _ ->
            (write =<<) . (removeBlank =<<) . (normalMode =<<) $ finishTask state
        _ -> pure state
event (EvKey KEsc _) state =
    case state ^. mode of
        Insert IList ICreate _ -> (normalMode =<<) . (write =<<) $ createList state
        Insert IList IEdit _ -> (write =<<) . (normalMode =<<) $ finishListTitle state
        Insert ITask _ _ -> (write =<<) . (removeBlank =<<) . (normalMode =<<) $ finishTask state
        _ -> pure state
event (EvKey (KChar c) [MCtrl]) state =
    case c of
        c -> case state ^. mode of
                    Insert IList ICreate _ -> (normalMode =<<) . (write =<<) $ createList state
                    Insert IList IEdit _ -> (write =<<) . (normalMode =<<) $ finishListTitle state
                    Insert ITask _ _ -> (write =<<) . (removeBlank =<<) . (normalMode =<<) $ finishTask state
                    _ -> pure state

event e state =
    pure $
    case state ^. mode of
        Insert iType iMode field -> state & mode .~ Insert iType iMode (F.event e field)
        _                        -> state
