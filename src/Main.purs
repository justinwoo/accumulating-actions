module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..))

-- | Labels to be run have to be lexicographically ordered if we don't want to deal with a symbol list
-- | You probably wouldn't use this unless you're a madman
class RunAccEffects (rl :: RL.RowList) (actions :: # Type) (i :: # Type) (o :: # Type)
  | rl i -> actions o where
  runAccEffectsImpl :: RLProxy rl -> {|actions} -> {|i} -> Effect {|o}

instance runAccEffectsNil :: RunAccEffects RL.Nil actions i i where
  runAccEffectsImpl _ _ input = pure input

instance runAccEffectsCons ::
  ( Row.Cons name ({|i} -> Effect a) actions' actions
  , IsSymbol name
  , Row.Cons name a i i'
  , Row.Lacks name i
  , RunAccEffects tail actions i' o
  ) => RunAccEffects (RL.Cons name ({|i} -> Effect a) tail) actions i o where
  runAccEffectsImpl _ actions input = do
    value <- action input
    let inter = Record.insert nameP value input
    runAccEffectsImpl tailP actions inter
    where
      nameP = SProxy :: _ name
      action = Record.get nameP actions
      tailP = RLProxy :: _ tail

runAccEffects :: forall actions actionsL results return
   . RL.RowToList actions actionsL
  => RunAccEffects actionsL actions () results
  => {|actions}
  -> ({|results} -> return)
  -> Effect return
runAccEffects actions fn = do
  results <- runAccEffectsImpl actionsLP actions {}
  pure (fn results)
   where
     actionsLP = RLProxy :: _ actionsL

main :: Effect Unit
main = do
  return :: {apple :: String, banana :: String, kiwi :: String} <- runAccEffects myAccEffects identity
  log $ show return
  where
    myAccEffects = {apple, banana, kiwi}

    apple :: {} -> Effect String
    apple r = pure "hi"

    banana :: {apple :: String} -> Effect String
    banana r = pure $ r.apple <> " banana"

    kiwi :: {apple :: String, banana :: String} -> Effect String
    kiwi r = pure $ r.banana <> " kiwi"
