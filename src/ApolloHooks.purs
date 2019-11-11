module ApolloHooks
  ( QueryState(..)
  , Cache
  , Options
  , gql
  , useMutation
  , useQuery
  , module GraphQL.Language.AST
  ) where

import Prelude
import React.Basic.Hooks (Hook, UseEffect, type (/\), (/\))
import Data.Nullable (null, toMaybe, Nullable)
import Data.Maybe (Maybe(..))
import React.Basic.Hooks as React
import Effect.Class (liftEffect)
import GraphQL.Language.AST (DocumentNode)
import Data.Function.Uncurried (Fn1, runFn1, Fn2, runFn2)
import Effect.Uncurried (runEffectFn2, EffectFn2, runEffectFn1, EffectFn1, mkEffectFn2)
import Effect (Effect)
import React.Basic.Hooks.Internal (unsafeHook)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Control.Promise (Promise, fromAff)
import Control.Promise as Promise
import Data.Tuple.Native (T2, prj)
import Data.Typelevel.Num.Reps (d0, d1)
import Prim.Row (class Union)

data QueryState resultType
  = Loading
  | Error (Array String)
  | Data resultType

type JSCache p x
  = { readQuery :: EffectFn1 { query :: DocumentNode } (p)
    , writeQuery :: EffectFn1 { query :: DocumentNode, data :: p } (Unit)
    | x
    }

type Cache mutation otherFields
  = { readQuery :: { query :: DocumentNode } -> Effect mutation
    , writeQuery :: { query :: DocumentNode, data :: mutation } -> Effect Unit
    | otherFields
    }

type JSOptions query otherFields mutation
  = ( update :: EffectFn2 (JSCache (Record query) otherFields) (Record mutation) (Unit) )

type Options query otherFields mutation
  = ( update ::
      ( Cache (Record query) otherFields ->
        (Record mutation) ->
        Effect Unit
      )
    )

foreign import runThisFn1 :: forall this a b. String -> this -> EffectFn1 a b

foreign import _gql :: Fn1 String DocumentNode

gql :: String -> DocumentNode
gql string = runFn1 _gql string

type JSQueryResult d
  = { loading :: Nullable Boolean
    , error :: Nullable (Array String)
    , data :: Nullable (Record d)
    }

foreign import _useQuery ::
  forall d.
  EffectFn2 DocumentNode
    {}
    (JSQueryResult d)

foreign import _useMutation ::
  forall v mutation query otherFields opts _opts.
  EffectFn2 DocumentNode
    (Record opts)
    ( T2 (EffectFn1 (Record v) (Promise (Record mutation)))
        (JSQueryResult mutation)
    )

useMutation ::
  forall v mutation query otherFields opts _opts.
  DocumentNode ->
  Record opts ->
  Hook (UseEffect Unit)
    ( ( Record (v) ->
        Aff (Record mutation)
      )
        /\ JSQueryResult mutation
    )
useMutation mutation options = React.do
  tuple <- unsafeHook $ runEffectFn2 _useMutation mutation options
  let
    mutationFunction = prj d0 tuple
  let
    d = prj d1 tuple
  pure $ ((affFn mutationFunction) /\ (d))
  where
  affFn mutationFunction x = mapAff (runEffectFn1 mutationFunction) x

  mapAff f x = (liftEffect $ f x) >>= Promise.toAff

useQuery ::
  forall d.
  DocumentNode ->
  {} ->
  Hook
    ( UseEffect Unit
    )
    (QueryState (Record d))
useQuery query options = React.do
  result <- unsafeHook $ runEffectFn2 _useQuery query options
  pure $ (queryState $ result)

findState _ _ (Just d) = Data d

findState _ (Just error) _ = Error error

findState (Just loading) _ _ = Loading

findState _ _ _ = Error ["Something went wrong"]

queryState { loading, error, data: result } = findState l e d
  where
  l = toMaybe loading

  e = toMaybe error

  d = toMaybe result

handleUpdate updateFn =
  mkEffectFn2
    ( \jsCache d -> updateFn (cache jsCache) d
    )
  where
  cache jsCache = jsCache { readQuery = runEffectFn1 $ runThisFn1 "readQuery" jsCache, writeQuery = runEffectFn1 $ runThisFn1 "writeQuery" jsCache }
