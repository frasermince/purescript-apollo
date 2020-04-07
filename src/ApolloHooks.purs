module ApolloHooks
  ( QueryState(..)
  , QueryResult
  , Cache
  , Options
  , Client
  , gql
  , useMutation
  , useQuery
  , useApolloClient
  , MutationState(..)
  , handleUpdate
  , module GraphQL.Language.AST
  ) where

import Prelude
import React.Basic.Hooks (Hook, UseEffect, type (/\), (/\), UseContext, useContext)
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
import Effect.Aff (Aff, launchAff_, throwError, error)
import Control.Promise (Promise, fromAff)
import Control.Promise as Promise
import Data.Tuple.Native (T2, prj)
import Data.Typelevel.Num.Reps (d0, d1)
import Prim.Row (class Union)

data QueryState resultType
  = Loading
  | Error {message :: (String)}
  | Data resultType

data MutationState resultType
  = ErrorM {message :: (String)}
  | DataM resultType


derive instance eqQueryState :: Eq a => Eq (QueryState a)

type QueryResult d = {state :: QueryState d, refetch :: {} -> Effect Unit, networkStatus :: Int}
type Client = {resetStore :: Effect Unit}
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
    , error :: Nullable {message :: (String)}
    , data :: Nullable (Record d)
    , refetch :: EffectFn1 {} Unit
    , networkStatus :: Int
    }

foreign import _useQuery ::
  forall d opts.
  EffectFn2 DocumentNode
    (Record opts)
    (JSQueryResult d)

foreign import _useMutation ::
  forall v mutation query otherFields opts _opts.
  EffectFn2 DocumentNode
    (Record opts)
    ( T2 (EffectFn1 (Record v) (Promise (JSQueryResult mutation)))
        (JSQueryResult mutation)
    )

foreign import _useApolloClient :: Hook (UseContext Client) (Nullable Client)
useApolloClient :: Hook (UseContext Client) (Maybe Client)
useApolloClient = React.do
  client <- _useApolloClient
  pure $ toMaybe client

useMutation ::
  forall v mutation query otherFields opts _opts.
  DocumentNode ->
  Record opts ->
  Hook (UseEffect Unit)
    ( ( Record (v) ->
        Aff (Record mutation)
      )
        /\ QueryResult (Record mutation)
    )
useMutation mutation options = React.do
  tuple <- unsafeHook $ runEffectFn2 _useMutation mutation options
  let
    mutationFunction = prj d0 tuple
  let
    d = prj d1 tuple
  pure $ ((affFn mutationFunction) /\ (queryState d))
  where
  affFn mutationFunction x = do
     result <- mapAff (runEffectFn1 mutationFunction) x
     case mutationState result of
          DataM r -> pure $ r
          ErrorM m -> throwError $ error m.message

  mapAff f x = (liftEffect $ f x) >>= Promise.toAff

useQuery ::
  forall d opts.
  DocumentNode ->
  Record opts ->
  Hook
    ( UseEffect Unit
    )
    (QueryResult (Record d))
useQuery query options = React.do
  result <- unsafeHook $ runEffectFn2 _useQuery query options
  pure $ (queryState $ result)

findState _ _ (Just d) = Data d
findState _ (Just error) _ = Error error
findState (Just loading) _ _ = Loading
findState _ _ _ = Error {message: "Something went wrong"}


findStateM _ (Just d) = DataM d
findStateM (Just error) _ = ErrorM error
findStateM _ _ = ErrorM {message: "Something went wrong"}


mutationState { error, data: result } = findStateM e d
  where
  e = toMaybe error

  d = toMaybe result

queryState { loading, error, data: result, networkStatus, refetch} = {state: findState l e d, networkStatus: networkStatus, refetch: runEffectFn1 refetch}
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
