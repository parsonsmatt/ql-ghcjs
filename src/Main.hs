{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import React.Flux
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

main :: IO ()
main = reactRender "main" lolo ()

lolo :: ReactView ()
lolo = defineControllerView "lololo" todoStore $ \todoState () ->
  div_ [] $ do
    h1_ [] "Hello, world!"
    todoList_ todoState

todoList_ :: TodoState -> ReactElementM eventHandler ()
todoList_ st = div_ $ do
  ul_ $ mapM_ todoItem_ $ todoList st

todoItem :: ReactView (Int, Todo)
todoItem = defineView "todo item" $ \(id, todo) -> 
  li_ ["key" @= id] (elemText . tdTitle $ todo)

todoItem_ :: (Int, Todo) -> ReactElementM eventHandler ()
todoItem_ todo = viewWithKey todoItem (fst todo) todo mempty

data Todo
  = Todo
  { tdTitle :: String
  , tdComplete :: Bool
  } deriving (Show, Typeable)

newtype TodoState
  = TodoState
  { todoList :: [(Int, Todo)]
  } deriving (Show, Typeable)

data TodoAction
  = TodoCreate String
  | TodoDelete Int

instance StoreData TodoState where
  type StoreAction TodoState = TodoAction
  transform (TodoCreate str) (TodoState todos) = 
    let newTodo = (1 + maximum (map fst todos), Todo str False)
     in return (TodoState (newTodo : todos))
  transform (TodoDelete int) (TodoState todos) =
    return (TodoState (filter ((int /=) . fst) todos))

todoStore :: ReactStore TodoState
todoStore = mkStore $ TodoState [(1, Todo "asdf" False)]
