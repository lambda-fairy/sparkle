{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings, TemplateHaskell, TypeFamilies #-}

module Sparkle.Types
    (
      -- * Types
      Project(..)
    , projTitle
    , projTasks
    , projRevision

    , emptyProject

    , Task(..)
    , Tasks(..)
    , taskTitle
    , taskDone

    , Pos

      -- * Actions
    , QueryProject(..)
    , QueryTask(..)
    , InsertTask(..)
    , ReplaceTask(..)
    , DeleteTask(..)

      -- * Utilities
    , queryP
    , updateP

      -- * Debugging
    , testProject
    , printTasks
    , testTasks
    , testTask

    ) where

import Data.Acid
import Data.Acid.Advanced (query', update')
import Data.Aeson hiding ((.=))
import Data.Aeson.TH
import Data.Data (Data, Typeable)
import qualified Data.HashMap.Strict as H
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.SafeCopy ()
import qualified Data.List.NonEmpty as L
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T

import Sparkle.Common

-- | A Sparkle project.
data Project = Project
    { _projTitle    :: Text
      -- ^ Title of the project
    , _projTasks    :: Tasks
      -- ^ Tree of tasks
    , _projRevision :: Integer
      -- ^ Monotonically increasing revision number.  This should be
      -- incremented every time the project is modified.
    } deriving (Show, Data, Typeable)

-- | The set of tasks in the project.
newtype Tasks = Tasks { getTasks :: Forest Task }
    deriving (Show, Data, Typeable)

-- | A single task.
data Task = Task
    { _taskTitle :: Text
    , _taskDone  :: Bool
    } deriving (Show, Data, Typeable)

-- | A position in the task tree, represented as a list of indices.
type Pos = NonEmpty Integer

$(concatMapM makeLenses [''Task, ''Project])
$(concatMapM (deriveSafeCopy 0 'base) [''Task, ''Tasks, ''Project])

emptyProject :: Project
emptyProject = Project "Untitled Project" (Tasks []) 0


withTasks :: (Tasks -> Tasks) -> Update Project ()
withTasks f = withTasks' $ \tasks -> ((), f tasks)

withTasks' :: (Tasks -> (a, Tasks)) -> Update Project a
withTasks' f = do
    projRevision += 1
    tasks <- use projTasks
    let (a, tasks') = f tasks
    projTasks .= tasks'
    return a

elemAt :: Integral i => i -> [a] -> Maybe a
elemAt i = listToMaybe . genericDrop i

insertAt :: Integral i => i -> a -> [a] -> [a]
insertAt i a xs = before ++ a : after
  where (before, after) = genericSplitAt i xs


-- Pure functions ------------------------------------------------------

queryTask' :: Pos -> Tasks -> Maybe Task
queryTask' is_ (Tasks forest_) = go is_ forest_
  where
    go is forest = case L.uncons is of
        (i, Nothing)  -> elemAt i forest >>= \(Node x _      ) -> return x
        (i, Just is') -> elemAt i forest >>= \(Node _ forest') -> go is' forest'

insertTask'
    :: Pos   -- ^ Position at which to insert the task
    -> Task  -- ^ The new task object
    -> Tasks -> Tasks
insertTask' is_ x (Tasks forest_) = Tasks (go is_ forest_)
  where
    go is forest = case L.uncons is of
        (i, Nothing) -> insertAt i (Node x []) forest
        (i, Just is') -> case genericSplitAt i forest of
            (before, node:after) -> before ++ [over branches (go is') node] ++ after
            _ -> forest ++ [Node x []]

replaceTask'
    :: Pos   -- ^ The position of the task
    -> Task  -- ^ The new task object
    -> Tasks -> Tasks
replaceTask' is_ x (Tasks forest_) = Tasks (go is_ forest_)
  where
    go is forest = case L.uncons is of
        (i, Nothing) -> case genericSplitAt i forest of
            (before, node:after) -> before ++ [set root x node] ++ after
            _ -> fallback forest
        (i, Just is') -> case genericSplitAt i forest of
            (before, node:after) -> before ++ [over branches (go is') node] ++ after
            _ -> fallback forest
    -- If the task tree has changed in the mean time, add the task to
    -- the end of the branch
    fallback = (++ [Node x []])

deleteTask'
    :: Pos  -- ^ Task to delete
    -> Tasks -> Tasks
deleteTask' is_ (Tasks forest_) = Tasks (go is_ forest_)
  where
    go is forest = case L.uncons is of
        (i, Nothing) -> case genericSplitAt i forest of
            (before, (Node _ cs):after) -> before ++ cs ++ after
            _ -> forest
        (i, Just is') -> case genericSplitAt i forest of
            (before, node:after) -> before ++ [over branches (go is') node] ++ after
            _ -> forest


-- Acidic functions ----------------------------------------------------

queryProject :: Query Project Project
queryProject = ask

queryTask :: Pos -> Query Project (Maybe Task)
queryTask pos = queryTask' pos <$> view projTasks

insertTask :: Pos -> Task -> Update Project ()
insertTask pos x = withTasks (insertTask' pos x)

replaceTask :: Pos -> Task -> Update Project ()
replaceTask pos x = withTasks (replaceTask' pos x)

deleteTask :: Pos -> Update Project ()
deleteTask pos = withTasks (deleteTask' pos)

$(makeAcidic ''Project ['queryProject, 'queryTask, 'insertTask, 'replaceTask, 'deleteTask])


-- Helper functions ----------------------------------------------------

queryP
    :: (QueryEvent event, EventState event ~ Project,
        MonadIO m, MonadReader (AcidState Project) m)
    => event
    -> m (EventResult event)
queryP event = do
    acid <- ask
    query' acid event

updateP
    :: (UpdateEvent event, EventState event ~ Project,
        MonadIO m, MonadReader (AcidState Project) m)
    => event
    -> m (EventResult event)
updateP event = do
    acid <- ask
    update' acid event


-- JSON serialization --------------------------------------------------

instance FromJSON Tasks where
    parseJSON = (Tasks <$>) . parseForest
      where
        parseForest = mapM parseTree <=< parseJSON
        parseTree
            = withObject "tree node" $ \v -> do
                data_     <- v .: "data"
                children_ <- v .: "children"
                Node data_ <$> parseForest children_

instance ToJSON Tasks where
    toJSON = renderForest . getTasks
      where
        renderForest = toJSON . map renderTree
        renderTree (Node data_ children_)
            = Object $ H.fromList [("data", toJSON data_), ("children", renderForest children_)]

$(deriveJSON (stripTypeName "task") ''Task)
$(deriveJSON (stripTypeName "proj") ''Project)


-- Debugging -----------------------------------------------------------

testProject :: Project
testProject = emptyProject
    & projTitle .~ "Test Project"
    & projTasks .~ testTasks

printTasks :: Tasks -> IO ()
printTasks = putStrLn . drawForest . map (fmap show) . getTasks

testTasks :: Tasks
testTasks = Tasks
    [ Node (Task ">\")_" True)
        [ Node (Task "Duck" False)   []
        , Node (Task "Duck" True)    []
        , Node (Task "Goose!" False) []
        ]
    , Node (Task "\x2192 also utf-8 is c\x014D\x014Dl \x2190" False)
        [ Node (Task "<script>alert('Look at me! I fail at XSS!')</script>" False) []
        ]
    ]

testTask :: Task
testTask = Task (T.replicate 30 "#") True
