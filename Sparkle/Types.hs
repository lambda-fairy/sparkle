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
    , Tasks
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
import Data.Aeson.TH
import Data.Data (Data, Typeable)
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
type Tasks = [Task]

-- | A single task.
data Task = Task
    { _taskTitle :: Text
    , _taskDone  :: Bool
    } deriving (Show, Data, Typeable)

-- | A position in the task tree, represented as a non-negative index.
type Pos = Integer

$(concatMapM makeLenses [''Task, ''Project])
$(concatMapM (deriveSafeCopy 0 'base) [''Task, ''Project])

emptyProject :: Project
emptyProject = Project "Untitled Project" [] 0


withTasks :: (Tasks -> Tasks) -> Update Project ()
withTasks f = withTasks' $ \tasks -> ((), f tasks)

withTasks' :: (Tasks -> (a, Tasks)) -> Update Project a
withTasks' f = do
    projRevision += 1
    tasks <- use projTasks
    let (a, tasks') = f tasks
    projTasks .= tasks'
    return a


-- Pure functions ------------------------------------------------------

queryTask' :: Pos -> Tasks -> Maybe Task
queryTask' i = listToMaybe . genericDrop i

insertTask'
    :: Pos   -- ^ Position at which to insert the task
    -> Task  -- ^ The new task object
    -> Tasks -> Tasks
insertTask' i a ts = case genericSplitAt i ts of
    (before, after) -> before ++ a : after

replaceTask'
    :: Pos   -- ^ The position of the task
    -> Task  -- ^ The new task object
    -> Tasks -> Tasks
replaceTask' i a ts = case genericSplitAt i ts of
    (before, _:after) -> before ++ a : after
    _ -> ts ++ [a]

deleteTask'
    :: Pos  -- ^ Task to delete
    -> Tasks -> Tasks
deleteTask' i ts = case genericSplitAt i ts of
    (before, _:after) -> before ++ after
    _ -> ts


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

$(deriveJSON (stripTypeName "task") ''Task)
$(deriveJSON (stripTypeName "proj") ''Project)


-- Debugging -----------------------------------------------------------

testProject :: Project
testProject = emptyProject
    & projTitle .~ "Test Project"
    & projTasks .~ testTasks

printTasks :: Tasks -> IO ()
printTasks = mapM_ print

testTasks :: Tasks
testTasks =
    [ Task ">\")_" True
    , Task "Giant enemy crab" False
    , Task "Attack its weak point\nfor massive damage" True
    , Task "(actually took place in feudal Japan)" False
    , Task "\x2192 also utf-8 is c\x014D\x014Dl \x2190" False
    , Task "<script>alert('Look at me! I fail at XSS!')</script>" False
    ]

testTask :: Task
testTask = Task (T.replicate 30 "#") True
