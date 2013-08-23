{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies #-}

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

      -- * Actions
    , InsertTask(..)
    , ModifyTask(..)
    , DeleteTask(..)
    , QueryProject(..)

      -- * Debugging
    , testProject
    , printTasks
    , testTasks
    , testTask

    ) where

import Control.Monad.Reader (ask)
import Data.Acid (Query, Update, makeAcidic)
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
type Pos = NonEmpty Int

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

insertAt :: Int -> a -> [a] -> [a]
insertAt i a xs = before ++ a : after
  where (before, after) = splitAt i xs


-- Pure functions ------------------------------------------------------

insertTask'
    :: Pos   -- ^ Position at which to insert the task
    -> Task  -- ^ The new task object
    -> Tasks -> Tasks
insertTask' is_ x (Tasks forest_) = Tasks (go is_ forest_)
  where
    go is forest = case L.uncons is of
        (i, Nothing) -> insertAt i (Node x []) forest
        (i, Just is') -> case splitAt i forest of
            (before, node:after) -> before ++ [over branches (go is') node] ++ after
            _ -> forest ++ [Node x []]

modifyTask'
    :: Pos   -- ^ The position of the task
    -> Task  -- ^ The new task object
    -> Tasks -> Tasks
modifyTask' is_ x (Tasks forest_) = Tasks (go is_ forest_)
  where
    go is forest = case L.uncons is of
        (i, Nothing) -> case splitAt i forest of
            (before, node:after) -> before ++ [set root x node] ++ after
            _ -> fallback forest
        (i, Just is') -> case splitAt i forest of
            (before, node:after) -> before ++ [over branches (go is') node] ++ after
            _ -> fallback forest
    -- If the task tree has changed in the mean time, add the task to
    -- the end of the branch
    fallback = (++ [Node x []])

deleteTask'
    :: Pos  -- ^ Task to delete
    -> Tasks -> (Maybe Task, Tasks)
deleteTask' is_ (Tasks forest_) = over _2 Tasks $ go is_ forest_
  where
    go is forest = case L.uncons is of
        (i, Nothing) -> case splitAt i forest of
            (before, (Node task cs):after) -> (Just task, before ++ cs ++ after)
            _ -> (Nothing, forest)
        (i, Just is') -> case splitAt i forest of
            (before, node:after) ->
                let (deleted, children') = go is' (view branches node)
                in  (deleted, before ++ [set branches children' node] ++ after)
            _ -> (Nothing, forest)


-- Acidic functions ----------------------------------------------------

insertTask :: Pos -> Task -> Update Project ()
insertTask pos x = withTasks (insertTask' pos x)

modifyTask :: Pos -> Task -> Update Project ()
modifyTask pos x = withTasks (modifyTask' pos x)

deleteTask :: Pos -> Update Project (Maybe Task)
deleteTask pos = withTasks' (deleteTask' pos)

-- | Retrieve the current project.
queryProject :: Query Project Project
queryProject = ask

$(makeAcidic ''Project ['insertTask, 'modifyTask, 'deleteTask, 'queryProject])


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
