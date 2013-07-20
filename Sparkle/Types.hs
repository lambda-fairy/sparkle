{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, TypeFamilies #-}

module Sparkle.Types
    (
      -- * Types
      Project(..)
    , emptyProject
    , Task(..)

      -- * Actions
    , QueryProject(..)

    ) where

import Control.Monad.Reader (ask)
--import Control.Monad.State (MonadState, get, put)
import Data.Acid (AcidState, Query, Update, makeAcidic)
import Data.Data (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text)

data Project = Project
    { projTitle :: Text
    , projTasks :: [Task]
    } deriving (Show, Data, Typeable)

data Task = Task
    { taskTitle    :: Text
    , taskDone     :: Bool
    , taskChildren :: [Task]
    } deriving (Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Task)
$(deriveSafeCopy 0 'base ''Project)

emptyProject :: Project
emptyProject = Project "Untitled Project" []

-------- Pure interface ------------------------------------------------

-------- Acidic functions ----------------------------------------------

-- | Retrieve the current project.
queryProject :: Query Project Project
queryProject = ask

$(makeAcidic ''Project ['queryProject])
