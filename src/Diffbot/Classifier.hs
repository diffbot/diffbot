module Diffbot.Classifier where

import Data.Maybe

import Diffbot.Types

data Classifier = Classifier
    { classifierFields :: Maybe String
    , classifierMode   :: Maybe String
    , classifierStats  :: Bool
    }


instance Fields Classifier where
    fields        = classifierFields
    setFields f a = a { classifierFields = f }


instance Request Classifier where
    toReq a = Req { reqApi     = "http://api.diffbot.com/v2/analyze"
                  , reqContent = Nothing
                  , reqQuery   = mkClassifierQuery a
                  }


mkClassifierQuery :: Classifier -> [(String, Maybe String)]
mkClassifierQuery a = catMaybes [ fieldsQuery a
                                , mkQueryBool "stats" (classifierStats a)
                                , mkQuery     "mode"  (classifierMode a)
                                ]


defClassifier :: Classifier
defClassifier = Classifier { classifierFields = Nothing
                           , classifierMode   = Nothing
                           , classifierStats  = False
                           }
