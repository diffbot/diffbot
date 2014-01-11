module Classifier where

import Types

data Classifier = Classifier
    { classifierToken  :: String
    , classifierUrl    :: String
    , classifierFields :: Maybe String
    , classifierMode   :: Maybe String
    , classifierStats  :: Bool
    }


instance Fields Classifier where
    fields        = classifierFields
    setFields f a = a { classifierFields = f }


instance Request Classifier where
    toReq a = Req { reqApi     = "http://api.diffbot.com/v2/analyze"
                  , reqToken   = classifierToken a
                  , reqUrl     = classifierUrl a
                  , reqContent = Nothing
                  , reqQuery   = fieldsQuery a ++ mkClassifierQuery a
                  }

mkClassifierQuery :: Classifier -> [(String, Maybe String)]
mkClassifierQuery a = statsQuery ++ modeQuery
  where
    statsQuery = if classifierStats a then [("stats", Nothing)] else []
    modeQuery  = mkQuery "mode" $ classifierMode a


mkClassifier :: String -> String -> Classifier
mkClassifier token url = Classifier { classifierToken  = token
                                    , classifierUrl    = url
                                    , classifierFields = Nothing
                                    , classifierMode   = Nothing
                                    , classifierStats  = False
                                    }
