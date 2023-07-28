-- TODO(tech-debt): dedicate this module to representations and formatting
module ChaiMicroSql.CommonUtils (ToStringable(..)) where

class ToStringable a where
    toString :: a -> String
