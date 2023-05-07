module ChaiMicroSql.CommonUtils (ToStringable(..)) where

class ToStringable a where
    toString :: a -> String
