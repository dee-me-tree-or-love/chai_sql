{-# LANGUAGE OverloadedStrings #-}

module LibSpec (spec) where

import qualified ChaiMicroSql.TAST       as TAST
import qualified ChaiMicroSql.TypeErrors as TE
import           Data.ByteString         (ByteString)
import qualified Lib                     as L
import qualified Test.Hspec              as THS

spec :: THS.Spec
spec = do
  THS.describe "type inference sample" $ do
    THS.describe "with valid context and SQL" $ do
      THS.it "returns successfull result" $ do
        let contextSrc = "tables: \n\
              \  - title: Cats \n\
              \    columns: \n\
              \      - name: age\n\
              \        spec: Number\n\
              \      - name: name\n\
              \        spec: Text\n\
              \      - name: isHomely\n\
              \        spec: Bool\n\
              \  - title: Friends \n\
              \    columns: \n\
              \      - name: firstFriendName\n\
              \        spec: Text\n\
              \      - name: secondFriendName\n\
              \        spec: Text\n\
              \      - name: bestFriends\n\
              \        spec: Bool" :: ByteString
        let sqlSrc = "SELECT name, firstFriendName, secondFriendName FROM Cats AS c, Friends AS c;" :: String

        case L.loadYamlContext contextSrc of
          Left _ -> error "should not be reachable"
          Right c -> do
            let sql = head $ L.parseSql sqlSrc
            let inferredType = L.inferSqlType c sql

            let n = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeText
            let fn = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "firstFriendName") TAST.TAstAtomicTypeText
            let sn = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "secondFriendName") TAST.TAstAtomicTypeText
            let t = [sn, fn, n] :: TAST.TAstDbView
            THS.shouldBe inferredType $ Right t

    THS.describe "with valid context and invalid SQL" $ do
      THS.it "returns successfull failed inference" $ do
        let contextSrc = "tables: \n\
              \  - title: Cats \n\
              \    columns: \n\
              \      - name: age\n\
              \        spec: Number\n\
              \      - name: name\n\
              \        spec: Text\n\
              \      - name: isHomely\n\
              \        spec: Bool\n\
              \  - title: Friends \n\
              \    columns: \n\
              \      - name: firstFriendName\n\
              \        spec: Text\n\
              \      - name: secondFriendName\n\
              \        spec: Text\n\
              \      - name: bestFriends\n\
              \        spec: Bool" :: ByteString
        let sqlSrc = "SELECT notAnAttribute1, notAnAttribute1 FROM NotCats AS c, NotFriends AS c;" :: String

        case L.loadYamlContext contextSrc of
          Left _ -> error "should not be reachable"
          Right c -> do
            let sql = head $ L.parseSql sqlSrc
            let inferredType = L.inferSqlType c sql

            let e1 = TE.makeError "Could not infer variable type. Variable `NotFriends` is not in context."
            let e2 = TE.makeError"Could not infer variable type. Variable `NotCats` is not in context."
            THS.shouldBe inferredType $ Left $ TE.joinErrors e1 e2

    THS.describe "with invalid context" $ do
      THS.it "returns an error before inference" $ do
        let contextSrc = "NotTables: \n\
              \  - NotTitle: Cats \n\
              \    columns: \n\
              \      - name: age\n\
              \        spec: NotNumber\n\
              \      - name: name\n\
              \        spec: NotText\n\
              \      - name: isHomely\n\
              \        spec: NotBool\n\
              \  - NotTitle: Friends \n\
              \    columns: \n\
              \      - name: firstFriendName\n\
              \        spec: NotText\n\
              \      - name: secondFriendName\n\
              \        spec: NotText\n\
              \      - name: bestFriends\n\
              \        spec: NotBool" :: ByteString

        let context =  L.loadYamlContext contextSrc
        let e = TE.makeError "Error occurred when decoding the scheme: AesonException \"Error in $: parsing ChaiMicroSql.Loaders.YamlContextLoader.DbSchema(DbSchema) failed, key \\\"tables\\\" not found\""
        THS.shouldBe context $ Left e
