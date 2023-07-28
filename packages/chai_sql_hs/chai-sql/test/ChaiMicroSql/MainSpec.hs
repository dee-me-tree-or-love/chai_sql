{-# LANGUAGE OverloadedStrings #-}

module ChaiMicroSql.MainSpec (spec) where

import qualified ChaiMicroSql.Main       as CSM
import qualified ChaiMicroSql.TAST       as TAST
import qualified ChaiMicroSql.TypeErrors as TE
import           Data.ByteString         (ByteString)
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

        case CSM.loadYamlContext contextSrc of
          Left _ -> error "should not be reachable"
          Right c -> do
            let sql = head $ CSM.parseSql sqlSrc
            let inferredType = CSM.inferSqlType c sql

            let n = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeText
            let fn = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "firstFriendName") TAST.TAstAtomicTypeText
            let sn = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "secondFriendName") TAST.TAstAtomicTypeText
            let t = [sn, fn, n] :: TAST.TAstDbView
            THS.shouldBe inferredType $ Right t

    THS.describe "with valid context and invalid SQL" $ do
      THS.it "returns failed inference" $ do
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

        case CSM.loadYamlContext contextSrc of
          Left _ -> error "should not be reachable"
          Right c -> do
            let sql = head $ CSM.parseSql sqlSrc
            let inferredType = CSM.inferSqlType c sql

            let e1 = TE.makeError "Could not infer variable type. Variable `NotFriends` is not in context."
            let e2 = TE.makeError "Could not infer variable type. Variable `NotCats` is not in context."
            THS.shouldBe inferredType $ Left $ TE.joinErrors e1 e2

    THS.describe "with valid context and mismatching SQL" $ do
      THS.it "returns failed inference" $ do
        let contextSrc = "tables: \n\
              \  - title: Cats \n\
              \    columns: \n\
              \      - name: age\n\
              \        spec: Number\n\
              \      - name: name\n\
              \        spec: Text\n\
              \      - name: isHomely\n\
              \        spec: Bool" :: ByteString
        let sqlSrc = "SELECT name as catName, name as dogName FROM Cats AS c, Dogs AS d;" :: String

        case CSM.loadYamlContext contextSrc of
          Left _ -> error "should not be reachable"
          Right c -> do
            let sql = head $ CSM.parseSql sqlSrc
            let inferredType = CSM.inferSqlType c sql

            let e1 = TE.makeError "Could not infer variable type. Variable `Dogs` is not in context."
            THS.shouldBe inferredType $ Left e1

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

        let context =  CSM.loadYamlContext contextSrc
        let e = TE.makeError "Error occurred when decoding the scheme: AesonException \"Error in $: parsing ChaiMicroSql.Loaders.YamlContextLoader.DbSchema(DbSchema) failed, key \\\"tables\\\" not found\""
        THS.shouldBe context $ Left e


  THS.describe "type checking sample" $ do
    THS.describe "with valid context and SQL type hint" $ do
      THS.it "returns no errors" $ do
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
        let sqlSrc = "--@cs :: {name: Text, firstFriendName: Text, secondFriendName: Text}\n\
          \SELECT name, firstFriendName, secondFriendName FROM Cats AS c, Friends AS c;" :: String

        case CSM.loadYamlContext contextSrc of
          Left _ -> error "should not be reachable"
          Right c -> do
            let sql = head $ CSM.parseSql sqlSrc
            let checkResult = CSM.checkSqlType c sql

            THS.shouldBe checkResult Nothing

    THS.describe "with valid context and wrong SQL type hint" $ do
      THS.it "returns mismatch error" $ do
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
        let sqlSrc = "--@cs :: {age: Number}\n\
          \SELECT name, firstFriendName, secondFriendName FROM Cats AS c, Friends AS c;" :: String

        case CSM.loadYamlContext contextSrc of
          Left _ -> error "should not be reachable"
          Right c -> do
            let sql = head $ CSM.parseSql sqlSrc
            let checkResult = CSM.checkSqlType c sql

            -- TODO(tech-debt): improve the checking error readability
            let e = TE.makeError "Specified type hint `[TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey \"age\") TAstAtomicTypeNumber]` does not match inferred type `[TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey \"secondFriendName\") TAstAtomicTypeText,TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey \"firstFriendName\") TAstAtomicTypeText,TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey \"name\") TAstAtomicTypeText]`"
            THS.shouldBe checkResult $ Just e

    --FIXME(bug): this is a captured bug
    THS.describe "with valid context and mismatching SQL but right type hint" $ do
      THS.it "returns failed inference" $ do
        let contextSrc = "tables: \n\
              \  - title: Cats \n\
              \    columns: \n\
              \      - name: age\n\
              \        spec: Number\n\
              \      - name: name\n\
              \        spec: Text\n\
              \      - name: isHomely\n\
              \        spec: Bool" :: ByteString
        let sqlSrc = "--@cs :: {catName: Text, dogName: Text}\n\
              \SELECT name as catName, name as dogName FROM Cats AS c, Dogs AS d;" :: String

        case CSM.loadYamlContext contextSrc of
          Left _ -> error "should not be reachable"
          Right c -> do
            let sql = head $ CSM.parseSql sqlSrc
            let checkResult = CSM.checkSqlType c sql

            let e1 = TE.makeError "Could not infer variable type. Variable `Dogs` is not in context."
            THS.shouldBe checkResult $ Just e1
