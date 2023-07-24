{-# LANGUAGE OverloadedStrings #-}
module ChaiMicroSql.Loaders.YamlContextLoaderSpec (spec) where

import qualified ChaiMicroSql.Loaders.YamlContextLoader as YCL
import qualified ChaiMicroSql.TAST                      as TAST
import qualified ChaiMicroSql.TypeContext               as TCX
import qualified ChaiMicroSql.TypeErrors                as TE
import           Data.ByteString                        (ByteString)
import qualified Test.Hspec                             as THS

spec :: THS.Spec
spec = do
    THS.describe "YAML loader" $ do
        THS.describe "on valid schemas" $ do
            THS.describe "with all elements" $ do
                THS.it "returns correct context" $ do
                    let src = "tables: \n\
                            \  - title: Foo \n\
                            \    columns: \n\
                            \      - name: bar\n\
                            \        spec: Number\n\
                            \      - name: quack\n\
                            \        spec: Text\n\
                            \      - name: peep\n\
                            \        spec: Bool\n\
                            \  - title: Woo \n\
                            \    columns: \n\
                            \      - name: qar\n\
                            \        spec: Number\n\
                            \      - name: puack\n\
                            \        spec: Text\n\
                            \      - name: beep\n\
                            \        spec: Bool" :: ByteString
                    let tf = TAST.makeRecord [ TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey "bar") TAST.TAstAtomicTypeNumber
                                            , TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey "quack") TAST.TAstAtomicTypeText
                                            , TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey "peep") TAST.TAstAtomicTypeBool ]
                    let tw = TAST.makeRecord [ TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey "qar") TAST.TAstAtomicTypeNumber
                                            , TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey "puack") TAST.TAstAtomicTypeText
                                            , TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey "beep") TAST.TAstAtomicTypeBool ]
                    let ec = Right
                            $ TCX.extend (TCX.makeKey "Woo") (TCX.contextualize tw)
                            $ TCX.extend (TCX.makeKey "Foo") (TCX.contextualize tf)
                            TCX.freshContext
                    THS.shouldBe (YCL.load src) ec

            THS.describe "with no columns" $ do
                THS.it "returns correct context of empty records" $ do
                    let src = "tables: \n\
                            \  - title: Foo \n\
                            \    columns: []\n\
                            \  - title: Woo \n\
                            \    columns: []" :: ByteString
                    let tf = TAST.emptyRecord
                    let tw = TAST.emptyRecord
                    let ec = Right
                            $ TCX.extend (TCX.makeKey "Woo") (TCX.contextualize tw)
                            $ TCX.extend (TCX.makeKey "Foo") (TCX.contextualize tf)
                            TCX.freshContext
                    THS.shouldBe (YCL.load src) ec

            THS.describe "with no tables" $ do
                THS.it "returns correct empty context" $ do
                    let src = "tables: []" :: ByteString
                    let ec = Right TCX.freshContext
                    THS.shouldBe (YCL.load src) ec

        THS.describe "on not valid (non-parseable) schemas" $ do
            THS.describe "with non-supported column types" $ do
                THS.it "returns error" $ do
                    let src = "tables: \n\
                            \  - title: Foo \n\
                            \    columns: \n\
                            \      - name: bar\n\
                            \        spec: MaliciousNotNumber\n\
                            \      - name: quack\n\
                            \        spec: MaliciousNotText\n\
                            \      - name: peep\n\
                            \        spec: MaliciousNotBool" :: ByteString
                    let ec = Left $ TE.makeError "Error occurred when decoding the scheme: AesonException \"Error in $.tables[0].columns[0].spec: Provided `spec` value is not one of supported types: Number | Bool | Text\""
                    THS.shouldBe (YCL.load src) ec

            THS.describe "with non-existent column types" $ do
                THS.it "returns error" $ do
                    let src = "tablOOOOOOs: \n\
                            \  - title: Foo \n\
                            \    columns: \n\
                            \      - name: bar\n\
                            \        spec: Number" :: ByteString
                    let ec = Left $ TE.makeError "Error occurred when decoding the scheme: AesonException \"Error in $: parsing ChaiMicroSql.Loaders.YamlContextLoader.DbSchema(DbSchema) failed, key \\\"tables\\\" not found\""
                    THS.shouldBe (YCL.load src) ec

