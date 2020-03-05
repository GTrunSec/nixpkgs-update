module DoctestSpec where

import Test.DocTest
import Test.Hspec

spec :: Spec
spec = do
  describe "Doctests" do
    it "should all pass" do
      doctest
        [ "-isrc",
          "-XOverloadedStrings",
          "-XDataKinds",
          "-XFlexibleContexts",
          "-XGADTs",
          "-XLambdaCase",
          "-XPolyKinds",
          "-XRankNTypes",
          "-XScopedTypeVariables",
          "-XTypeApplications",
          "-XTypeFamilies",
          "-XTypeOperators",
          "src/Version.hs",
          "src/GH.hs",
          "src/Time.hs"
        ]
