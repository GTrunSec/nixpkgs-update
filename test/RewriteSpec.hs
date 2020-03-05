{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module RewriteSpec where

import Text.RawString.QQ (r)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import OurPrelude
import qualified Rewrite
import System.Posix.Temp (mkdtemp)
import Test.Hspec
import qualified Utils

spec :: Spec
spec = do
  describe "Rewrite.quotedUrls" do
    it "quotes an unquoted meta.homepage URL" do
      d <- mkdtemp "nixpkgs-update-rewrite-spec"
      let filePath = d <> "/no_quoted_homepage.nix"
      T.writeFile filePath noQuotedHompageNix
      putStrLn filePath
      let options = Utils.Options False "" False False
      let updateEnv = Utils.UpdateEnv "inadyn" "2.5" "2.6" Nothing options
      let rwArgs = Rewrite.Args updateEnv "inadyn" filePath noQuotedHompageNix
      result <- runExceptT $ Rewrite.quotedUrls (\msg -> T.putStrLn msg) rwArgs
      result `shouldBe` Right (Just "blah")
      newContents <- T.readFile filePath
      newContents `shouldSatisfy` ("\"http://troglobit.com/project/inadyn/\"" `T.isInfixOf`)


noQuotedHompageNix :: Text
noQuotedHompageNix = [r|
{ stdenv, fetchFromGitHub, autoreconfHook, pkgconfig
, gnutls, libite, libconfuse }:

stdenv.mkDerivation rec {
  pname = "inadyn";
  version = "2.6";

  src = fetchFromGitHub {
    owner = "troglobit";
    repo = "inadyn";
    rev = "v${version}";
    sha256 = "013kxlglxliajv3lrsix4w88w40g709rvycajb6ad6gbh8giqv47";
  };

  nativeBuildInputs = [ autoreconfHook pkgconfig ];

  buildInputs = [ gnutls libite libconfuse ];

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    homepage = http://troglobit.com/project/inadyn/;
    description = "Free dynamic DNS client";
    license = licenses.gpl2Plus;
    maintainers = with maintainers; [ ];
    platforms = platforms.linux;
  };
}
|]
