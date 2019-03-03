{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns, NoImplicitPrelude, LambdaCase #-}
import Protolude
import Data.Fix
import Data.List (lookup)
import Data.String
import Nix.Expr
import qualified Nix.Pretty
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP

actions :: [(Text, ([Text] -> NExpr))]
actions =
  [ ("haskellEnv", haskellEnv)
  , ("testExpr", const testExpr) ]

main :: IO ()
main = do
  map toS <$> getArgs >>= \case
    [] -> print errmsg
    (action:args)  -> case lookup action actions of
      Just exprGen -> printNix $ exprGen args
      Nothing      -> PP.putDoc $ "action " <> PP.pretty action <> " doesn’t exist, " <> errmsg
  putStr ("\n" :: Text)
  where
    ppList = PP.list . map PP.pretty :: [Text] -> PP.Doc ann
    printNix expr = PP.putDoc $ Nix.Pretty.prettyNix expr
    errmsg = "please specify one of " <> ppList (map fst actions)


fetchFromGitHub :: Text -> Text -> Text -> Text -> NExpr
fetchFromGitHub owner repo rev sha = "fetchFromGitHub" @@ mkNonRecSet
    [ "owner"  $$= owner
    , "repo"   $$= repo
    , "rev"    $$= rev
    , "sha256" $$= sha ]
  where k $$= v = k $= (mkStr v)

haskellEnv :: [Text] -> NExpr
haskellEnv pkgs = ((importNixpkgs @. "haskellPackages")
                     `haskellOverrides` [ myPkg ])
                    @. "my-pkg" @. "env"
  where
    myPkg = "my-pkg" $=
      mkLets [ "buildDepends" $= mkWith "self" (mkList $ map mkSym pkgs) ]
             ("super" @. "mkDerivation" @@ mkNonRecSet
               [ "pname"   $= mkStr "pkg-env"
               , "src"     $= mkStr "/dev/null"
               , "version" $= mkStr "none"
               , "license" $= mkStr "none"
               , inherit [ StaticKey "buildDepends" ] nullPos
               , "buildTools" $= mkWith "self" (mkList
                   [ "ghcid"
                   , "cabal-install"
                   , "hpack"
                   -- TODO: somehow add hscolour to a haskell env
                   , "hscolour"
                   , "hoogleLocal"
                       @@ mkNonRecSet [ "packages" $= "buildDepends" ]
                   ])
               ])

    haskellOverrides :: NExpr -> [Binding NExpr] -> NExpr
    haskellOverrides haskPkgs hpDefs = haskPkgs @. "override"
      @@ mkNonRecSet [
           "overrides" $= (Param "self" ==> Param "super" ==> mkNonRecSet hpDefs)
         ]
    importNixpkgs = ("import" @@ mkEnvPath "nixpkgs" @@ mkNonRecSet [])

testExpr :: NExpr
testExpr =
  let params ps = mkParamset ps False in
  mkFunction (params $ map ( , Nothing)
                      [ "lib", "buildPythonApplication", "pythonPackages",
                        "fetchFromGitHub" ])
             ("buildPythonApplication" @@ drvAttr)
  where
    version = "3.9"
    drvAttr = mkRecSet
      [ "name"    $= mkStrA [ "rss2email-", A "version" ]
      , "version" $= mkStr "3.9"
      , "src"     $= fetchFromGitHub "wking" "rss2email" ("v"<>version)
                       "b5888c4bdcd296582ed29dd2392eaf12647db9d53fcf91e4728e27a1c05b65e9"
      ]

-- | String quotation, either a plain string (S) or antiquoted (A)
-- TODO: also copied to yarn2nix, ask jwiegley
data StrQ = S !Text | A !NExpr
instance IsString StrQ where
  fromString = S . fromString

mkStrA :: [StrQ] -> NExpr
mkStrA = Fix . NStr . DoubleQuoted . map f
  where f (S t) = Plain t
        f (A r) = Antiquoted r
