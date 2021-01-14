module Main where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (decodeJson)
import Data.Array (filter, sortWith)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Set (member)
import Data.Set as Set
import Data.String (Pattern(..), stripPrefix, stripSuffix)
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log, logShow)

type Org = {name :: String, ignored :: Array String}
type Repo = {name :: String, html_url :: String}
type Repos = Array Repo

main :: Effect Unit
main = launchAff_ do
  traverse processOrg orgs

-- If there's an error, return empty array
getOrgRepos :: String -> Aff Repos
getOrgRepos org = do
  let
    url = "https://api.github.com/orgs/" <> org <> "/repos"
  errorOrResponse <- AX.get AXRF.json url
  case errorOrResponse of
    Left httpError -> do
      log $ "Failed to get " <> url <> " : " <> printError httpError
      pure []
    Right {body} -> case decodeJson body of
      Right (repos :: Repos) -> pure repos
      Left decodeError -> do
        logShow decodeError
        pure []

shortenName :: String -> String
shortenName = removePrefix <<< removeSuffix
  where
    removePrefix n = fromMaybe n $ stripPrefix (Pattern "purescript-") n
    removeSuffix n = fromMaybe n $ stripSuffix (Pattern "-purescript") n

shortenRepoName :: Repo -> Repo
shortenRepoName repo = repo {name = shortenName repo.name}

renderRepo :: Repo -> String
renderRepo {html_url, name} =
  "\"=HYPERLINK(\"\"" <> html_url <> "\"\";\"\"" <> name <> "\"\")\""

processOrg :: Org -> Aff Unit
processOrg {name, ignored} = do
  log $ shortenName name
  reposLong <- getOrgRepos name
  let
    ignoredSet = Set.fromFoldable ignored
    repos = sortWith _.name
      $ filter (\r -> not $ member r.name ignoredSet)
      $ map shortenRepoName reposLong
  traverse_ (log <<< renderRepo) repos
  log ""

orgs :: Array Org
orgs =
  [ { name: "purescript"
    , ignored:
      [ "atom-language"
      , "documentation"
      , "governance"
      , "governance"
      , "gsoc"
      , "in"
      , "infrastructure"
      , "logo"
      , "npm-installer"
      , "package-sets"
      , "psc-package"
      , "purescript"
      , "purescript.github.io"
      , "pursuit"
      , "pursuit-backups"
      , "registry"
      , "roadmap"
      , "spago"
      , "trypurscript"
      ]
    }
  , { name: "purescript-contrib"
    , ignored:
      [ "atom-language"
      , "governance"
      , "pulp"
      , "vim"
      ]
    }
  , { name: "purescript-web"
    , ignored:
      [ "web-performance"
      ]
    }
  , { name: "purescript-node"
    , ignored: []
    }
]
