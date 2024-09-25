-- | Type-safe implementation for requesting 'AccessToken' permissions
--
-- <https://docs.github.com/en/rest/apps/apps?apiVersion=2022-11-28#create-an-installation-access-token-for-an-app>
--
-- Usage:
--
-- 1. Use a constructor function for a specific permission, each of which only
--    accepts appropriate values (e.g. you can't ask for @admin@ on a permission
--    that only allows @read@ or @write@).
-- 2. Combine these using 'Permissions' 'Semigroup' instance.
--
-- For example:
--
-- @
-- let permissions = 'actions' 'Read' <> 'checks' 'Write'
--
-- 'generateInstallationTokenScoped' AllRepositories permissions creds installationId
-- @
--
-- Supplying the same permission more than once will take the higher:
--
-- @
-- 'checks' 'Read' <> 'checks' 'Write' == 'checks' 'Write'
-- @
module GitHub.App.Token.Permissions
  ( Permissions
  , Read (..)
  , Write (..)
  , Admin (..)
  , actions
  , administration
  , checks
  , codespaces
  , contents
  , dependabot_secrets
  , deployments
  , environments
  , issues
  , metadata
  , packages
  , pages
  , pull_requests
  , repository_custom_properties
  , repository_hooks
  , repository_projects
  , secret_scanning_alerts
  , secrets
  , security_events
  , single_file
  , statuses
  , vulnerability_alerts
  , workflows
  , members
  , organization_administration
  , organization_custom_roles
  , organization_custom_org_roles
  , organization_custom_properties
  , organization_copilot_seat_management
  , organization_announcement_banners
  , organization_events
  , organization_hooks
  , organization_personal_access_tokens
  , organization_personal_access_token_requests
  , organization_plan
  , organization_projects
  , organization_packages
  , organization_secrets
  , organization_self_hosted_runners
  , organization_user_blocking
  , team_discussions
  , email_addresses
  , followers
  , git_ssh_keys
  , gpg_keys
  , interaction_limits
  , profile
  , starring
  ) where

import GitHub.App.Token.Prelude hiding (Read)

import Data.Aeson (ToJSON (..))
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

newtype Permissions = Permissions
  { _unwrap :: MonoidalMap Text Permission
  }
  deriving stock (Eq)
  deriving newtype (Semigroup, Monoid, ToJSON)

data Permission
  = PermissionRead
  | PermissionWrite
  | PermissionAdmin
  deriving stock (Eq)

instance Semigroup Permission where
  _ <> PermissionAdmin = PermissionAdmin
  PermissionAdmin <> _ = PermissionAdmin
  _ <> PermissionWrite = PermissionWrite
  PermissionWrite <> _ = PermissionWrite
  PermissionRead <> PermissionRead = PermissionRead

instance ToJSON Permission where
  toJSON =
    toJSON @Text . \case
      PermissionRead -> "read"
      PermissionWrite -> "write"
      PermissionAdmin -> "admin"

class AsReadWrite a where
  toReadWritePermission :: a -> Permission

class AsReadWriteAdmin a where
  toReadWriteAdminPermission :: a -> Permission

data Read = Read

instance AsReadWrite Read where
  toReadWritePermission _ = PermissionRead

instance AsReadWriteAdmin Read where
  toReadWriteAdminPermission _ = PermissionRead

data Write = Write

instance AsReadWrite Write where
  toReadWritePermission _ = PermissionWrite

instance AsReadWriteAdmin Write where
  toReadWriteAdminPermission _ = PermissionWrite

data Admin = Admin

instance AsReadWriteAdmin Admin where
  toReadWriteAdminPermission _ = PermissionAdmin

actions :: AsReadWrite a => a -> Permissions
actions = mkReadWrite "actions"

administration :: AsReadWrite a => a -> Permissions
administration = mkReadWrite "administration"

checks :: AsReadWrite a => a -> Permissions
checks = mkReadWrite "checks"

codespaces :: AsReadWrite a => a -> Permissions
codespaces = mkReadWrite "codespaces"

contents :: AsReadWrite a => a -> Permissions
contents = mkReadWrite "contents"

dependabot_secrets :: AsReadWrite a => a -> Permissions
dependabot_secrets = mkReadWrite "dependabot_secrets"

deployments :: AsReadWrite a => a -> Permissions
deployments = mkReadWrite "deployments"

environments :: AsReadWrite a => a -> Permissions
environments = mkReadWrite "environments"

issues :: AsReadWrite a => a -> Permissions
issues = mkReadWrite "issues"

metadata :: AsReadWrite a => a -> Permissions
metadata = mkReadWrite "metadata"

packages :: AsReadWrite a => a -> Permissions
packages = mkReadWrite "packages"

pages :: AsReadWrite a => a -> Permissions
pages = mkReadWrite "pages"

pull_requests :: AsReadWrite a => a -> Permissions
pull_requests = mkReadWrite "pull_requests"

repository_custom_properties :: AsReadWrite a => a -> Permissions
repository_custom_properties = mkReadWrite "repository_custom_properties"

repository_hooks :: AsReadWrite a => a -> Permissions
repository_hooks = mkReadWrite "repository_hooks"

repository_projects :: AsReadWriteAdmin a => a -> Permissions
repository_projects = mkReadWriteAdmin "repository_projects"

secret_scanning_alerts :: AsReadWrite p => p -> Permissions
secret_scanning_alerts = mkReadWrite "secret_scanning_alerts"

secrets :: AsReadWrite p => p -> Permissions
secrets = mkReadWrite "secrets"

security_events :: AsReadWrite p => p -> Permissions
security_events = mkReadWrite "security_events"

single_file :: AsReadWrite p => p -> Permissions
single_file = mkReadWrite "single_file"

statuses :: AsReadWrite p => p -> Permissions
statuses = mkReadWrite "statuses"

vulnerability_alerts :: AsReadWrite p => p -> Permissions
vulnerability_alerts = mkReadWrite "vulnerability_alerts"

-- | Only supported permission is 'Write'
workflows :: Permissions
workflows = mkWrite "workflows"

members :: AsReadWrite p => p -> Permissions
members = mkReadWrite "members"

organization_administration :: AsReadWrite p => p -> Permissions
organization_administration = mkReadWrite "organization_administration"

organization_custom_roles :: AsReadWrite p => p -> Permissions
organization_custom_roles = mkReadWrite "organization_custom_roles"

organization_custom_org_roles :: AsReadWrite p => p -> Permissions
organization_custom_org_roles = mkReadWrite "organization_custom_org_roles"

organization_custom_properties :: AsReadWriteAdmin p => p -> Permissions
organization_custom_properties = mkReadWriteAdmin "organization_custom_properties"

-- | Only supported permission is 'Write'
organization_copilot_seat_management :: Permissions
organization_copilot_seat_management = mkWrite "organization_copilot_seat_management"

organization_announcement_banners :: AsReadWrite p => p -> Permissions
organization_announcement_banners = mkReadWrite "organization_announcement_banners"

-- | Only supported permission is 'Read'
organization_events :: Permissions
organization_events = mkRead "organization_events"

organization_hooks :: AsReadWrite p => p -> Permissions
organization_hooks = mkReadWrite "organization_hooks"

organization_personal_access_tokens :: AsReadWrite p => p -> Permissions
organization_personal_access_tokens = mkReadWrite "organization_personal_access_tokens"

organization_personal_access_token_requests :: AsReadWrite p => p -> Permissions
organization_personal_access_token_requests = mkReadWrite "organization_personal_access_token_requests"

-- | Only supported permission is 'Read'
organization_plan :: Permissions
organization_plan = mkRead "organization_plan"

organization_projects :: AsReadWriteAdmin p => p -> Permissions
organization_projects = mkReadWriteAdmin "organization_projects"

organization_packages :: AsReadWrite p => p -> Permissions
organization_packages = mkReadWrite "organization_packages"

organization_secrets :: AsReadWrite p => p -> Permissions
organization_secrets = mkReadWrite "organization_secrets"

organization_self_hosted_runners :: AsReadWrite p => p -> Permissions
organization_self_hosted_runners = mkReadWrite "organization_self_hosted_runners"

organization_user_blocking :: AsReadWrite p => p -> Permissions
organization_user_blocking = mkReadWrite "organization_user_blocking"

team_discussions :: AsReadWrite p => p -> Permissions
team_discussions = mkReadWrite "team_discussions"

email_addresses :: AsReadWrite p => p -> Permissions
email_addresses = mkReadWrite "email_addresses"

followers :: AsReadWrite p => p -> Permissions
followers = mkReadWrite "followers"

git_ssh_keys :: AsReadWrite p => p -> Permissions
git_ssh_keys = mkReadWrite "git_ssh_keys"

gpg_keys :: AsReadWrite p => p -> Permissions
gpg_keys = mkReadWrite "gpg_keys"

interaction_limits :: AsReadWrite p => p -> Permissions
interaction_limits = mkReadWrite "interaction_limits"

-- | Only supported permission is 'Write'
profile :: Permissions
profile = mkWrite "profile"

starring :: AsReadWrite p => p -> Permissions
starring = mkReadWrite "starring"

mkRead :: Text -> Permissions
mkRead name = Permissions $ MonoidalMap.singleton name PermissionRead

mkWrite :: Text -> Permissions
mkWrite name = Permissions $ MonoidalMap.singleton name PermissionWrite

mkReadWrite :: AsReadWrite p => Text -> p -> Permissions
mkReadWrite name =
  Permissions
    . MonoidalMap.singleton name
    . toReadWritePermission

mkReadWriteAdmin :: AsReadWriteAdmin p => Text -> p -> Permissions
mkReadWriteAdmin name =
  Permissions
    . MonoidalMap.singleton name
    . toReadWriteAdminPermission
