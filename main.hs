{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Monad
import           Data.Text
import           Data.Text.Encoding
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Wai
import           System.Directory
import           Yesod hiding (runDB)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    token Text
    UniqueToken token
    deriving Show
|]

data TokenApp = TokenApp

mkYesod "TokenApp" [parseRoutes|
/ HomeR GET
|]

instance Yesod TokenApp where
    isAuthorized _ _ = do
        mUser <- maybeCurrentUser
        return $ case mUser of
            Just _ -> Authorized
            Nothing -> Unauthorized "Please provide a token"

maybeCurrentUser = do
    req <- waiRequest
    case lookup "token-header" (requestHeaders req) of
        Nothing -> return Nothing
        Just token -> runDB . getBy . UniqueToken $ decodeUtf8 token

dbFile = "poc.sqlite"

runDB = runSqlite dbFile

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Token authentication POC!|]

main :: IO ()
main = do
    exists <- doesFileExist dbFile
    when exists (removeFile dbFile)
    runDB $ do
        runMigration migrateAll

        insert $ User "secure_token"
    warp 3000 TokenApp
