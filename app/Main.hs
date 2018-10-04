{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
module Main where

import           Control.Monad.IO.Class
import           Data.Text.Lazy                as T
                                                ( Text
                                                , pack
                                                , toStrict
                                                )
import           Data.Text.Lazy.Read
import qualified Data.Text                     as S
import           Data.Int
import           Data.Function                  ( (&) )
import qualified Data.ByteString.Char8         as C8
import           Web.Scotty                    as H
import           System.Environment
import           System.Exit
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , lazy
                                                )
import           Data.Aeson                     ( (.=)
                                                , object
                                                )
import           Data.Aeson.Lens                ( key
                                                , _String
                                                )
import           Network.Wreq                  as W
                                                ( postWith
                                                , defaults
                                                , header
                                                , responseBody
                                                )
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.Cors
import           Network.HTTP.Types.Status
import           Data.Monoid                    ( (<>) )
import           Hasql.Statement                ( Statement(..) )
import qualified Hasql.Session                 as Session
import qualified Hasql.Connection              as Conn
import qualified Hasql.Decoders                as Decoders
import qualified Hasql.Encoders                as Encoders

main :: IO ()
main = do
  port       <- fmap T.pack $ getEnv "PORT"
  connString <- fmap C8.pack $ getEnv "DATABASE_URL"
  mdb        <- Conn.acquire connString
  case mdb of
    Right db -> do
      scotty (parsePort port) $ do
        middleware logStdoutDev
        middleware simpleCors
        middleware $ staticPolicy (addBase "static")
        server db
    Left merr -> case merr of
      Just err -> die $ C8.unpack err
      Nothing  -> die "failed to open postgres connection"

server :: Conn.Connection -> ScottyM ()
server db = do
  H.get "/" $ do
    setHeader "Content-Type" "text/html"
    file "static/index.html"

  H.post "/donate" $ do
    mreferrer <- H.header "Referer"
    email     <- H.param "email"
    -- note <- H.param "note" `rescue` (\_ -> "")
    amt       <- fmap decimal $ H.param "amount"
    case mreferrer of
      Nothing -> do
        status status400
        html "<h1>error</h1><p>didn't find the Referer header.</p>"
      Just referrer -> do
        case amt of
          Left err -> do
            status status400
            html $ "<h1>error</h1><p>wrong amount: " <> (T.pack err) <> "</p>"
          Right (amount, _) -> do
            inv <- liftIO $ fetchInvoice referrer email amount
            liftIO $ print inv
            html inv

  H.get "/balance/:domain" $ do
    domain <- fmap toStrict $ param "domain"
    balr   <- liftIO $ Session.run (Session.statement domain getBalance) db
    case balr of
      Left err -> do
        status status500
        html $ "<h1>error</h1>" <> (err & show & T.pack)
      Right balance -> html $ "<h1>balance</h1>" <> (balance & show & T.pack)

fetchInvoice :: Text -> Text -> Int -> IO Text
fetchInvoice domain email amount = do
  let opts =
        W.defaults
          &  W.header "Content-Type"
          .~ ["application/json"]
          &  W.header "Authorization"
          .~ ["7db0ff61-98a1-4bef-89db-2525c7813f6e"]
  let body =
        object ["description" .= domain, "email" .= email, "amount" .= amount]
  r <- W.postWith opts "https://dev-api.opennode.co/v1/charges" body

  pure
    $  r
    ^. responseBody
    .  key "data"
    .  key "lightning_invoice"
    .  key "payreq"
    .  _String
    .  lazy

getBalance :: Statement S.Text Int64
getBalance = Statement sql encoder decoder True
 where
  sql     = "select coalesce(sum(amt), 0) from donations where domain = $1"
  encoder = Encoders.param Encoders.text
  decoder = Decoders.singleRow (Decoders.column Decoders.int8)

parsePort :: Text -> Int
parsePort port = case decimal port of
  Left  _      -> 3000
  Right (n, _) -> n
