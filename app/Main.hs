{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
module Main where

import           Control.Monad.IO.Class
import           Data.Text.Lazy                as T
                                                ( Text
                                                , pack
                                                )
import           Data.Text.Lazy.Read
import           Data.Text.Lazy.IO             as T
                                                ( putStrLn )
import           Data.Int
import           Data.Function                  ( (&) )
import           Web.Scotty
import           System.Environment
import           System.Exit
import qualified Data.ByteString.Char8         as C8
import           Network.Wai.Middleware.RequestLogger
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
  connString <- getEnv "DATABASE_URL"
  mdb        <- Conn.acquire $ connString & C8.pack
  case mdb of
    Right db -> do
      T.putStrLn $ "Starting server on port " <> port <> "..."
      scotty (parsePort port) $ do
        middleware logStdoutDev
        middleware simpleCors
        server db
    Left merr -> case merr of
      Just err -> die $ C8.unpack err
      Nothing  -> die "failed to open postgres connection"

server :: Conn.Connection -> ScottyM ()
server db = do
  get "/" $ do
    html "<h1>hello</h1>"

  get "/balance" $ do
    balr <- liftIO $ Session.run (Session.statement () getBalance) db
    case balr of
      Left err -> do
        status status500
        html $ "<h1>error</h1>" <> (err & show & T.pack)
      Right balance -> html $ "<h1>balance</h1>" <> (balance & show & T.pack)

getBalance :: Statement () Int64
getBalance = Statement sql encoder decoder True
 where
  sql     = "select coalesce(sum(amt), 0) from donations"
  encoder = Encoders.unit
  decoder = Decoders.singleRow (Decoders.column Decoders.int8)

parsePort :: Text -> Int
parsePort port = case decimal port of
  Left  _      -> 3000
  Right (n, _) -> n
