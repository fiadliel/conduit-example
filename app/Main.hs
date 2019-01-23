{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Resource
import Control.Lens
import Data.Conduit 
import Data.Binary.Builder
import Network.AWS
import Network.AWS.S3
import Network.AWS.Data.Body

import Network.Wai
import Network.Wai.Conduit (responseSource)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import System.IO

bodyToConduit :: RsBody -> ConduitT () (Flush Builder) (ResourceT IO) ()
bodyToConduit (RsBody body) = mapOutput (Chunk . fromByteString) body

responseToWai :: GetObjectResponse -> IO Network.Wai.Response
responseToWai objResponse = 
    responseSource status200 [] $ bodyToConduit (objResponse ^. gorsBody)

doRequest :: Env -> BucketName -> ObjectKey -> IO Network.Wai.Response
doRequest env bucket object = do
    object <- runResourceT $ runAWS env $ send (getObject bucket object)
    responseToWai object

main = print "amazonka to conduit"