{-# LANGUAGE CPP #-}
module Mail.Hailgun.SendEmail
    ( sendEmail
    , HailgunSendResponse(..)
    ) where

#if __GLASGOW_HASKELL__ < 800
import           Control.Applicative
#endif
import           Control.Monad                         (mzero)
import           Data.Aeson
import qualified Data.Aeson.Key                        as Key
import qualified Data.ByteString.Char8                 as BC
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import           Mail.Hailgun.Communication
import           Mail.Hailgun.Errors
import           Mail.Hailgun.Internal.Data
import           Mail.Hailgun.MailgunApi
import           Mail.Hailgun.PartUtil
import qualified Network.HTTP.Client                   as NC
import qualified Network.HTTP.Client.MultipartFormData as NCM
import           Network.HTTP.Client.TLS               (tlsManagerSettings)

-- | Send an email using the Mailgun API's. This method is capable of sending a message over the
-- Mailgun service. All it needs is the appropriate context.
sendEmail
   :: HailgunContext -- ^ The Mailgun context to operate in.
   -> HailgunMessage -- ^ The Hailgun message to be sent.
   -> IO (Either HailgunErrorResponse HailgunSendResponse) -- ^ The result of the sent email. Either a sent email or a successful send.
sendEmail context message = do
   request <- postRequest url context (toEmailParts message)
#if MIN_VERSION_http_client(0,5,0)
   mgr <- NC.newManager tlsManagerSettings
   response <- NC.httpLbs request mgr
#else
   response <- NC.withManager tlsManagerSettings (NC.httpLbs request)
#endif
   return $ parseResponse response
   where
      url = mailgunApiPrefixContext context ++ "/messages"

toEmailParts :: HailgunMessage -> [NCM.Part]
toEmailParts message = params ++ attachmentParts
   where
      params = map paramsToPart . toSimpleEmailParts $ message
      attachmentParts = map attachmentToPart . messageAttachments $ message

toSimpleEmailParts :: HailgunMessage -> [(BC.ByteString, BC.ByteString)]
toSimpleEmailParts message =
   [ (BC.pack "from", messageFrom message)
   , (BC.pack "subject", T.encodeUtf8 $ messageSubject message)
   ] ++ to
   ++ cc
   ++ bcc
   ++ fromContent (messageContent message)
   where
      to = convertEmails (BC.pack "to") . messageTo $ message
      cc = convertEmails (BC.pack "cc") . messageCC $ message
      bcc = convertEmails (BC.pack "bcc") . messageBCC $ message

      fromContent :: MessageContent -> [(BC.ByteString, BC.ByteString)]
      fromContent t@(TextOnly _) = [ (BC.pack "text", textContent t) ]
      fromContent th@(TextAndHTML {}) = (BC.pack "html", htmlContent th) : fromContent (TextOnly . textContent $ th)

      convertEmails :: BC.ByteString -> [VerifiedEmailAddress] -> [(BC.ByteString, BC.ByteString)]
      convertEmails prefix = fmap ((,) prefix)

-- TODO replace with MailgunSendResponse
-- | The response to an email being accepted by the Mailgun API.
data HailgunSendResponse = HailgunSendResponse
   { hsrMessage :: String -- ^ The freeform message from the mailgun API.
   , hsrId      :: String -- ^ The ID of the message that has been accepted by the Mailgun api.
   }
   deriving (Show)

instance FromJSON HailgunSendResponse where
   parseJSON (Object v) = HailgunSendResponse
      <$> v .: Key.fromText (T.pack "message")
      <*> v .: Key.fromText (T.pack "id")
   parseJSON _ = mzero

