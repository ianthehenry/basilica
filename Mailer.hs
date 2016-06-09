module Mailer (
  Email(..),
  sendMail,
  easyEmail,
  Mailer,
  newMailer
) where

import           BasePrelude
import           Control.Lens ((?~))
import           Data.ByteString
import           Data.Text (Text)
import qualified Data.Text as Text
import           Network.Wreq
import           Network.HTTP.Client (HttpException)

data Mailer = Mailer { mailerKey :: ByteString }

newMailer :: ByteString -> Mailer
newMailer = Mailer

data Email = Email { emailTo :: Text
                   , emailFromName :: Text
                   , emailFromEmail :: Text
                   , emailReplyTo :: Text
                   , emailSubject :: Text
                   , emailBody :: Text
                   }

easyEmail :: Text -> Text -> Text -> Email
easyEmail to subject body =
  Email { emailTo = to
        , emailFromName = "Basilica"
        , emailFromEmail = "ianthehenry+basilica@gmail.com"
        , emailReplyTo = "ianthehenry+basilica@gmail.com"
        , emailSubject = subject
        , emailBody = body
        }

emailForm :: Email -> [FormParam]
emailForm Email{..} = [ "to" := emailTo
                      , "from" := Text.intercalate "" [emailFromName, " <", emailFromEmail, ">"]
                      , "h:Reply-To" := emailReplyTo
                      , "subject" := emailSubject
                      , "text" := emailBody
                      ]

sendMail :: Mailer -> Email -> IO ()
sendMail Mailer{mailerKey} email = catch (void sendEmail) logError
  where
    sendEmail = postWith opts "https://api.mailgun.net/v3/mail.basilica.horse/messages" form
    logError :: HttpException -> IO ()
    logError = print
    opts = defaults & auth ?~ basicAuth "api" mailerKey
    form = emailForm email
