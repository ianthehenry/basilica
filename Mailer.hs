module Mailer (
  Email(..),
  sendMail,
  easyEmail,
  Mailer,
  newMailer
) where

import           BasePrelude
import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           Network.Wreq

data Mailer = Mailer { mailerKey :: Text }

newMailer :: Text -> Mailer
newMailer key = Mailer { mailerKey = key }

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

instance Aeson.ToJSON Email where
  toJSON Email{..} = Aeson.object [ "auto_html" .= False
                                  , "to" .= [Aeson.object ["email" .= emailTo]]
                                  , "from_name" .= emailFromName
                                  , "from_email" .= emailFromEmail
                                  , "headers" .= Aeson.object ["Reply-To" .= emailReplyTo]
                                  , "subject" .= emailSubject
                                  , "text" .= emailBody
                                  ]

sendMail :: Mailer -> Email -> IO ()
sendMail Mailer{mailerKey} email = do
  post "https://mandrillapp.com/api/1.0/messages/send" (Aeson.toJSON body)
  return ()
  where
    body = Aeson.object [ "message" .= email
                        , "key" .= mailerKey
                        ]
