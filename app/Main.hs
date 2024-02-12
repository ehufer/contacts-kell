{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty ( post, get, html, param, scotty, redirect )

import Lucid
-- import Lucid.Base ( renderText, Html, ToHtml(toHtml) )
import Data.Text as T
-- import Lucid.Html5 (div_)
import Data.IORef
import Data.Map
import qualified Data.Map as M
import Control.Monad.IO.Class

data Contact = Detail { idContact :: Int, firstName :: Text, lastName :: Text, phoneNumber :: Text, email :: Text, addressPrincipal:: Address}
        deriving (Show)

data Address = Address { street :: Text, zipCode :: Text}
        deriving (Show)

type Contacts = Map Int Contact 

type Db = IORef Contacts

--Model

-- address = Address { street = T.concat ["hello"], zipCode =  }
-- contact = Detail {idContact = 1, firstName =  "h", lastName = "Garcia", phoneNumber = "123", email = "haha", addressPrincipal = address }

-- contacts = mempty :: Contacts

addContact:: Db -> Contact -> IO ()
addContact db c =  modifyIORef db $ M.insert (idContact c) c
                    
-- getConctact:: Db -> Int -> IO Maybe Contact

-- getContacts:: Db -> IO [Contact]
-- getContacts db  = do (i, contacts) <- liftIO $ readIORef db
--                      return (M.toList contacts)


-- updateContact:: Db -> Contact -> IO ()

--View

viewContact :: Contact -> Html ()
viewContact c  = div_ $ do
                    h1_ (toHtml name)
                    p_ (toHtml number)
                where
                    name = T.concat [firstName c, " ", lastName c]
                    number = T.concat ["number: ", phoneNumber c]

labelWInput_ :: Text -> Html ()
labelWInput_ name = label_ $ do 
                    span_ (toHtml name) 
                    input_ [type_ "text", name_ name]

addContactForm :: Html ()
addContactForm = form_ [method_ "post", action_ "/create"] $ do
                    labelWInput_ "firstName" 
                    labelWInput_ "lastName" 
                    labelWInput_ "phoneNumber" 
                    labelWInput_ "email" 
                    labelWInput_ "addressPrincipal" 
                    labelWInput_ "AddressOptional" 
                    labelWInput_ "zipCode" 
                    input_ [type_ "submit"]

viewContacts :: Contacts -> Html ()
viewContacts cs = div_ $ do
    mapM_ ( \(i, contact)-> div_ $ do
        p_ (toHtml (show i))
        p_ (toHtml (firstName contact))
        p_ (toHtml (phoneNumber contact))
        ) (M.toDescList cs) 
        

--Control


-- test :: Web.Scotty.Internal.Types.ActionT   Data.Text.Internal.Lazy.Text IO ()
-- test = do beam <- param "word"
--           html . renderText $ mdiv beam



mdiv :: Text -> Html ()
mdiv t = div_ $ h1_ (toHtml text)
    where
        text = T.concat ["hello, ", t, "How are you?"]


main :: IO ()
main = do
    db <- newIORef (mempty :: Contacts)
    scotty 3000 $ do
        get "/" $ do
            contacts <- liftIO $ readIORef db
            html . renderText $ viewContacts contacts
        get "/create" $ do
            html . renderText $ html_ $ do
                head_ $ do
                    link_ [rel_ "styleshhet", type_ "text/css", href_ "styles.css"]
                addContactForm 

        post "/create" $ do 
            first <- param "firstName"
            second <- param "lastName"
            number <- param "phoneNumber"
            emailInput <- param "email"
            addressPrincipalInput <- param "addressPrincipal"
            zipInput <- param "zipCode"
            contacts <- liftIO $ readIORef db
            let len = Prelude.length(M.toList contacts)

            let address = Address { street = addressPrincipalInput, zipCode = zipInput}
            let c = Detail {idContact = len + 1, firstName =  first, lastName = second, phoneNumber = number, email = emailInput, addressPrincipal = address }

            liftIO $ addContact db c

            redirect "/"

