{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

import Categorize

data App = App

mkYesod "App" [parseRoutes|
/         HomeR     GET POST
|]
  
instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
    
readSession :: Read a => T.Text -> Handler (Maybe a)
readSession name = do
    textValue <- lookupSession name
    return (readMaybe . T.unpack =<< textValue)
    

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Haskell Categorization Demo"
  res <- lookupSession "res"
  deleteSession "res"
  toWidget [lucius|body { margin:0.7cm 1cm 1cm 1cm; } |]
  [whamlet|
     <h2>This is a test of an initial port of KnowledgeBooks categorization code to Haskell
     <h4>Enter plain text:
     <form method=post>
        <textarea type=text name=name rows="6" cols="70">
        <br>
        <br>
        <input type=submit value="Process text">
     <br>
     <h4>Results from combined 1gram and 2gram analysis:
     <p>#{fromMaybe "" res}
     <br>
     <p>Code size:
       <ul>
        <li>Yesod web app: 41 lines
        <li>NLP code: 62 lines
     <p>
       <i>Copyright 2014 Mark Watson.
   |]

postHomeR :: Handler ()
postHomeR = do
    name <- runInputPost $ ireq textField "name"
    --setMessage $ toHtml $ T.pack $
    --  "Assigned tags from combined 1gram and 2gram analysis: " ++ (show $ bestCategories $ splitWords $ T.unpack name)
    setSession "res" $ T.pack $ (show $ bestCategories $ splitWords $ T.unpack name)
    redirectUltDest HomeR
    
main :: IO ()
main = warp 3000 App
