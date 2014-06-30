{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

import Utils (splitWordsKeepCase)

import Categorize
import Entities
import Summarize

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
  categories <- lookupSession "categories"
  humanNames <- lookupSession "humanNames"
  countryNames <- lookupSession "countryNames"
  companyNames <- lookupSession "companyNames"
  summary <- lookupSession "summary"
  summary_s <- lookupSession "summary_s"
  the_text <- lookupSession "the_text"
  deleteSession "categories"
  deleteSession "humanNames"
  deleteSession "countryNames"
  deleteSession "companyNames"
  deleteSession "summmary"
  deleteSession "summmary_s"
  deleteSession "the_text"
  toWidget [lucius|
            body { margin:0.7cm 1cm 1cm 1cm; }
   |]
  [whamlet|
     <h2>This is a test of an initial port of KnowledgeBooks Natural Language Processing (NLP) code to Haskell
     <h4>Enter plain text (no special characters):
     <form method=post>
        <textarea type=text name=name rows="6" cols="70">
        <br>
        <br>
        <input type=submit value="Process text">
     <br>
     <p>#{fromMaybe "" the_text}
     <h4>Summary of text:
     <p>#{fromMaybe "" summary_s}
     <h4>Category results from combined 1gram and 2gram analysis:
     <p>#{fromMaybe "" categories}
     <h4>Human names found in text:
     <p>#{fromMaybe "" humanNames}
     <h4>Country names found in text:
     <p>#{fromMaybe "" countryNames}
     <h4>Company names found in text:
     <p>#{fromMaybe "" companyNames}
     <h4>Summary of text, with scoring:
     <p>#{fromMaybe "" summary}
     <br>
     <br>
     <div>
       <p>Compared to previous versions written in Common Lisp, Scheme, Java, and Clojure the code is very compact. The current code size is:
         <ul>
          <li>Yesod web app: 90 lines
          <li>NLP code: 500 lines
       <p>
         <i>Copyright 2014 Mark Watson.
   |]

postHomeR :: Handler ()
postHomeR = do
    name <- runInputPost $ ireq textField "name"
    --setMessage $ toHtml $ T.pack $
    --  "Assigned tags from combined 1gram and 2gram analysis: " ++ (show $ bestCategories $ splitWords $ T.unpack name)
    setSession "categories" $ T.pack $ (show $ bestCategories $ splitWords $ T.unpack name)
    setSession "humanNames" $ T.pack $ (show $ humanNames $ T.unpack name)
    setSession "countryNames" $ T.pack $ (show $ countryNames $ splitWordsKeepCase $ T.unpack name)
    setSession "companyNames" $ T.pack $ (show $ companyNames $ splitWordsKeepCase $ T.unpack name)
    setSession "summary" $ T.pack $ (show $ summarize $ T.unpack name)
    setSession "summary_s" $ T.pack $ (show $ summarize_s $ T.unpack name)
    setSession "the_text" name
    redirectUltDest HomeR
    
main :: IO ()
main = warp 3000 App
