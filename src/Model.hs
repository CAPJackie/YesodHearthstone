{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")



-- {"id":1, "deckId":3, "name":"holi", "cost":3000, "attack":3000, "health":5000, "text":"Example card" }
instance ToJSON (Entity Card) where
    toJSON (Entity cardId card) = object
        [
            "id" .= (String $ toPathPiece cardId),
            "deckId" .= cardDeckId card,
            "name" .= cardName card,
            "cost" .= cardCost card,
            "attack" .= cardAttack card,
            "health" .= cardHealth card,
            "text" .= cardText card
        ]

instance FromJSON Card where
    parseJSON (Object card) = Card
        <$> card .: "deckId"
        <*> card .: "name"
        <*> card .: "cost"
        <*> card .: "attack"
        <*> card .: "health"
        <*> card .: "text"
    parseJSON _ = mzero


data Privileges =
    PrvRegisteredUser
    deriving (Show,Read,Eq)
    
derivePersistField "Privileges"