{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Deck where

import Import
import Database.Persist.Sql (rawSql)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                                )


deckForm :: Maybe Deck -> AForm Handler Deck
deckForm deck = Deck
                    <$> areq textField "name" (deckName <$> deck)


cardForm :: Maybe DeckId -> Maybe Card -> AForm Handler Card
cardForm did card = Card
                            did
                            <$> areq textField "name" (cardName <$> card)
                            <*> areq intField "cost" (cardCost <$> card)
                            <*> areq intField "attack" (cardAttack <$> card)
                            <*> areq intField "health" (cardHealth <$> card)
                            <*> areq textField "text" (cardText <$> card)

getDeckNewR :: Handler Html
getDeckNewR = do
    (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ deckForm Nothing
    defaultLayout $ do
        let actionR = DeckNewR
        $(widgetFile "Deck/DeckCreate")


postDeckNewR :: Handler Html
postDeckNewR = do
    ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ deckForm  Nothing
    case result of
        FormSuccess deck -> do 
            _ <- runDB $ insert deck
            redirect DeckListR
        _ -> defaultLayout $ do
            let actionR = DeckNewR
            $(widgetFile "Deck/DeckCreate")


getDeckListR ::  Handler Html
getDeckListR  = do
    decks <- runDB $ selectList [] []
    ( _ , _ ) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ deckForm Nothing
    defaultLayout $ do
        $(widgetFile "Deck/DeckList")



getDeckNewCardR :: DeckId -> Handler Html
getDeckNewCardR deckId = do
    (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ cardForm (Just deckId) Nothing
    defaultLayout $ do
        let actionR = (DeckNewCardR deckId)
        $(widgetFile "Deck/CardCreate")

postDeckNewCardR :: DeckId -> Handler Html
postDeckNewCardR deckId = do
    ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ cardForm (Just deckId) Nothing
    case result of
        FormSuccess card -> do
            _ <- runDB $ insert card
            redirect DeckListR
        _ -> defaultLayout $ do
            let actionR = (DeckNewCardR deckId)
            $(widgetFile "Deck/CardCreate")


getDeckListCardsR :: DeckId -> Handler Html
getDeckListCardsR deckId = do
    cards <- selectCardsByDeckId deckId
    defaultLayout $ do
        let actionR = (DeckListCardsR deckId)
        $(widgetFile "Deck/DeckCardsList")

selectCardsByDeckId :: DeckId -> Handler [Entity Card]
selectCardsByDeckId deckId = runDB $ rawSql s [toPersistValue deckId]
    where s = "SELECT distinct ?? FROM deck, card WHERE card.deck_id = ?"





getCardsJsonR :: Handler Value
getCardsJsonR = do
    cards <- runDB $ selectList [] [] :: Handler [Entity Card]

    return $ object ["cards" .= cards]


postCardsJsonR :: Handler Value
postCardsJsonR = do
    card <- requireJsonBody :: Handler Card
    _    <- runDB $ insert card

    sendResponseStatus status201 ("CREATED" :: Text)



getCardJsonR :: CardId -> Handler Value
getCardJsonR cardId = do
    card <- runDB $ get404 cardId

    return $ object ["card" .= (Entity cardId card)]


putCardJsonR :: CardId -> Handler Value
putCardJsonR cardId = do
    card <- requireJsonBody :: Handler Card

    runDB $ replace cardId card

    sendResponseStatus status200 ("UPDATED" :: Text)


deleteCardJsonR :: CardId -> Handler Value
deleteCardJsonR cardId = do
    runDB $ delete cardId

    sendResponseStatus status200 ("DELETED" :: Text)