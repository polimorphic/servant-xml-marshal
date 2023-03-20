{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.XML where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Internal
import Data.Conduit.Internal
import Data.Map
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Text.XML
import Text.XML.Marshal

data XML

instance Accept XML where
    contentType _ = "application" // "xml" /: ("charset", "utf-8")

instance ToXml a => MimeRender XML a where
    mimeRender _ = renderLBS def . nodesToDocument . toXml

nodesToDocument :: [Node] -> Document
nodesToDocument ns = case ns of
    [n] -> case n of
        NodeElement ne -> Document
            { documentPrologue = Prologue
                { prologueBefore = []
                , prologueDoctype = Nothing
                , prologueAfter = []
                }
            , documentRoot = ne
            , documentEpilogue = []
            }
        _ -> emptyDoc
    _ -> emptyDoc
  where
    emptyDoc = Document
        { documentPrologue = Prologue
            { prologueBefore = []
            , prologueDoctype = Nothing
            , prologueAfter = []
            }
        , documentRoot = Element
            { elementName = ""
            , elementAttributes = empty
            , elementNodes = []
            }
        , documentEpilogue = []
        }