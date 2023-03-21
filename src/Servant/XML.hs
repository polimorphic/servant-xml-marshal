{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.XML where

import Data.Map (empty)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept, MimeRender, contentType, mimeRender)
import Text.XML (renderLBS, def)
import Text.XML.Marshal (ToXmlDocument, toXmlDocument)

data XML

instance Accept XML where
    contentType _ = "application" // "xml" /: ("charset", "utf-8")

instance ToXmlDocument a => MimeRender XML a where
    mimeRender _ = renderLBS def . toXmlDocument
