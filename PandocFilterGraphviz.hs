--- Copyright (c) 2017, Jean-Pierre PRUNARET
---
--- All rights reserved.
---
--- Redistribution and use in source and binary forms, with or without
--- modification, are permitted provided that the following conditions are met:
---
---     * Redistributions of source code must retain the above copyright
---       notice, this list of conditions and the following disclaimer.
---
---     * Redistributions in binary form must reproduce the above
---       copyright notice, this list of conditions and the following
---       disclaimer in the documentation and/or other materials provided
---       with the distribution.
---
---     * Neither the name of Jean-Pierre PRUNARET nor the names of other
---       contributors may be used to endorse or promote products derived
---       from this software without specific prior written permission.
---
--- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE OverloadedStrings #-}

module PandocFilterGraphviz where

import           Control.Monad          (unless)
import           Crypto.Hash

import           Data.Byteable          (toBytes)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8

import qualified Data.Map.Strict        as M
import           Data.Text              as T
import           Data.Text.Encoding     as E

import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process         (readProcess, system)

import           Text.Pandoc
import           Text.Pandoc.JSON

(¤) :: Text -> Text -> Text
(¤) = T.append

hexSha3_512 :: ByteString -> ByteString
hexSha3_512 bs = C8.pack $ show (hash bs :: Digest SHA3_512)

sha :: Text -> Text
sha = E.decodeUtf8 . hexSha3_512 . B16.encode . E.encodeUtf8

data RenderFormat
  = SVG
  | EPS
  deriving (Show)

fileName4Code :: RenderFormat -> Text -> Text -> Maybe Text -> FilePath
fileName4Code format name source ext = filename
  where
    dirname = name ¤ "-images"
    shaN = sha source
    extension =
      case format of
        SVG -> ".svg"
        EPS -> ".eps"
    barename =
      shaN ¤
      (case ext of
         Just "msc" -> extension
         Just "dot" -> extension
         Just e     -> "." ¤ e
         Nothing    -> "")
    filename = T.unpack dirname </> T.unpack barename

getCaption :: M.Map Text Text -> (Text, Text)
getCaption m =
  case M.lookup "caption" m of
    Just cap -> (cap, "fig:")
    Nothing  -> ("", "")

ensureWriteFile :: FilePath -> String -> IO ()
ensureWriteFile fp contents = do
  let dir = takeDirectory fp
  createDirectoryIfMissing True dir
  exists <- doesFileExist fp
  unless exists $ writeFile fp contents

formatToFlag :: RenderFormat -> String
formatToFlag format =
  case format of
    SVG -> "svg"
    EPS -> "eps"

renderDot :: RenderFormat -> String -> FilePath -> IO FilePath
renderDot format src dst = do
  ensureWriteFile dst =<< readProcess cmd args src
  return dst
  where
    cmd = "dot"
    args = ["-T", formatToFlag format]

-- Here is some msc rendering stuff
renderMsc :: RenderFormat -> String -> FilePath -> IO FilePath
renderMsc format src dst = do
  ensureWriteFile dst =<< readProcess cmd args src
  return dst
  where
    cmd = "mscgen"
    args = ["-T", formatToFlag format, "-o", "-"]

-- and we combine everything into one function
--
data RenderAllOptions = RenderAllOptions
  { urlPrefix    :: Maybe String
  , renderFormat :: RenderFormat
  } deriving (Show)

renderAll :: RenderAllOptions -> Block -> IO Block
renderAll options cblock@(CodeBlock (id, classes, attrs) content)
  | "msc" `elem` classes =
    let dest = fileName4Code format "mscgen" (T.pack content) (Just "msc")
     in do img <- renderMsc format content dest
           return $ image img
  | "graphviz" `elem` classes =
    let dest = fileName4Code format "graphviz" (T.pack content) (Just "dot")
     in do img <- renderDot format content dest
           return $ image img
  | otherwise = return cblock
  where
    format = renderFormat options
    toTextPairs = Prelude.map (\(f, s) -> (T.pack f, T.pack s))
    m = M.fromList $ toTextPairs attrs
    (caption, typedef) = getCaption m
    image img =
      Para
        [ Image
            (id, classes, attrs)
            [Str $ T.unpack caption]
            ( case urlPrefix options of
                Just prefix -> prefix </> img
                Nothing     -> img
            , T.unpack caption)
        ]
renderAll pre x = return x

relativizePandocUrls :: String -> Inline -> Inline
relativizePandocUrls with (Image a b (url, title)) =
  Image a b (with ++ url, title)
relativizePandocUrls with x = x

stripHeading :: Block -> Block
stripHeading cblock@(Header level att content) = Null
stripHeading x                                 = x
