{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module MessageTH (options) where

import Data.Aeson.TH
  ( Options (sumEncoding, tagSingleConstructors),
    SumEncoding (TaggedObject, contentsFieldName, tagFieldName),
    defaultOptions,
  )

options :: Options
options = defaultOptions {tagSingleConstructors = True, sumEncoding = TaggedObject {tagFieldName = "(tag)", contentsFieldName = "(contents)"}}