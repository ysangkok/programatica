-- A proxy module to make it easy to switch between different parser combinator
-- implementations.

module ParserType(
  module
    --OldParsers
    HBCParsers
    --P -- doesn't work with HBC
) where

-- Pick and export one:
--import OldParsers --as P
import HBCParsers --as P
