{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Shader.Quote where

import Data.Char
import Data.List

import Data.ByteString.Char8 as ByteString (pack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Shader

shaderSrc :: QuasiQuoter
shaderSrc = QuasiQuoter
  { quoteExp  = quoteShaderSrc
  , quotePat  = error "shaderSrc: not a pattern QQ"
  , quoteType = error "shaderSrc: not a type QQ"
  , quoteDec  = error "shaderSrc: not a declaration QQ"
  }

quoteShaderSrc :: String -> Q Exp
quoteShaderSrc str
  | ("file", fp) <- break isSpace (dropWhile isSpace str)
  = let fp' = dropWhileEnd isSpace fp
    in [| FileShader fp' |]
  | otherwise
  = [| InlineShader (ByteString.pack str) |]

shader :: QuasiQuoter
shader = QuasiQuoter
  { quoteExp  = quoteShader
  , quotePat  = error "shader: not a pattern QQ"
  , quoteType = error "shader: not a type QQ"
  , quoteDec  = error "shader: not a declaration QQ"
  }

deriving instance Lift ShaderType

quoteShader :: String -> Q Exp
quoteShader (break isSpace . dropWhile isSpace -> (sty, ssrc)) =
  let
    qSty :: Q Exp
    qSty = lift =<< case fmap toLower sty of
      "compute"         -> pure ComputeShader
      "vertex"          -> pure VertexShader
      "tess_control"    -> pure TessControlShader
      "tess_evaluation" -> pure TessEvaluationShader
      "geometry"        -> pure GeometryShader
      "fragment"        -> pure FragmentShader
      _                 -> fail $ mconcat
        [ "invalid shader type: "
        , sty
        , "; must be one of: "
        , "COMPUTE, VERTEX, TESS_CONTROL, TESS_EVALUATION, GEOMETRY, FRAGMENT."
        ]
    qShaderSrc = quoteShaderSrc ssrc
  in [| compileNewShader $qSty $qShaderSrc |]