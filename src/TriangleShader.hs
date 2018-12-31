{-# LANGUAGE QuasiQuotes #-}
module TriangleShader where

import Shader
import Shader.Quote


shVertex2D :: IO Shader
shVertex2D = [shader| vertex file shaders\triangle2D.vertex |]

shFragment2D :: IO Shader
shFragment2D = [shader| fragment file shaders\triangle2D.fragment |]