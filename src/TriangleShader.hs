{-# LANGUAGE QuasiQuotes #-}
module TriangleShader where

import Shader
import Shader.Quote


triangleVertex :: IO Shader
triangleVertex = [shader| vertex
  # version 410

  in vec2 position;

  void main()
  {
      gl_Position = vec4(position, 0.0, 1.0);
  }
  |]

triangleFragment :: IO Shader
triangleFragment = [shader| fragment
  # version 410

  in vec2 position;

  void main()
  {
      gl_Position = vec4(position, 0.0, 1.0);
  }
  |]