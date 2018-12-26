module Shader where

import Control.Exception
import Control.Monad
import Data.Foldable
import Foreign
import Foreign.C

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString (pack)
import Graphics.GL

newtype Shader = Shader { shaderID :: GLuint }
  deriving (Eq, Ord, Show)

data ShaderSrc = InlineShader !ByteString | FileShader FilePath
  deriving (Eq, Ord, Show)

shaderSrc :: ShaderSrc -> IO ByteString
shaderSrc (InlineShader src) = pure src
shaderSrc (FileShader fp) = ByteString.readFile fp

shaderSrcLoc :: ShaderSrc -> FilePath
shaderSrcLoc (InlineShader _) = "<inline>"
shaderSrcLoc (FileShader fp) = fp

data ShaderType
  = ComputeShader
  | VertexShader
  | TessControlShader
  | TessEvaluationShader
  | GeometryShader
  | FragmentShader
  deriving (Eq, Ord, Show)

shaderType :: ShaderType -> GLenum
shaderType = \case
  ComputeShader        -> GL_COMPUTE_SHADER
  VertexShader         -> GL_VERTEX_SHADER
  TessControlShader    -> GL_TESS_CONTROL_SHADER
  TessEvaluationShader -> GL_TESS_EVALUATION_SHADER
  GeometryShader       -> GL_GEOMETRY_SHADER
  FragmentShader       -> GL_FRAGMENT_SHADER

data ShaderException
  = ShaderFailedToCompile FilePath String
  | ShaderProgramFailedToLink String

instance Show ShaderException where
  show (ShaderFailedToCompile fp msg)
    = mconcat
      [ "Shader at "
      , fp
      , " failed to compile. OpenGL error log:\n"
      , msg
      ]
  show (ShaderProgramFailedToLink msg)
    = "Shader program failed to compile. OpenGL error log:\n" <> msg

instance Exception ShaderException

compileNewShader :: ShaderType -> ShaderSrc -> IO Shader
compileNewShader sty ssrc = do
  -- Load the shader's source and compile it
  shader <- glCreateShader (shaderType sty)
  src <- shaderSrc ssrc
  ByteString.useAsCString src
    \cstr -> withPtr cstr \cstr_p -> glShaderSource shader 1 cstr_p nullPtr
  glCompileShader shader

  -- Check for errors
  status <- overPtr $ glGetShaderiv shader GL_COMPILE_STATUS
  when (status == 0) do
    infoLogLength <- overPtr $ glGetShaderiv shader GL_INFO_LOG_LENGTH
    errors <- allocaArray (fromIntegral infoLogLength) \arr -> do
      glGetShaderInfoLog shader infoLogLength nullPtr arr
      peekArray (fromIntegral infoLogLength) arr
    
    throwIO $ ShaderFailedToCompile (shaderSrcLoc ssrc) (castCCharToChar <$> errors)

  pure (Shader shader)
  
overPtr :: (Storable a) => (Ptr a -> IO void) -> IO a
overPtr k = alloca \ptr -> k ptr >> peek ptr

withPtr :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
withPtr a k = alloca \ptr -> poke ptr a >> k ptr

newtype Program = Program { programID :: GLuint }

attachShaders :: [Shader] -> IO Program
attachShaders shaders = do
  program <- glCreateProgram
  for_ shaders $ glAttachShader program . shaderID

  withCString "outColor" $ glBindFragDataLocation program 0

  glLinkProgram program
  status <- overPtr $ glGetProgramiv program GL_LINK_STATUS
  when (status == 0) do
    infoLogLength <- overPtr $ glGetProgramiv program GL_INFO_LOG_LENGTH
    errors <- allocaArray (fromIntegral infoLogLength) \arr -> do
      glGetProgramInfoLog program infoLogLength nullPtr arr
      peekArray (fromIntegral infoLogLength) arr
    
    throwIO $ ShaderProgramFailedToLink (castCCharToChar <$> errors)
  
  pure (Program program)