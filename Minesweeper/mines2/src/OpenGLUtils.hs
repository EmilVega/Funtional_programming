module OpenGLUtils where

import Graphics.UI.GLUT
import Graphics.GLUtil
import Control.Applicative

loadGLTextureFromFile :: FilePath -> IO TextureObject
loadGLTextureFromFile f = do t <- either error id <$> readTexture f
                             textureFilter Texture2D $= ((Linear', Nothing), Linear')
                             texture2DWrap $= (Mirrored, ClampToEdge)
                             return t

drawTexture :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> IO()
drawTexture (u,v) (uw,vh) (x,y) (w,h) = do
    renderPrimitive Quads $ do
        tex (u)      (v - vh) >> ver (x)     (y - h) -- Top left coor: (-1, 1)
        tex (u + uw) (v - vh) >> ver (x + w) (y - h)
        tex (u + uw) (v)      >> ver (x + w) (y)
        tex (u)      (v)      >> ver (x)     (y)
        where ver x y = vertex (Vertex2 x y :: Vertex2 GLfloat)
              tex u v = texCoord (TexCoord2 u v :: TexCoord2 GLfloat)
