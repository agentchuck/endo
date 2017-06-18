--https://wiki.haskell.org/OpenGLTutorial1
--http://www.renci.org/wp-content/pub/tutorials/BeautifulCode.pdf

import qualified Graphics.Rendering.Cairo as C
main = do
 C.withImageSurface
 C.FormatARGB32 100 100 $ \surf -> do
 C.renderWith surf $ do
 C.save
 C.setOperator C.OperatorOver
 C.setSourceRGB 0 0 0
 C.rectangle 0 0 100 100
 C.fill
 C.setSourceRGB 1 1 1
 C.selectFontFace "Trebuchet MS"
 C.FontSlantNormal
 C.FontWeightNormal
 C.setFontSize 18
 C.textPath "Hello world"
 C.fill
 C.restore
 C.surfaceWriteToPNG surf "Text.png"
