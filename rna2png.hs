import Codec.Picture
import Control.Monad
import Control.Monad.ST
--import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M
--import qualified Data.Array.Repa     as R -- for Repa

data ImgFormat = Bmp | Jpg | Png | Tiff

-- proc build () =
-- rna ← read
-- foreach r ∈ rna
-- case r is of the form
-- ‘PIPIIIC’ ⇒ addColor (black rgb)
-- ‘PIPIIIP’ ⇒ addColor (red rgb)
-- ‘PIPIICC’ ⇒ addColor (green rgb)
-- ‘PIPIICF’ ⇒ addColor (yellow rgb)
-- ‘PIPIICP’ ⇒ addColor (blue rgb)
-- ‘PIPIIFC’ ⇒ addColor (magenta rgb)
-- ‘PIPIIFF’ ⇒ addColor (cyan rgb)
-- ‘PIPIIPC’ ⇒ addColor (white rgb)
-- ‘PIPIIPF’ ⇒ addColor (transparent α)
-- ‘PIPIIPP’ ⇒ addColor (opaque α)
-- ‘PIIPICP’ ⇒ bucket ← ε
-- ‘PIIIIIP’ ⇒ position ← move (position, dir)
-- ‘PCCCCCP’ ⇒ dir ← turnCounterClockwise (dir)
-- ‘PFFFFFP’ ⇒ dir ← turnClockwise (dir)
-- ‘PCCIFFP’ ⇒ mark ← position
-- ‘PFFICCP’ ⇒ line (position, mark)
-- ‘PIIPIIP’ ⇒ tryfill ()
-- ‘PCCPFFP’ ⇒ addBitmap (transparentBitmap)
-- ‘PFFPCCP’ ⇒ compose ()
-- ‘PFFICCF’ ⇒ clip ()
-- anything else ⇒ do nothing
-- end case
-- end foreach
-- draw bitmaps[0] all alpha values are set to 255!
-- exit

type RGB = (Int, Int, Int)
data Color = CRGB   !RGB 
           | CTrans !Int

data Bucket = Bucket { rTot :: Int
                      , gTot :: Int
                      , bTot :: Int
                      , aTot :: Int
                      , cNum :: Int
                      , tNum :: Int
                      }
--type Pos = (Int, Int)
data Dir = N | E | S | W
--type Pixel = (Int, Int, Int, Int)
-- type Bitmap = [

-- type State = (Bucket, Pos, Pos, Dir, [Bitmap])

ccw :: Dir -> Dir
ccw dir = 
  case dir of N -> W
              E -> N
              W -> E
              S -> S

cw :: Dir -> Dir
cw dir = 
  case dir of N -> E
              E -> S
              W -> W
              S -> N


main :: IO ()
main = do
  [rnaPath] <- getArgs
  savePngImage rnaPath generateImg

generateImg :: DynamicImage
generateImg = ImageRGB8 (generateImage originalFnc 1200 1200)

originalFnc :: Int -> Int -> PixelRGB8
originalFnc x y =
  let (q, r) = x `quotRem` max 10 y
      s      = fromIntegral . min 0xff
  in PixelRGB8 (s q) (s r) (s (q + r + 30))

