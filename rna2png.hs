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
     deriving Show

black   = CRGB (0, 0, 0)
red     = CRGB (255, 0, 0)
green   = CRGB (0, 255, 0)
yellow  = CRGB (255, 255, 0)
blue    = CRGB (0, 0, 255)
magenta = CRGB (255, 0, 255)
cyan    = CRGB (0, 255, 255)
white   = CRGB (255, 255, 255)

transparent = CTrans 0
opaque      = CTrans 255

data Bucket = Bucket { rTot :: Int
                     , gTot :: Int
                     , bTot :: Int
                     , aTot :: Int
                     , cNum :: Int
                     , tNum :: Int
                     } deriving Show

type Pos = (Int, Int)
data Dir = N | E | S | W
data Pix = Pix { r :: Int
                   , g :: Int
                   , b :: Int
                   , a :: Int
                   } deriving Show

-- type Bitmap = [

-- type State = (Bucket, Pos, Pos, Dir, [Bitmap])

emptyBucket :: Bucket
emptyBucket = Bucket 0 0 0 0 0 0 

addColor :: Color -> Bucket -> Bucket
addColor (CRGB (r,g,b)) (Bucket br bg bb ba bc bt) = Bucket (br + r) (bg + g) (bb + b) ba (bc + 1) bt
addColor (CTrans t) (Bucket br bg bb ba bc bt) = Bucket br bg bb (ba + t) bc (bt + 1)

drip :: Bucket -> Pix
drip bucket = Pix pr pg pb pa
  where pr = div (rTot bucket) (max 1 (cNum bucket))
        pg = div (gTot bucket) (max 1 (cNum bucket))
        pb = div (bTot bucket) (max 1 (cNum bucket))
        pa = div (aTot bucket) (max 1 (tNum bucket))

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

fixpos :: Pos -> Pos
fixpos (x,y) = (fixcoord x, fixcoord y)
  where fixcoord c 
          | c < 0 = 0
          | c > 599 = 599
          | otherwise = c

move :: Dir -> Pos -> Pos
move dir (inx, iny) = fixpos (outx, outy)
  where outx = case dir of E -> inx + 1
                           W -> inx - 1
                           _ -> inx
        outy = case dir of S -> iny + 1
                           N -> iny - 1
                           _ -> iny

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

