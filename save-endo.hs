import System.Environment as SE

-- Should we try faststring?
type DNA = String
type RNA = String

type State = (DNA, RNA)

data PItem = PBase !DNA
           | PSkip !Int
           | PSearch !DNA
           | POpen
           | PClose
           | PRNA !DNA
           | PError

instance Show (PItem) where
  show (PBase x) = x
  show (PSkip x) = "!" ++ show x
  show (PSearch x) = "?" ++ x
  show (POpen) = "("
  show (PClose) = ")"
  show (PError) = "*"

type Pattern = [PItem]

data TItem = TBase !DNA
           | TRef !Int !Int
           | TLen !Int
           | TRNA !DNA
           | TClose
           | TError

instance Show (TItem) where
  show (TBase x) = x
  show (TRef x y) = show x ++ "." ++ show y
  show (TLen x) = "|" ++ show x ++ "|"
  show (TClose) = "."
  show (TError) = "*"

type Template = [TItem]

nat :: DNA -> (Int, DNA)
nat [] = (-1, [])
nat ('P':xs) = (0, xs)
nat (x:xs)
  | x == 'I' || x == 'F' = (2 * natval, natdna)
  | x == 'C' = (2 * natval + 1, natdna)
    where (natval, natdna) = nat xs


consts :: DNA -> (DNA, DNA)
consts ('C':xs) = ('I':cBases, cDNA)
  where (cBases, cDNA) = consts xs
consts ('F':xs) = ('C':cBases, cDNA)
  where (cBases, cDNA) = consts xs
consts ('P':xs) = ('F':cBases, cDNA)
  where (cBases, cDNA) = consts xs
consts ('I':'C':xs) = ('P':cBases, cDNA)
  where (cBases, cDNA) = consts xs
consts dna = ([], dna)

pattern :: DNA -> (String, DNA)
pattern ('C':xs) = ("I", xs)
pattern ('F':xs) = ("C", xs)
pattern ('P':xs) = ("F", xs)
pattern ('I':'C':xs) = ("P", xs)
pattern ('I':'P':xs) = ("!" ++ show natval,  natdna)
  where (natval, natdna) = nat xs
pattern ('I':'F':xs) = ("?" ++ conres, condna)
  where (conres, condna) = consts (drop 1 xs)
pattern ('I':'I':'C':xs) = ("RNA: " ++ take 7 xs, drop 7 xs)
pattern ('I':'I':'I':xs) = ("RNA: " ++ take 7 xs, drop 7 xs)
pattern xs = ("Error", [])

pattern' :: DNA -> (PItem, DNA)
pattern' [] = (PError, [])
pattern' ('C':xs) = (PBase "I", xs)
pattern' ('F':xs) = (PBase "C", xs)
pattern' ('P':xs) = (PBase "F", xs)
pattern' ('I':'C':xs) = (PBase "P", xs)
pattern' ('I':'P':xs) = (PSkip natval,  natdna)
  where (natval, natdna) = nat xs
pattern' ('I':'F':xs) = (PSearch conres, condna)
  where (conres, condna) = consts (drop 1 xs)
pattern' ('I':'I':'P':xs) = (POpen, xs)
pattern' ('I':'I':'C':xs) = (PClose, xs)
pattern' ('I':'I':'F':xs) = (PClose, xs)
pattern' ('I':'I':'I':xs) = (PRNA (take 7 xs), drop 7 xs)
pattern' xs = (PError, [])

runpattern :: DNA -> [RNA] -> Pattern -> Int -> (DNA, Pattern, [RNA])
runpattern [] rnas pat _ = ([], pat, rnas)
runpattern dna rnas pat lvl =
  case pitem of PError -> ([], [], rnas)
                PRNA r -> runpattern pdna (rnas ++ [r]) pat lvl
                POpen  -> runpattern pdna rnas (pat ++ [POpen]) (lvl + 1)
                PClose -> if lvl == 0 then (pdna, pat, rnas)
                                      else runpattern pdna rnas (pat ++ [PClose]) (lvl - 1)
                --PSkip x -> runpattern pdna rnas (pat ++ [PSkip x]) lvl
                item   -> runpattern pdna rnas (pat ++ [pitem]) lvl
  where (pitem, pdna) = pattern' dna

template' :: DNA -> (TItem, DNA)
template' [] = (TError, [])
template' ('C':xs) = (TBase "I", xs)
template' ('F':xs) = (TBase "C", xs)
template' ('P':xs) = (TBase "F", xs)
template' ('I':'C':xs) = (TBase "P", xs)
template' ('I':'P':xs) = (TRef lval nval, ndna)
  where (lval, ldna) = nat xs
        (nval, ndna) = nat ldna 
template' ('I':'F':xs) = (TRef lval nval, ndna)
  where (lval, ldna) = nat xs
        (nval, ndna) = nat ldna 
template' ('I':'I':'C':xs) = (TClose, xs)
template' ('I':'I':'F':xs) = (TClose, xs)
template' ('I':'I':'I':xs) = (TRNA (take 7 xs), drop 7 xs)
template' xs = (TError, [])

runtemplate :: DNA -> [RNA] -> Template -> (DNA, Template, [RNA])
runtemplate [] rnas temp = ([], temp, rnas)
runtemplate dna rnas temp =
  case titem of TError -> ([], [], rnas)
                TRNA r -> runtemplate tdna (rnas ++ [r]) temp
                TClose -> (tdna, temp, rnas)
                item   -> runtemplate tdna rnas (temp ++ [item])
  where (titem, tdna) = template' dna

dnastep :: DNA -> (DNA, Pattern, Template, [RNA])
dnastep dna = (tdna, patt, temp, prna ++ trna)
  where (pdna, patt, prna) = runpattern dna [] [] 0
        (tdna, temp, trna) = runtemplate pdna [] []

protect :: Int -> DNA -> DNA
protect lvl dna
  | lvl == 0 = dna
  | otherwise = protect (lvl - 1) (quote dna)

quote :: DNA -> DNA
quote [] = []
quote (x:xs)
  | x == 'I' = 'C' : quote xs
  | x == 'C' = 'F' : quote xs
  | x == 'F' = 'P' : quote xs
  | x == 'P' = "IC" ++ quote xs

asnat :: Int -> DNA
asnat x 
  | x == 0    = "P"
  | mod2 == 0 = 'I' : asnat (div x 2)
  | otherwise = 'C' : asnat (div x 2)
    where mod2 = mod x 2

main :: IO ()
main = do
       -- c <- getChar
       -- putChar c
       args <- SE.getArgs
       if null args
         then do
           putStrLn "Specify input file"
           return ()
         else do
           let inputFile = head args
           putStrLn inputFile
           dna <- readFile inputFile
           putStrLn (take 100 dna)
           let (d,p,r) = runpattern dna [] [] 0
           print p
           print r
           return ()

