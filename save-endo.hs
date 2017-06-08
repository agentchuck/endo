import System.Environment as SE

-- Should we try faststring?
type DNA = String

data PItem = PBase !String

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
pattern ('I':'I':'I':xs) = ("RNA: " ++ take 7 xs, drop 7 xs)
pattern xs = ("Error", [])


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
           return ()

