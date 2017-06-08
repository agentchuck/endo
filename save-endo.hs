import System.Environment as SE

-- Should we try faststring?
type DNA = String

patt :: DNA -> (String, DNA)
patt ('I':'I':'I':xs) = ("RNA: " ++ take 7 xs, drop 7 xs)
patt xs = ("Error", [])


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

