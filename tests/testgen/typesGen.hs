import Data.List
import System.FilePath.Posix
import System.Environment
import System.Process

-- generator of types in the following form
-- sub : !c;?d;rec X. [!a0;end,  !a1;?b0;?b1;X, ..., !ax;?b0;..;?bx;X]
-- sup : ?d;!c;rec X. [!a0;end,  !a1;?b0;?b1;X, ..., !ay;?b0;..;?by;X] 

mkPair :: Int -> Int -> (String, String)
mkPair x y = let sub = "!c;?d;rec X. [!a0;end, "++ branches 0 x ++"]"
                 sup = "?d;!c;rec X. [!a0;end, "++ branches 0 y ++"]"
             in (sub, sup)


branches :: Int -> Int -> String
branches br i = intercalate ", " $
               map (\x -> branch br x) [1..i]
  where branch _ i = sndi "a" i ++ ";" ++ rcvij "b" 0 i ++ ";X"


opij :: String -> String -> Int -> Int -> String
opij dir a i j = intercalate ";" $
              map (\x -> dir++a++(show x)) [i..j]

rcvij :: String -> Int -> Int -> String
rcvij a i j = opij "?" a i j

sndij :: String -> Int -> Int -> String
sndij a i j = opij "!" a i j

rcvi :: String -> Int -> String
rcvi a i = rcvij a i i

sndi :: String -> Int -> String
sndi a i = sndij a i i


typesGen :: Int -> Int -> IO ()
typesGen x y =
  let (sub, sup) = mkPair x y
  in do writeFile "sub.txt" sub
        writeFile "sup.txt" sup
        return ()


main :: IO ()
main = do  args <- getArgs
           if ((length args) /= 2)
             then do putStrLn $ "Usage: typesGen <int> <int>\n"
                             ++ "       number of uncontrollable loops in the subtype and supertype respectively"
                     return ()
             else do let x = read (args!!0) :: Int
                         y = read (args!!1) :: Int
                         
                     typesGen x y
