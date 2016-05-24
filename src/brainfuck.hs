import System.IO
import System.Environment 
import Control.Monad
import Data.List
import Data.Function
import Data.Word

main :: IO ()
main = do
    (filename:_) <- getArgs
    handle <- openFile filename ReadMode
    code <- hGetContents handle
    parseBrainfuck code
    hClose handle


parseBrainfuck :: String -> IO ()
parseBrainfuck ""       = return ()
parseBrainfuck (ch:str) = pBF (Universe (repeat '\0') ch (str ++ (repeat '\0'))) (Universe (repeat 0) 0 (repeat 0))

-- pBF stands for parseBrainFuck
pBF :: Universe Char -> Universe Word8 -> IO ()
pBF input@(Universe _ '\0' _) _ = return ()
pBF input@(Universe _  '>' _) dataSpace                    = pBF (goRight input) (goRight dataSpace)           
pBF input@(Universe _  '<' _) dataSpace                    = pBF (goRight input) (goLeft  dataSpace)           
pBF input@(Universe _  '+' _)           (Universe ls x rs) = pBF (goRight input) (Universe ls (x+1) rs) 
pBF input@(Universe _  '-' _)           (Universe ls x rs) = pBF (goRight input) (Universe ls (x-1) rs)
pBF input@(Universe _  '.' _) dataSpace@(Universe  _ x  _) = putChar (toEnum (fromEnum x)) >> pBF (goRight input) dataSpace
pBF input@(Universe _  ',' _)           (Universe ls x rs) = getChar >>= (\ch -> pBF (goRight input) (Universe ls (toEnum (fromEnum ch)) rs))
pBF input@(Universe _  '[' _) dataSpace@(Universe  _ x  _) = pBF (if x == 0 then nextBr 0 input else goRight  input) dataSpace
pBF input@(Universe _  ']' _) dataSpace@(Universe  _ x  _) = pBF (if x == 0 then goRight  input else prevBr 0 input) dataSpace 
pBF input dataSpace = pBF (goRight input) dataSpace

nextBr :: Int -> Universe Char -> Universe Char
nextBr _       (Universe  _ '\0'  _) = error "Parse error: [ and ] mismatch"
nextBr 1 input@(Universe ls  ']' rs) = input
nextBr x input@(Universe  _  ']'  _) = nextBr (x-1) (goRight input) 
nextBr x input@(Universe  _  '['  _) = nextBr (x+1) (goRight input) 
nextBr x input = nextBr x (goRight input)

prevBr :: Int -> Universe Char -> Universe Char
prevBr _       (Universe  _ '\0'  _) = error "Parse error: [ and ] mismatch"
prevBr 1 input@(Universe ls  '[' rs) = input
prevBr x input@(Universe  _  ']'  _) = prevBr (x+1) (goLeft input) 
prevBr x input@(Universe  _  '['  _) = prevBr (x-1) (goLeft input)
prevBr x input = prevBr x (goLeft input)

-- both of list are infinite
data Universe a = Universe [a] a [a] deriving Show

goLeft :: Universe a -> Universe a
goLeft (Universe (l:ls) x rs) = Universe ls l (x:rs)
goLeft _ = error "Invalid finite universe" 

goRight :: Universe a -> Universe a
goRight (Universe ls x (r:rs)) = Universe (x:ls) r rs
goRight _ = error "Invalid finite universe"
