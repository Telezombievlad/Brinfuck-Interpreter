import System.IO
import System.Environment 
import Control.Monad
import Data.List
import Data.Function
import Data.Word

-- Main

main :: IO ()
main = do
    (inputMode:outputMode:memoryMode:dataMode:filename:_) <- getArgs
    handle <- openFile filename ReadMode
    code <- hGetContents handle
    parseBrainfuck inputMode outputMode memoryMode dataMode code
    hClose handle

-- Enabling different memory modes and output modes modes

memory :: String -> a -> Universe a
memory "mem-def" zero = Universe [] zero (replicate 30000 zero)
memory "mem-inf" zero = Universe (repeat zero) zero (repeat zero)
memory _ _ = error "Undefined memory mode"

inputF :: (Enum a, Read a) => String -> IO a
inputF "in-chr" = do
    ch <- getChar
    return $ toEnum $ fromEnum $ ch 
inputF "in-int" = do
    str <- getLine
    return $ read str    
inputF _ = error "Undefined input mode"

outputF :: (Enum a, Show a) => String -> (a -> IO ())
outputF "out-chr" = putChar . toEnum . fromEnum
outputF "out-int" = putStrLn . show 
outputF _ = error "Undefined output mode"

-- Parsing

parseBrainfuck :: String -> String -> String -> String -> String -> IO ()
parseBrainfuck _ _ _ _ "" = return ()
parseBrainfuck iM oM mM   "data-word8" (ch:str) = pBF (inputF iM) (outputF oM) (Universe (repeat '\0') ch (str ++ (repeat '\0'))) (memory mM (0 :: Word8))
parseBrainfuck iM oM mM     "data-int" (ch:str) = pBF (inputF iM) (outputF oM) (Universe (repeat '\0') ch (str ++ (repeat '\0'))) (memory mM (0 :: Int))
parseBrainfuck iM oM mM "data-integer" (ch:str) = pBF (inputF iM) (outputF oM) (Universe (repeat '\0') ch (str ++ (repeat '\0'))) (memory mM (0 :: Integer))
parseBrainfuck _ _ _ _ _ = error "Undefined data mode"



-- pBF stands for parseBrainFuck
pBF :: (Num a, Enum a, Show a, Eq a) => (IO a) -> (a -> IO ()) -> Universe Char -> Universe a -> IO ()
pBF inCh outCh input@(Universe _ '\0' _) _ = return ()
pBF inCh outCh input@(Universe _  '>' _) dataSpace                    = pBF inCh outCh (goRight input) (goRight dataSpace)           
pBF inCh outCh input@(Universe _  '<' _) dataSpace                    = pBF inCh outCh (goRight input) (goLeft  dataSpace)           
pBF inCh outCh input@(Universe _  '+' _)           (Universe ls x rs) = pBF inCh outCh (goRight input) (Universe ls (x+1) rs) 
pBF inCh outCh input@(Universe _  '-' _)           (Universe ls x rs) = pBF inCh outCh (goRight input) (Universe ls (x-1) rs)
pBF inCh outCh input@(Universe _  '.' _) dataSpace@(Universe  _ x  _) = (outCh x) >> pBF inCh outCh (goRight input) dataSpace
pBF inCh outCh input@(Universe _  ',' _)           (Universe ls x rs) = inCh >>= (\ch -> pBF inCh outCh (goRight input) (Universe ls (toEnum (fromEnum ch)) rs))
pBF inCh outCh input@(Universe _  '[' _) dataSpace@(Universe  _ x  _) = pBF inCh outCh (if x == 0 then nextBr 0 input else goRight  input) dataSpace
pBF inCh outCh input@(Universe _  ']' _) dataSpace@(Universe  _ x  _) = pBF inCh outCh (if x == 0 then goRight  input else prevBr 0 input) dataSpace 
pBF inCh outCh input dataSpace = pBF inCh outCh (goRight input) dataSpace

-- Additional

data Universe a = Universe [a] a [a] deriving Show

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

goLeft :: Universe a -> Universe a
goLeft (Universe (l:ls) x rs) = Universe ls l (x:rs)
goLeft _ = error "Invalid finite universe" 

goRight :: Universe a -> Universe a
goRight (Universe ls x (r:rs)) = Universe (x:ls) r rs
goRight _ = error "Invalid finite universe"
