import Data.Char

type Program = [Cmd]
data Cmd = GoR | GoL | Incr | Decr | Write | Read | Loop [Cmd]
     deriving (Show, Eq)

compile :: [Char] -> Program
compile code = prog
    where (prog, leftovers) = parse code

parse :: [Char] -> (Program, [Char])
parse [] = ([], [])
parse ('[' : morechars) = (Loop loopbody : tail, leftovers)
      where 
      	    (tail, leftovers) = parse afterloop
      	    (loopbody, afterloop) = parse morechars
parse (']' : morechars) = ([], morechars)
parse (x : morechars) 
      | '>' == x = (GoR : tail, leftovers)
      | '<' == x = (GoL : tail, leftovers)
      | '+' == x = (Incr : tail, leftovers)
      | '-' == x = (Decr : tail, leftovers)
      | '.' == x = (Write : tail, leftovers)
      | ',' == x = (Read : tail, leftovers)
      | otherwise = error "Not a valid char"
      where (tail, leftovers) = parse morechars

type Tape = ([Char], Char, [Char])

run :: Program -> [Char] -> [Char]
run prog stdin = stdout
    where (tape, _, stdout) = eval prog ([], '\0', ['\0','\0'..]) stdin

eval :: Program -> Tape -> [Char] -> (Tape, [Char], [Char])
eval [] tape stdin = (tape, stdin, [])

-- TODO: recursively process loops

eval (GoL : morecmd) (y : lefttape, x, righttape) stdin
     = eval morecmd (lefttape, y, x : righttape) stdin
eval (GoL : morecmd) ([], _, _) stdin
     = error "Can't go left past the start of the tape."
eval (GoR : morecmd) (lefttape, x, y : righttape) stdin
     = eval morecmd (x : lefttape, y, righttape) stdin
eval (Read : morecmd) tape []
     = error "No more stdin to read."
eval (Read : morecmd) (lefttape, x, righttape) (i : morein)
     = eval morecmd (lefttape, i, righttape) morein
eval (Write : morecmd) (lefttape, x, righttape) stdin
     = (tape, stdin', x : stdout)
     where (tape, stdin', stdout) = eval morecmd (lefttape, x, righttape) stdin
eval (cmd : morecmd) (lefttape, x, righttape) stdin
     | Incr == cmd = eval morecmd (lefttape, chr (ord x + 1), righttape) stdin
     | Decr == cmd = eval morecmd (lefttape, chr (ord x - 1), righttape) stdin






