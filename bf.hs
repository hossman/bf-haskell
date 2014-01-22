
type Program = [Cmd]
data Cmd = GoR | GoL | Incr | Decr | Write | Read | Loop [Cmd]
     deriving (Show)

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

-- type Tape = ([Char], Char, [Char])









