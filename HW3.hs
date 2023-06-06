{-|
Module       : HW3
Description  : Introduction to Haskell
Maintainer   : CS 131, Programming Languages
-}
module HW3 where

--------------------------------------------------------------------------------  
-- Lists
--------------------------------------------------------------------------------  


-- | Returns every other element of a list, starting with the first (zero-th)
evens :: [a] -> [a]
evens [] = []
evens [e] = [e]
evens (h0 : h1 : t) = h0 : evens t

-- | Returns every other element of a list, starting with the second (one-th)
odds :: [a] -> [a]
odds [] = []
odds (h : t) = evens t

-- | Partitions a list of elements into a tuple of two lists, where the first
--   item in the tuple is a list of the elements at even-numbered indices and
--   the second item in the tuple is a list of the elements at odd-numbered 
--   indices
evenodds :: [a] -> ([a], [a])
evenodds []        = ([], [])
evenodds [h]       = ([h], [])
evenodds (h0:h1:t) = (h0:restEvens, h1:restOdds)
    where (restEvens, restOdds) = evenodds t

-- | The inverse of 'evenodds'
riffle :: ([a], [a]) -> [a]
riffle ([], l2) = l2
riffle (l1, []) = l1
riffle (h1:t1, h2:t2) = [h1, h2] ++ riffle (t1, t2)

--------------------------------------------------------------------------------
-- Higher-order functions, currying and uncurrying
-- 
-- Functions curry and uncurry are already pre-defined in Haskell,
-- which is why you need to call your version 'myCurry' and 'myUncurry'.
--------------------------------------------------------------------------------


-- | Converts an uncurried function to a curried function
myCurry :: ( (a,b) -> c ) -> ( a -> b -> c )
myCurry f a b = f (a,b)


-- | Converts a curried function to an uncurried function
myUncurry :: ( a -> b -> c ) -> ( (a,b) -> c )
myUncurry f (a, b) = f a b


-- | A curried version of 'riffle'
riffle2 :: [a] -> [a] -> [a]
riffle2 = myCurry riffle


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

-- | Binary tree of Ints
-- A tree can be empty, or in can have a root and two subtrees (left and right)
data TreeOfInt = Empty
               | Branch Int TreeOfInt TreeOfInt
    deriving (Eq, Show)


-- | Returns the minimum element of the tree. Gives an error if the tree is empty
least :: TreeOfInt -> Int
least Empty = undefined
least (Branch n Empty _) = n
least (Branch _ left _) = least left

--------------------------------------------------------------------------------
-- Stack machines
--------------------------------------------------------------------------------

-- | Arithmetic expressions
data Expr = Num   Double        -- ^ Represents a floating-point value
          | BinOp Expr Op Expr  -- ^ Represents a binary operation
    deriving (Show, Eq)


-- | Binary operators
data Op = PlusOp | MinusOp | TimesOp | DivOp
    deriving (Show, Eq)


-- | Stack instructions
data StackInstr = Push Double  -- ^ Push a number on the stack
                | DoOp Op      -- ^ Perform an operation, using the top two stack values
                | Swap         -- ^ Swap the top two stack values
    deriving (Show, Eq)


-- A stack is represented as a list of floating-point numbers;
-- the head of the list is the top of the stack.
type StackValue = Double
type Stack = [StackValue]


-- | Evaluate a list of stack instructions, given an initial stack
evalRPN :: [StackInstr] -> Stack -> StackValue

--    If there are no more instructions, the result is the top stack value 
evalRPN [] (top : _) = top

-- If the next instruction is a push, push the value on the stack and evaluate
-- the rest of the instructions
evalRPN (Push d : instrs) stack = evalRPN instrs (d : stack)

-- If the next instruction is an operation, perform the operation on the top 
-- two elements on the stack, push the result on the stack, and evaluate the
-- rest of the instructions.
--
-- (The first argument to the operation is the _second_ element on the stack;
--  the second argument is the _first_ element on the stack.)
evalRPN (DoOp op : instrs) (arg2 : arg1 : stack) = evalRPN instrs (value : stack)
    where value = case op of PlusOp  -> arg1 + arg2
                             MinusOp -> arg1 - arg2
                             TimesOp -> arg1 * arg2
                             DivOp   -> arg1 / arg2

-- If the next instruction is a swap, swap the top two values on the stack, 
-- and evaluate the rest of the instructions.
evalRPN (Swap : instrs) (arg2 : arg1 : stack) = evalRPN instrs (arg1 : arg2 : stack)


-- | Translate an expression to stack operations
toRPN :: Expr -> [StackInstr]
toRPN (Num d) = [Push d]
toRPN (BinOp left op right) = toRPN left ++ toRPN right ++ [DoOp op]


-- | Minimize the stack depth
toRPNopt :: Expr -> ([StackInstr], Integer)

-- Optimal RPN for all numbers is the same: 
--   + Push the number on the stack
--   + The depth is 1
toRPNopt (Num n) = ([Push n], 1)

-- Optimal RPN for a binary operation:
toRPNopt (BinOp left op right) 
--  Figure out the instructions & depths of left and right sides
    =  let (lInstructions, lDepth) = toRPNopt left
           (rInstructions, rDepth) = toRPNopt right 
        in 
--    If the depths are the same:
--      + The order doesn't matter (so we'll do left first, to avoid the swap)
--      + The depth is 1 (to store the result of the left side), plus
--        the depth of the left side (or, equivalently, the right side)
          if lDepth == rDepth
          then (lInstructions ++ rInstructions ++ [DoOp op], 1 + lDepth)
--  Otherwise, do the most-expensive side first and return the larger depth.
--  If the left side has a larger depth:
--    + Do the left side first, followed by the right side.
--    + The depth is the left side's depth.
          else if lDepth > rDepth
              then (lInstructions ++ rInstructions ++ [DoOp op], lDepth)
--  If the right side has a larger depth:
--    + Do the right side first, followed by the left side, followed by a swap
--    + The depth is the right side's depth.
          else (rInstructions ++ lInstructions ++ [Swap, DoOp op], rDepth)

--------------------------------------------------------------------------------
-- Example expressions. Define these as described in the assignment.
--------------------------------------------------------------------------------

depth3 :: Expr
depth3 = BinOp depth2 PlusOp depth2
  where depth2 = BinOp (Num 1) PlusOp (Num 1)

depth4 :: Expr
depth4 = BinOp depth3 PlusOp depth3
