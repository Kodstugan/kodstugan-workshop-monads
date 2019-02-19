import Prelude hiding (Maybe, Nothing, Just, Writer)


data List a = Head a (List a) | Empty

instance Functor List where
  fmap f (Head x xs) = Head (f x) (fmap f xs)
  fmap f (Empty) = Empty


-- Let's create the Writer Monad!
data Writer a = Writer a String

instance Functor Writer where
  fmap f (Writer value log) = undefined

instance Applicative Writer where
  pure value = Writer value ""
  (Writer f log1) <*> (Writer value log2) = undefined

instance Monad Writer where
  return = pure
  (Writer value log) >>= func = undefined


-- Log some math with the writer!
-- add4 and sub2 both have the type: Int -> Writer Int
-- See the symmetry with: a -> M a
-- Since writer is a monad, this means that we can use bind (>>=) to chain writer producing functions!

add4 :: Int -> Writer Int
add4 x = Writer (x + 4) ("Added: " ++ show x ++ " + 4")

sub2 :: Int -> Writer Int
sub2 x = Writer (x - 2) ("Subtracted: " ++ show x ++ " - 2")

logMath :: Writer Int
logMath = (return 0) >>=            -- Create a new writer with value 0 and empty log by using return
            (\r1 -> add4 r1 >>=     -- Bind the add4 function
              (\r2 -> sub2 r2 >>=   -- Bind the sub2 function twice
                (\r3 -> sub2 r3)))  -- What will the final result be? What will the log contain?


-- Can you rewrite the above logMath function using do notation?
logMathDo :: Writer Int
logMathDo = do
  r1 <- return 0
  r2 <- undefined
  r3 <- undefined
  r4 <- undefined
  return r4
  
-- Let's introduce another function, "logComment" that just writes to the log but doesn't do
-- any math or give us anything useful back.
-- (), or "unit" as it is called is the "empty" trash type. Because haskell is "pure" a function always
-- have to return SOMETHING even when it doesnt want to or dont have anything useful to return

logComment :: String -> Writer ()
logComment comment = Writer () comment

-- How can we use logComment togheter with our math?
logMathComment :: Writer Int
logMathComment = (return 0) >>=
                 (\r1 -> add4 r1 >>=
                   (\r2 -> sub2 r2 >>=
                     (\r3 -> logComment "Halfway there!" >>=
                       (\_ -> sub2 r3 >>=
                         (\r4 -> logComment "We're back at 0!" >>=
                           (\_ -> return r4))))))

-- We can still reach r3 even if we ignore the result of logComment, but perhaps you can see why
-- "do-notation" makes chaining monadic functions easier

logMathCommentDo :: Writer Int
logMathCommentDo = do
  -- Do some math like above, add 4, sub 2 or do you own thing!
  logComment "Hafway there!"
  -- Keep subtracting until we're back at 0
  logComment "We're back at 0!"
  -- Make sure to return something useful, if logComment is the last function we will return ()!
  return undefined



  
-- This time, lets do IO and write/read to the console instead of just writing to a log!

-- putStrLn :: String -> IO ()
-- getLine :: IO String

-- Say hello and IO
sayHello :: IO ()
sayHello =
  putStrLn "Hello!" >>=
  (\_ -> putStrLn "What is your name?" >>=
    (\_ -> getLine >>=
      (\name -> putStrLn ("Nice to meet you, " ++ name ++ "!") >>=
        (\_ -> putStrLn "Goodbye!"))))


-- Rewrite the sayHello function using do notation!
sayHelloDo = undefined



{- BONUS - (>>) operator:
Sometimes it is sexier to skip do notation and write small chains with (>>=) but it is cumbersome to do a lot of (\_ -> f) anonymous functions when we don't care about the result. Instead we can use the (>>) "sequence" function that simply ignores the "value" of the left monad but also makes sure to compose the monadic "effects" with the right monad.

(>>) :: M a -> M b -> M b

Rewrite the sayHello function using (>>) when we want to ignore the result of the previous computation (like putstrln's "()") and (>>=) when we care about the result.

Example: putStrln "Do you like ice cream?" >> getLine >>= (\ans -> putStrln "you answered: " ++ ans)
-}

sayHelloSequence = undefined


{- The Maybe Monad
Now we have shown how we can use monads to hide the "side effect" of writing to a log using the Writer as well as read/writing from the terminal using IO. 

In the Writer monad we compose, and "chain" the side effects by appending the logs to eachother
In the IO monad, GHC/I is responsible for handling the side effects, we just have to sequence our computations properly.
In the Maybe monad, the side effect is that our functions at any point can return "Nothing", how do we compose functions and values that could be nonexistent?

Figuring out how Maybe should compose the side effect of failure (I.E. that things can be Nothing) is slightly trickier than what you have dealt with so far, but we have already supplied you with the different possible cases to give you some guidance.
-}



-- Let's create the Maybe Monad!
data Maybe a = Just a | Nothing

-- fmap :: (a -> b) -> Maybe a -> Maybe b
instance Functor Maybe where
  fmap f (Nothing) = undefined
  fmap f (Just a) = undefined

-- pure :: a -> Maybe a
-- <*> :: Maybe (a -> b) -> Maybe a -> Maybe b
instance Applicative Maybe where
  pure value = undefined
  (Just func) <*> (Just value) = undefined
  (Nothing) <*> (Just value) = undefined
  (Just func) <*> (Nothing) = undefined
  (Nothing) <*> (Nothing) = undefined

-- return :: a -> Maybe a
-- return is thus equals to 'pure'
-- >>= :: Maybe a -> (a -> Maybe b) -> Maybe b
instance Monad Maybe where
  return = pure
  (Just value) >>= func = undefined
  (Nothing) >>= func = undefined


