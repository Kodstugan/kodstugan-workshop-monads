import Prelude hiding (Maybe, Nothing, Just, Writer, List)

{- EXCERCISE: The Writer Monad
The Writer monad allows us to compose functions that "write" to a log.
The monad takes care of adding togheter our logs and make sure we write in the right order.
Sadly, the Writer monad is rarely used in practice, but it is one of the simpler monads to implement and can sometimes help when debugging!
We have already written the typeclass definitions required to be a monad, but its up to you to actually implement the "undefined" functions!
-}

{- Let's create the Writer Monad! -}
{-
A Writer is a datatype that holds a value as well as a "log" of entries,
which we represent as a string
-}
data Writer a = Writer a String

instance Functor Writer where
  fmap f (Writer value log) = undefined

instance Applicative Writer where
  pure value = Writer value ""
  (Writer f log1) <*> (Writer value log2) = undefined

instance Monad Writer where
  return = pure
  (Writer value log) >>= func = undefined

{-
Sidenote: 'return' in the monad typeclass is semantically always equal to 'pure' in the applicative typeclass.
But we need to define both for historical reasons... ¯\_(ツ)_/¯
-}

{- Lets log some math operations with the writer!
add4 and sub2 both have the type: Int -> Writer Int
See the symmetry with: a -> M a
Since writer is a monad, this means that we can use bind (>>=) to chain writer producing functions!
-}
add4 :: Int -> Writer Int
add4 x = Writer (x + 4) ("Added: " ++ show x ++ " + 4")

sub2 :: Int -> Writer Int
sub2 x = Writer (x - 2) ("Subtracted: " ++ show x ++ " - 2")

logMath :: Writer Int
logMath = (return 0) >>=            -- Create a new writer with value 0 and empty log by using return
            (\r1 -> add4 r1 >>=     -- Bind the add4 function
              (\r2 -> sub2 r2 >>=   -- Bind the sub2 function twice
                (\r3 -> sub2 r3)))  -- What will the final result be? What will the log contain?


{- EXCERSICE: Can you rewrite the above logMath function using do notation? -}
logMathDo :: Writer Int
logMathDo = do
  r1 <- return 0
  r2 <- undefined
  r3 <- undefined
  r4 <- undefined
  return r4

{-
Let's introduce another function, "tell" that just writes to the log but doesn't do
any math or give us anything useful back. Because haskell is "pure" a function always have to return SOMETHING even when it doesnt want to or dont have anything useful to return. When we find ourselves in such a scenario (which we often do when using monads!) we simply return ().

(), or "unit" as it is called is haskells built in "empty" or "nothing" trash type and it means literally nothing and has no semantic value other than "this function terminated and had to give something back".
-}
tell :: String -> Writer ()
tell comment = Writer () comment

{- How can we use tell togheter with our math? -}
logMathComment :: Writer Int
logMathComment = (return 0) >>=
                 (\r1 -> add4 r1 >>=
                   (\r2 -> sub2 r2 >>=
                     (\r3 -> tell "Halfway there!" >>=
                       (\_ -> sub2 r3 >>=
                         (\r4 -> tell "We're back at 0!" >>=
                           (\_ -> return r4))))))

{-
We can still reach r3 even if we ignore the result of tell, but perhaps you can see why
"do-notation" makes chaining monadic functions easier
-}

{- EXCERCISE: Use tell togheter with "do-notation"! -}
logMathCommentDo :: Writer Int
logMathCommentDo = do
  -- Do some math like above, add 4, sub 2 or do you own thing!
  tell "Hafway there!"
  -- Keep subtracting until we're back at 0
  tell "We're back at 0!"
  -- Make sure to return something useful, if tell is the last function we will return ()!
  return undefined



{- EXCERCISE: The "comment" function
Lets deinfe a comment function that takes a value and a String, writes the string to the log and immediately returns the supplied value. Comment is somewhat useful when we want to annotate a pure function or value with a entry in the log, but its pretty good for practicing thinking in monads!

Note: There are multiple way to define comment,
bonusPoints++ for coolnes if you use >>=
bonusPoints-- for innefficiency if you use >>=     ;)
-}
comment :: a -> String -> Writer a
comment = undefined

-- Example on how comment can make it easier for us
add3 :: Int -> Writer Int
add3 x = comment (x + 3) ("Added: " ++ show x ++ " + 3")

  
{- EXCERCISE: The IO Monad, HELLO WORLD BABY!!
Now when we've got comfortable binding monadic actions using the Writer monad, lets move on to IO
In practice IO works pretty much exactly like the Writer monad we've practiced on, except that we don't have access to the log anymore! We simply have to make use with the built in putStrln. Oh, and also we can read from the log (or, the terminal) using getLine and get some input from actual humans!
-}


{- Say hello and IO

   putStrLn :: String -> IO ()
   getLine :: IO String

-} 
sayHello :: IO ()
sayHello = do
  putStrLn "Hello!"
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Nice to meet you, " ++ name ++ "!")
  putStrLn "Goodbye!"


{- EXCERCISE: Rewrite the sayHello function without using "do-notation"! -} 
sayHelloBind = undefined


{- EXCERCISE: The (>>) operator

Sometimes it is sexier to skip do notation and write small chains with (>>=) but it is cumbersome to do a lot of (\_ -> f) anonymous functions when we don't care about the result. Instead we can use the (>>) "sequence" function that simply ignores the "value" of the left monad but composes the monadic "effects" with the right monad.

(>>) :: M a -> M b -> M b

Rewrite the sayHello function using (>>) when we want to ignore the result of the previous computation (like putstrln's "()") and (>>=) when we care about the result.

Example: putStrln "Do you like ice cream?" >> getLine >>= (\ans -> putStrln "you answered: " ++ ans)
-}

sayHelloSequence = undefined


{- EXCERCISE: The Maybe Monad
Now we have shown how we can use monads to hide the "side effect" of writing to a log using the Writer as well as read/writing from the terminal using IO. 

In the Writer monad we compose, and "chain" the side effects by appending the logs to eachother
In the IO monad, GHC/I is responsible for handling the side effects, we just have to sequence our computations properly.
In the Maybe monad, the side effect is that our functions at any point can return "Nothing", how do we compose functions and values that could be nonexistent?

Figuring out how Maybe should compose the side effect of failure (I.E. that things can be Nothing) is slightly trickier than what you have dealt with so far. Thankfully we've already supplied you with the different cases to give you some guidance, you just have to fill in the blanks!
-}


{- Let's re-create the Maybe Monad! -}

data Maybe a = Just a | Nothing

{- What happens if we apply a function to Nothing? -}
instance Functor Maybe where
--fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f (Nothing) = undefined
  fmap f (Just a) = undefined


{- How can we apply a Maybe function to a Maybe value? -}
instance Applicative Maybe where
--pure :: a -> Maybe a
  pure value = undefined
-- <*> :: Maybe (a -> b) -> Maybe a -> Maybe b
  (Just func) <*> (Just value) = undefined
  (Nothing) <*> (Just value) = undefined
  (Just func) <*> (Nothing) = undefined
  (Nothing) <*> (Nothing) = undefined


{- How do we compose functions that may return Nothing? -}
instance Monad Maybe where
--return :: a -> Maybe a
  return = pure
-- >>= :: Maybe a -> (a -> Maybe b) -> Maybe b  
  (Just value) >>= func = undefined
  (Nothing) >>= func = undefined


{- BONUS: The State Monad
In the Writer monad we carried around a log we could write to using either tell or comment.
In IO, we could write to the terminal using putStrLn, but also read by getLine.

Now you might have thought, what if we could read from the log in writer too?
The fact is that there is already a monad that works like that, the State monad.

There is also a Reader monad, which is pretty much the opposite of Writer. It is said that if you combine Reader and Writer you end up with State, which is pretty elegant if you ask me.
Im not going to get too deep into the State monad because this workshop doc is already long enough and it is getting late, but I encourage you too read up on it. It is very useful since it allows us to fake mutable values in haskell!
-}



{- BONUS: Monad knowledge checklist to read up on in increasing order of difficulty/usefulness

Maybe Monad [X]
Writer Monad [X]
IO Monad [X]
List Monad []
Reader Monad []
State Monad []
Monad Transformers []
Either/Except Monad []
Free Monad []
Tardis Monad []
-}


{- BONUS: Monoid in the wild!
In our Writer implementation, we have hardcoded the "log" to be of type String, but if you've paid attention, you would have noticed that we only really have to be able to "add" and "compose" our log, as well as sometimes returning an empty log. Theese "empty" and "add" functionality can be further abstracted into the Monoid typeclass, which looks like this:

class Monoid where
  mappend :: o -> o -> o
  mempty :: o

Since String is a list of characters, and lists are monoids..
Instance List Moinoid where
  mappend = (++)
  mempty = []

It would be sufficient for the Writer monad to represent the log using any type, as longs as that type is a monoid! This would allow us to log not only strings, but also integers (that form a monoid with "+" and 0" ), thus transforming our text writing Writer into a number counting one! Or simply keep a log in the form of a list containing any type we can think of!

In fact, the "real" standard Writer Monad implementation has a monoid constraint on its type class instances and looks something like this:

data w a Writer a w

instance (Monoid w) => Functor Writer a w where
...
...


Hopefully this goes to show that by realizing that we only use the monoid characteristics of the String as our log type, limiting ourselves to Strings is unneccessary when we can replace the String with any monoid. By saying that the Writers can be of any type as long as it is a monoid, we can use the Writer monad to log both Strings, Integers, Natural numbers, Lists of any type, pretty much anything, as long as we can form a monoid with it!

-}


