newtype State s a = State {
  runState :: s -> (a, s)
}

-- Our monad is a function that transforms one state into another,
-- yielding a result when it does so. Because of this, the State monad
-- is sometimes called the state transformer monad.

type SimpleState s a = s -> (a, s)

-- bound the type variable s to String. Here, StringState still has a
-- type parameter a, though. **so** we have a suitable type
-- constructor for a monad.
type StringState a = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

returnAlt :: a -> SimpleState s a
resultAlt a s = (a, s)

-- A definition for (>>=)
-- m == step
-- k == makeStep
-- s == oldState
bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s -> let (a, s') = m s
                   in (k a) s'

bindAlt step makeStep oldState =
  let (result, newState) = step oldState
  in (makeStep result) newState

getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)

returnState :: a -> State s a
returnState = State $ \s -> (a, s)
