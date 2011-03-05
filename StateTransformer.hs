module StateTransformer
    (StateTransformer,runStateTransformer,getState,setState)
where

newtype StateTransformer state a = StateTransformer (state -> (state,a))

instance Monad (StateTransformer state) where
    (StateTransformer transform1) >>= f =
        StateTransformer (\ state1 -> let (state2,a) = transform1 state1
                                          StateTransformer transform2 = f a
                                      in  transform2 state2)
    f >> g = f >>= const g
    return a = StateTransformer (\ state -> (state,a))

instance Functor (StateTransformer state) where
    fmap f st = st >>= (return . f)

getState :: StateTransformer state state
getState = StateTransformer (\ state -> (state,state))

setState :: state -> StateTransformer state ()
setState state = StateTransformer (const (state,()))

runStateTransformer :: StateTransformer state a -> state -> (state,a)
runStateTransformer (StateTransformer transform) state = transform state
