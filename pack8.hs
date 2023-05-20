type Variables = (Int, Int, Int)
data MonadVars a = MonadVars { actualComputation :: (Variables -> (a, Variables)) }

putVars :: Variables -> MonadVars ()
putVars neWvars = MonadVars $ \vars -> ((), neWvars)

runComputation :: MonadVars a -> Variables -> (a, Variables)
runComputation computation vars = actualComputation computation $ vars

getVars :: MonadVars Variables
getVars = MonadVars $ \vars -> (vars, vars)


instance Functor MonadVars where
    -- fmap :: (a -> b) -> (MonadVars a) -> (MonadVars b)
    fmap f (MonadVars comp) =
        MonadVars $ \vars -> let (result, newVars) = comp vars in (f result, newVars)

instance Applicative MonadVars where
    pure x = MonadVars $ \vars -> (x, vars)
    (MonadVars compF) <*> (MonadVars compX) = MonadVars $ \vars -> let (f, variables_1) = compF vars
                                                                       (x, variables_2) = compX variables_1
                                                                       in (f x, variables_2)

--вызываю конструкторы с текущими переменными(vars)
--применяю функцию к значению

instance Monad MonadVars where
    (MonadVars comp1) >>= comp2 = MonadVars $ \vars -> let (result1, vars1) = runComputation (MonadVars comp1) vars
                                                           (MonadVars comp2') = comp2 result1
                                                           in runComputation (MonadVars comp2') vars1

--вызываю comp1 с текущими vars
--вызываю comp2 с результатом result1 и vars1, возвращаю result2 и vars2


computation :: MonadVars Int
computation = do
    (x1, x2, x3) <- getVars
    putVars (x1 + 2, x2 + 1, x3 * 3)
    return 200

main :: IO ()
main = do
    print $ runComputation computation (20, 30, 40)