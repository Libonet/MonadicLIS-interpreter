module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\env -> return (x :!: env))
  m >>= f = StateError (\env -> do (x :!: env') <- runStateError m env
                                   runStateError (f x) env')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError (\_ -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\env -> lookfor' v env)
    where lookfor' :: Variable -> Env -> Either Error (Pair Int Env)
          lookfor' v s = case M.lookup v s of
                            Nothing -> Left UndefVar
                            Just x -> Right (x :!: s)
  update v i = StateError (\env -> return (() :!: M.insert v i env))

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval c = do (() :!: env) <- runStateError (stepCommStar c) initEnv
            return env

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v exp) = do x <- evalExp exp
                          update v x
                          return Skip
stepComm (Seq Skip c2) = stepComm c2 
stepComm (Seq c1 c2) = do x <- stepComm c1
                          stepComm (Seq x c2)
stepComm (IfThenElse exp c1 c2) = do b <- evalExp exp
                                     if b then stepComm c1 else stepComm c2
stepComm (Repeat exp c) = stepComm (Seq c (IfThenElse exp Skip (Repeat exp c)))

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp (Const n) = return n
evalExp (Var v) = do x <- lookfor v
                     return x
evalExp (UMinus e) = do x <- evalExp e
                        return (-x)
evalExp (Plus e1 e2) = do x <- evalExp e1
                          y <- evalExp e2
                          return (x + y)
evalExp (Minus e1 e2) = do x <- evalExp e1
                           y <- evalExp e2
                           return (x - y)
evalExp (Times e1 e2) = do x <- evalExp e1
                           y <- evalExp e2
                           return (x * y)
evalExp (Div e1 e2) = do x <- evalExp e1
                         y <- evalExp e2
                         case y of
                          0 -> throw DivByZero
                          _ -> return (x `div` y)
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt e1 e2) = do x <- evalExp e1
                        y <- evalExp e2
                        return (x < y)
evalExp (Gt e1 e2) = do x <- evalExp e1
                        y <- evalExp e2
                        return (x > y)
evalExp (And e1 e2) = do x <- evalExp e1
                         y <- evalExp e2
                         return (x && y)
evalExp (Or e1 e2) = do x <- evalExp e1
                        y <- evalExp e2
                        return (x || y)
evalExp (Not e) = do x <- evalExp e
                     return (not x)                   
evalExp (Eq e1 e2) = do x <- evalExp e1
                        y <- evalExp e2
                        return (x == y)
evalExp (NEq e1 e2) = do x <- evalExp e1
                         y <- evalExp e2
                         return (x /= y)
evalExp (EAssgn v e) = do x <- evalExp e
                          update v x
                          return x
evalExp (ESeq e1 e2) = do evalExp e1
                          evalExp e2

