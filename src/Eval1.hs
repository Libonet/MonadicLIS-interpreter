module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'


-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v exp) = do x <- evalExp exp
                          update v x
stepComm (Seq Skip c2) = stepComm c2 
stepComm (Seq c1 c2) = do x <- stepComm c1
                          stepComm (Seq x c2)
stepComm (IfThenElse exp c1 c2) = do b <- evalExp exp
                                     if b then stepComm c1 else stepComm c2
stepComm (Repeat exp) c = stepComm (Seq c (IfThenElse exp (Repeat exp c) Skip))
                        

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const n) = return n
evalExp (Var v) = lookfor v
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
                         return (x / y)
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
                     return (not e)                   
evalExp (Eq e1 e2) = do x <- evalExp e1
                        y <- evalExp e2
                        return (x == y)
evalExp (NEq e1 e2) = do x <- evalExp e1
                         y <- evalExp e2
                         return (x /= y)
evalExp (EAssgn v e) = 
evalExp (ESeq e1 e2) = 





{-
data Exp a where
  -- Int
  Const ::Int -> Exp Int
  Var ::Variable -> Exp Int
  UMinus ::Exp Int -> Exp Int
  Plus ::Exp Int -> Exp Int -> Exp Int
  Minus ::Exp Int -> Exp Int -> Exp Int
  Times ::Exp Int -> Exp Int -> Exp Int
  Div ::Exp Int -> Exp Int -> Exp Int
  -- Bool
  BTrue ::Exp Bool
  BFalse ::Exp Bool
  Lt ::Exp Int -> Exp Int -> Exp Bool
  Gt ::Exp Int -> Exp Int -> Exp Bool
  And ::Exp Bool -> Exp Bool -> Exp Bool
  Or ::Exp Bool -> Exp Bool -> Exp Bool
  Not ::Exp Bool -> Exp Bool
  Eq ::Exp Int -> Exp Int -> Exp Bool
  NEq ::Exp Int -> Exp Int -> Exp Bool
  EAssgn ::Variable -> Exp Int -> Exp Int
  ESeq ::Exp Int -> Exp Int -> Exp Int
-}