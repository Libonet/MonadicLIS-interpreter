module Eval3
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

initTrace :: Trace
initTrace = ""

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
newtype StateErrorTrace a =
  SET { runSET :: Env -> Trace -> Either Error (Pair a (Pair Env Trace)) }

instance Monad StateErrorTrace where
  return x = SET (\env t -> return (x :!: (env :!: t)))
  m >>= f = SET (\env t -> do (x :!: (env' :!: t')) <- runSET m env t
                              runSET (f x) env' t')

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs


-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  track str = SET (\env t -> return (() :!: (env :!: t ++ str)))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = SET (\env t -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = SET (\env t -> lookfor' v env t)
    where lookfor' :: Variable -> Env -> Trace -> Either Error (Pair Int (Pair Env Trace))
          lookfor' v s t = case M.lookup v s of
                            Nothing -> Left UndefVar
                            Just x -> Right (x :!: (s :!: t))
  update v i = SET (\env t -> return (() :!: (M.insert v i env :!: t)))

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval c = do (() :!: (env :!: t)) <- runSET (stepCommStar c) initEnv initTrace
            return (env, t)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v exp) = do x <- evalIntExp exp
                          update v x
                          track $ "var " ++ v ++ " = " ++ show x ++ "\n"
                          return Skip
stepComm (Seq Skip c2) = stepComm c2 
stepComm (Seq c1 c2) = do x <- stepComm c1
                          stepComm (Seq x c2)
stepComm (IfThenElse exp c1 c2) = do b <- evalIntExp exp
                                     if b then stepComm c1 else stepComm c2
stepComm (Repeat exp c) = stepComm (Seq c (IfThenElse exp Skip (Repeat exp c)))

-- Evalua una expresion 
evalIntExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
evalIntExp (Const n) = return n
evalIntExp (Var v) = do x <- lookfor v
                        return x
evalIntExp (UMinus e) = do x <- evalIntExp e
                           return (-x)
evalIntExp (Plus e1 e2) = do x <- evalIntExp e1
                             y <- evalIntExp e2
                             return (x + y)
evalIntExp (Minus e1 e2) = do x <- evalIntExp e1
                              y <- evalIntExp e2
                              return (x - y)
evalIntExp (Times e1 e2) = do x <- evalIntExp e1
                              y <- evalIntExp e2
                              return (x * y)
evalIntExp (Div e1 e2) = do x <- evalIntExp e1
                            y <- evalIntExp e2
                            case y of
                              0 -> throw DivByZero
                              _ -> return (x `div` y)
evalIntExp (VarDec v) = do x <- lookfor v
                           x' <- return (x - 1)
                           update v x'
                           track $ "var " ++ v ++ " = " ++ show x' ++ "\n"
                           return x'
evalIntExp (VarInc v) = do x <- lookfor v
                           x' <- return (x + 1)
                           update v x'
                           track $ "var " ++ v ++ " = " ++ show x' ++ "\n"
                           return x'
evalIntExp BTrue = return True
evalIntExp BFalse = return False
evalIntExp (Lt e1 e2) = do x <- evalIntExp e1
                           y <- evalIntExp e2
                           return (x < y)
evalIntExp (Gt e1 e2) = do x <- evalIntExp e1
                           y <- evalIntExp e2
                           return (x > y)
evalIntExp (And e1 e2) = do x <- evalIntExp e1
                            y <- evalIntExp e2
                            return (x && y)
evalIntExp (Or e1 e2) = do x <- evalIntExp e1
                           y <- evalIntExp e2
                           return (x || y)
evalIntExp (Not e) = do x <- evalIntExp e
                        return (not x)                   
evalIntExp (Eq e1 e2) = do x <- evalIntExp e1
                           y <- evalIntExp e2
                           return (x == y)
evalIntExp (NEq e1 e2) = do x <- evalIntExp e1
                            y <- evalIntExp e2
                            return (x /= y)
evalIntExp (EAssgn v e) = do x <- evalIntExp e
                             update v x
                             track $ "var " ++ v ++ " = " ++ show x ++ "\n"
                             return x
evalIntExp (ESeq e1 e2) = do evalIntExp e1
                             evalIntExp e2
