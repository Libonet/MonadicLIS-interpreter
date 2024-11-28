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

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

instance Monad StateError where
  return x = StateError (\env -> return (x :!: env))
  m >>= f = StateError (\env -> do (x :!: env') <- runStateError m env
                                   runStateError (f x) env')

instance MonadError StateError where
  throw e = StateError (\_ -> Left e)

instance MonadState StateError where
  lookfor v = StateError (\env -> lookfor' v env)
    where lookfor' :: Variable -> Env -> Either Error (Pair Int Env)
          lookfor' v s = case M.lookup v s of
                            Nothing -> Left UndefVar
                            Just x -> Right (x :!: s)
  update v i = StateError (\env -> return (() :!: M.insert v i env))

newtype TraceT m a = 
  TraceT { runTT :: Pair (m a) Trace }

type StateErrorTrace = TraceT StateError

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans TraceT where
  lift op = TraceT (op :!: "")

instance Monad m => Monad (TraceT m) where
  return x = TraceT ((return x) :!: "")
  m >>= f = TraceT (let (x :!: t) = runTT m
                        (x' :!: t') = runTT $ (lift x) >>= f
                    in (x' :!: t ++ t'))

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Monad m => Functor (TraceT m) where
  fmap = liftM

instance Monad m => Applicative (TraceT m) where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs


-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  track str = TraceT (return () :!: str)

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = TraceT (throw e :!: "")

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  -- Busca el valor de una variable
  -- lookfor :: Variable -> m Int
  lookfor var = TraceT (lookfor var :!: "")
  -- Cambia el valor de una variable
  -- update :: Variable -> Int -> m ()
  update var num = TraceT (update var num :!: "")

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval = undefined

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
-- stepComm :: [dar el tipo segun corresponda]
stepComm = undefined

-- Evalua una expresion 
-- evalIntExp :: [dar el tipo segun corresponda]
evalExp = undefined
