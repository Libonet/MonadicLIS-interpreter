-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v exp) = do x <- evalExp exp
                          update v x
                          return Skip
stepComm (Seq Skip c2) = stepComm c2 
stepComm (Seq c1 c2) = do x <- stepComm c1
                          stepComm (Seq x c2)
stepComm (IfThenElse exp c1 c2) = do (b :!: env) <- evalExp exp
                                     if b then stepComm c1 else stepComm c2
stepComm (Repeat exp c) = stepComm (Seq c (IfThenElse exp (Repeat exp c) Skip))

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
evalExp (VarDec v) = do x <- lookfor v
                        (x' :!: env') <- return (x - 1)
                        update v x'
                        return x'
evalExp (VarInc v) = do x <- lookfor v
                        (x' :!: env') <- return (x + 1)
                        update v x'
                        return x'
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