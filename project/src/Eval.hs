module Eval
  ( evalExpr
  , evalSheet
  ) where

import qualified Data.Map as Map
import Types
import Graph (expandRange, findCyclesAndOrder)

evalExpr :: Env -> Expr -> Value
evalExpr _ (LitE val) = val
evalExpr env (Ref addr') = case Map.lookup addr' env of
  Nothing -> ErrV ("Missing reference: " ++ show addr')
  Just val -> val
evalExpr env (BinOp op e1 e2) =
  case (evalExpr env e1, evalExpr env e2) of
    (ErrV err, _) -> ErrV err
    (_, ErrV err) -> ErrV err
    (NumV d1, NumV d2) ->
      case op of
        Add -> NumV (d1 + d2)
        Sub -> NumV (d1 - d2)
        Mul -> NumV (d1 * d2)
        Div -> if d2 == 0
                 then ErrV "#DIV/0!"
                 else NumV (d1 / d2)
    _ -> ErrV "#VALUE!"
evalExpr env (RangeOp op start end) =
  let addrs = expandRange start end
      vals = [ Map.lookup a env | a <- addrs ]
      errors = [ err | Just (ErrV err) <- vals ]
  in case errors of
       (err:_) -> ErrV err
       [] ->
         let nums = [ d | Just (NumV d) <- vals ]
         in case op of
              SumR -> NumV (sum nums)
              AvgR -> if null nums
                        then ErrV "#DIV/0!"
                        else NumV (sum nums / fromIntegral (length nums))

evalSheet :: Sheet -> Env
evalSheet sheet = finalEnv
  where
    (acyclicOrder, cycles) = findCyclesAndOrder sheet
    
    allCycleAddrs = concat cycles
    initialEnv = Map.fromList [ (addr', ErrV "cycle") | addr' <- allCycleAddrs ]

    finalEnv = foldl evalCell initialEnv acyclicOrder

    evalCell :: Env -> Addr -> Env
    evalCell currentEnv addr' =
      case Map.lookup addr' (unSheet sheet) of
        Nothing -> currentEnv
        Just cell ->
          let val = case content cell of
                      Lit v -> v
                      Form expr -> evalExpr currentEnv expr
          in Map.insert addr' val currentEnv
