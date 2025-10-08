-- Interpreter.Core
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Interpreter.Core where

import Grammar.Expr
import Interpreter.InterpretT
import Interpreter.Context

import Control.Lens hiding (Context, op)

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.List ((!?), find)
import Utils
import Data.Text (Text)
import qualified Data.Text.IO as TIO
-- import qualified System.IO as IO
import Text.Megaparsec (runParser)
import Parser.Expr.Lit (literal)
import Grammar.Sttm
import qualified System.IO as IO
import Grammar.Program

-- todo: join sttms, expr and func here ;-;



-----------------------------BEGIN-FUNC--------------------------------------

refreshScope :: InterpretT ()
refreshScope = do
  ctx <- get
  put $ ctx 
    & cStack .~ [[]]
    & cScope .~ 0

evalFun :: [Value] -> FunDef -> InterpretT Value
evalFun args FunDef{..} = do
  oldCtx <- get
  refreshScope
  args `app` fParams

  mRtrnVal <- execBody body
  case mRtrnVal of
    Nothing -> error $ "The function " ++ show fName ++ " didnt return a thing."
    Just val -> do 
      put oldCtx
      pure val

app :: [Value] -> [Param] -> InterpretT ()
[] `app` [] = pure ()
(arg:args) `app` (Param{..}:params) = do
  addDecl (pName, arg)
  args `app` params
_ `app` _ = undefined

-----------------------------END-FUNC--------------------------------------


-----------------------------BEGIN-STTM--------------------------------------


-- i'll consider that returns provides values
evalSttm :: Sttm -> InterpretT (Maybe Value)
evalSttm (SInit name _typε expr) = do
  val <- evalExpr expr
  addDecl (name, val)
  pure Nothing

  -- ctx <- get
  -- case ctx `withDVar` (name, val) of
  --   Left msg -> error $ show msg -- TODO: treat this
  --   Right ctx' -> do
  --     put ctx'
  --     pure Nothing

-- does the attrib to the closest ref 
evalSttm (SAtrib name expr) = do
  val <- evalExpr expr

  ctx <- get
  case ctx `modifyDVarVal` (name, val) of
    Left msg -> error $ show msg -- TODO: treat this
    Right ctx' -> do
      put ctx'
      pure Nothing

-- TODO: Properly implement it!
evalSttm (SPrint expr) = do
  val <- evalExpr expr
  liftIO $ IO.print val
  pure Nothing


evalSttm (SGtrib name expr) = do -- undefined
  val <- evalExpr expr

  ctx <- get
  case ctx `modifyGlobalVal` (name, val) of
    Left msg -> error $ show msg -- TODO: treat this
    Right ctx' -> do
      put ctx'
      pure Nothing

evalSttm (SFunCall name args) = do
  ctx <- get
  case ctx `findFunDef` name of
    Nothing -> error $ "where is the " ++ show name
    Just funDef -> do
      args' <- mapM evalExpr args
      _ <- evalFun args' funDef
      pure Nothing

evalSttm (SMatch scrutinee cases) = do
  val <- evalExpr scrutinee
  val `execFirstMatchIn` cases

  where 
    _ `execFirstMatchIn` [] = pure Nothing
    val `execFirstMatchIn` ((pattern,body):anotherCases) = do
      enterBlock
      didMatch <- bindPatterns [val] [pattern]
      if not didMatch then do
        quitBlock
        val `execFirstMatchIn` anotherCases
      else do
        result <- execBody body
        quitBlock
        pure result
    
evalSttm (SWhile cond body) = do
  whileLoop
    where 
      whileLoop = do
        bool <- evalExpr cond 
        case bool of
          VFalse -> pure Nothing
          VTrue -> do 
            enterBlock
            result <- execBody body
            quitBlock
            case result of
              Nothing -> whileLoop  -- loop
              Just val -> pure (Just val)  -- break the loop
          _ -> throwError IError2

evalSttm (SFor i is body) = do

  list <- evalExpr is

  -- typε <- ctx `checkType` is
  forLoop $ listoquio list
    where 
      forLoop [] = pure Nothing
      forLoop (val:vals) = do 
        enterBlock
        addDecl (i, val)
        result <- execBody body
        quitBlock
        case result of
          Nothing -> forLoop vals           -- keeps the loop
          Just retVal -> pure $ Just retVal -- breaks the loop and returns


evalSttm (SReturn expr) = do
  val <- evalExpr expr
  pure $ Just val

-- evalSttm _ = undefined


-- no cycling
addDecl :: (Name, Value) -> InterpretT ()
addDecl d@(_name, _val) = do
  ctx <- get
  case ctx `withDVar` d of 
    Right ctx' -> put ctx'
    Left msg -> error $ show msg -- TODO: do this msg


execBody :: [Sttm] -> InterpretT (Maybe Value)
execBody [] = pure Nothing
execBody (stmt:stmts) = do
  mVal <- evalSttm stmt
  case mVal of
    Nothing -> execBody stmts   -- keep the loop
    Just val -> pure (Just val) -- breaks the loop and returns

-- also check?? 
-- ensure that they have the same size!
bindPatterns :: [Value] -> [Pattern] -> InterpretT Bool
bindPatterns [] [] = pure True
bindPatterns (v:vs) (p:ps) = 
  case p of
    PConstructor cName cPatterns -> 
      case v of
        VData vName vArgs 
          | length vArgs /= length cPatterns -> pure False
          | vName /= cName -> pure False
          | otherwise -> do
            innerOk <- bindPatterns vArgs cPatterns
            if innerOk then bindPatterns vs ps else pure False
        _ -> pure False

    PVar name -> do
      addDecl (name, v)
      bindPatterns vs ps
    
    _ -> bindPatterns vs ps

bindPatterns _ _ = pure False


-----------------------------END-STTM--------------------------------------


-----------------------------BEGIN-EXPR--------------------------------------

-- assume everything is well typed!
evalExpr :: Expr -> InterpretT Value
evalExpr (ELit lit) = 
  case lit of 
    LInt a -> pure $ VInt a
    LChar c -> pure $ VChar c
    LFloat x -> pure $ VFloat x
    LConstr name lits ->
      VData name <$>
        forM (fmap ELit lits) evalExpr 

evalExpr (EConstr name args) = 
  VData name <$> forM args evalExpr

evalExpr (EVar name) = do
  ctx <- get
  liftEither $ runExcept (ctx `findVarVal` name)

evalExpr (EGlobal name) = do
  ctx <- get
  case ctx `findGlobal` name of
    Nothing -> undefined
    Just val -> pure val

evalExpr (EUnOp op ex) =
  case op of 
    Neg -> do
      VInt x <- evalExpr ex -- what happens if it couldnt match with VInt?
      pure $ VInt (-x)
    Not -> do
      bool <- evalExpr ex
      case bool of
        VTrue -> pure VFalse
        VFalse -> pure VTrue
        _ -> undefined

evalExpr (EBinOp exL op exR)
  | op `elem` intBinOps = do
    VInt nL <- evalExpr exL
    VInt nR <- evalExpr exR
    pure $ VInt $ case op of
      Add -> nL + nR
      Sub -> nL - nR
      Mul -> nL * nR
      Div -> nL `quot` nR
      Rem -> nL `mod` nR
      _ -> undefined

  | op `elem` floatBinOps = do
    VFloat xL <- evalExpr exL
    VFloat xR <- evalExpr exR
    pure $ VFloat $ case op of
      Add_ -> xL + xR
      Sub_ -> xL - xR
      Mul_ -> xL * xR
      Div_ -> xL / xR
      _ -> undefined

  | op `elem` boolBinOps = do
    bL <- evalExpr exL
    bR <- evalExpr exR
    pure $ case (bL, op, bR) of
      (VTrue, And, VTrue)  -> VTrue
      (VFalse, Or, VFalse) -> VFalse
      (_, And, _)          -> VFalse
      (_, Or, _)           -> VTrue
      _ -> undefined

  | op `elem` listBinOps = do
    ls <- evalExpr exL
    rs <- evalExpr exR
    pure (mooCat ls rs)

  | op `elem` badCompareBinOps = undefined
  | op `elem` bEqOps = undefined

  | otherwise = undefined

evalExpr (EFunCall name args) = do
  ctx <- get
  case ctx `findFunDef` name of
    Nothing -> error $ "where is the " ++ show name
    Just funDef -> do
      args' <- mapM evalExpr args
      evalFun args' funDef

-- u checked, right?
evalExpr (EScan typε) = do
  input <- liftIO TIO.getLine
  case parseLiteral input of
    Nothing -> errorMsg
    Just lit -> do
      ctx <- get
      case ctx `checkLitType` lit of
        Nothing -> errorMsg
        Just litType 
          | litType /= typε -> errorMsg
          | otherwise -> pure (lit2Val lit)

  where
    errorMsg = error $ "wrong input! Expecting something from type: " ++ show typε
    parseLiteral :: Text -> Maybe Lit
    parseLiteral input =
      case runParser literal "" input of
        Right lit -> Just lit
        Left _    -> Nothing

evalExpr (EConst cName) = error $ 
  "Parsing internal problem: "
  ++ "the const " ++ show cName ++ " didnt expand!"

findVarVal :: Context -> Name -> Except InterpretError Value
ctx `findVarVal` name = 
  case (ctx ^. cStack) !? (ctx ^. cScope) of
    Nothing -> undefined
    Just localStack -> 
      case find ((name==) . (^. vName)) localStack of
        Just dcl -> pure $ dcl ^. vVal
        Nothing 
          | ctx ^. cScope == 0 -> undefined 
          | otherwise -> 
              (ctx & cScope %~ subtract 1) `findVarVal` name


findGlobal :: Context -> Name -> Maybe Value
ctx `findGlobal` name =
  ctx ^. cGlobs  
  & find ((name==) . (^. vName))
  & fmap (^. vVal)


-----------------------------END-EXPR--------------------------------------


