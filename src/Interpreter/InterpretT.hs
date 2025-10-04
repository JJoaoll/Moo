module Interpreter.InterpretT where

import Interpreter.Context

import Control.Monad.State
import Control.Monad.Except

import Utils

data InterpretError 
  = IError String 
  | IError2
  deriving (Eq, Show)


-- IORef when?
type InterpretT = StateT Context (ExceptT InterpretError IO)


-- AI sugestion
-- Implementação com IORef para performance e sharing
-- data Value 
--   = VStr   !Text                              -- Primitivos ainda imutáveis | VInt   {-# UNPACK #-} !Int               
--   | VChar  {-# UNPACK #-} !Char              
--   | VFloat {-# UNPACK #-} !Float             
--   | VRef   !(IORef Value)                     -- Ponteiro para valor mutável
--   | VList  !(IORef [IORef Value])             -- Lista de ponteiros (sharing)
--   | VData  !Name !(IORef [IORef Value])       -- Constructor com campos mutáveis
--   | VThunk !(IORef (Maybe Value)) (IO Value)  -- Lazy evaluation com cache

-- -- Instância manual de Eq (não pode derivar devido ao IO Value)
-- instance Eq Value where
--   (VStr a) == (VStr b) = a == b
--   (VInt a) == (VInt b) = a == b
--   (VChar a) == (VChar b) = a == b
--   (VFloat a) == (VFloat b) = a == b
--   (VRef a) == (VRef b) = a == b  -- Comparação de ponteiros
--   (VList a) == (VList b) = a == b
--   (VData n1 a) == (VData n2 b) = n1 == n2 && a == b
--   (VThunk a _) == (VThunk b _) = a == b  -- Ignora computação
--   _ == _ = False

-- -- Função/closure
-- data Function = Function
--   { funName :: Name
--   , funBody :: [Value] -> InterpretT Value
--   }

-- instance Eq Function where
--   (Function n1 _) == (Function n2 _) = n1 == n2

-- type InterpretT = StateT Context (ExceptT InterpretError IO)

-- -- Funções auxiliares para trabalhar com IORef Values

-- -- Criar um valor mutável
-- newValue :: Value -> IO Value
-- newValue val = VRef <$> newIORef val

-- -- Ler valor (resolve ponteiros automaticamente)
-- readValue :: Value -> IO Value
-- readValue (VRef ref) = readIORef ref >>= readValue  -- Resolve chains
-- readValue (VThunk cache comp) = do
--   cached <- readIORef cache
--   case cached of
--     Just val -> return val
--     Nothing -> do
--       val <- comp
--       writeIORef cache (Just val)
--       return val
-- readValue val = return val
  

-- -- Escrever valor (apenas para refs)
-- writeValue :: Value -> Value -> IO ()
-- writeValue (VRef ref) val = writeIORef ref val
-- writeValue _ _ = error "Cannot write to immutable value"

-- -- Criar lista compartilhada
-- newList :: [Value] -> IO Value
-- newList vals = do
--   refs <- mapM (\v -> newIORef v) vals
--   listRef <- newIORef refs
--   return $ VList listRef

-- -- Adicionar à lista (mutação in-place)
-- appendList :: Value -> Value -> IO ()
-- appendList (VList listRef) val = do
--   refs <- readIORef listRef
--   newRef <- newIORef val
--   writeIORef listRef (refs ++ [newRef])
-- appendList _ _ = error "Not a list"

-- -- Criar constructor com campos mutáveis
-- newData :: Name -> [Value] -> IO Value
-- newData name vals = do
--   refs <- mapM (\v -> newIORef v) vals
--   dataRef <- newIORef refs
--   return $ VData name dataRef

-- -- Atualizar campo do constructor
-- updateDataField :: Value -> Int -> Value -> IO ()
-- updateDataField (VData _ dataRef) idx newVal = do
--   refs <- readIORef dataRef
--   if idx < length refs
--     then writeIORef (refs !! idx) newVal
--     else error "Index out of bounds"
-- updateDataField _ _ _ = error "Not a data constructor"

-- -- Criar thunk para lazy evaluation
-- newThunk :: IO Value -> IO Value
-- newThunk computation = do
--   cache <- newIORef Nothing
--   return $ VThunk cache computation

-- -- Exemplo de uso no interpretador
-- evaluate :: Expr -> InterpretT Value
-- evaluate expr = do
--   ctx <- get
--   case expr of
--     -- Avaliação normal para primitivos
--     ELit (LInt n) -> return $ VInt n
--     ELit (LChar c) -> return $ VChar c
    
--     -- Variáveis retornam ponteiros (sharing automático)
--     EVar name -> do
--       case HM.lookup name (ctxVars ctx) of
--         Just ref -> return $ VRef ref
--         Nothing -> throwError $ IError $ "Variable not found: " ++ show name
    
--     -- Listas usam estrutura mutável
--     -- EList exprs -> do
--     --   vals <- mapM evaluate exprs
--     --   liftIO $ newList vals
    
--     _ -> throwError $ IError "Not implemented"

-- -- Operações otimizadas para listas
-- -- Exemplo: concatenação sem copiar elementos
-- concatLists :: Value -> Value -> IO Value
-- concatLists (VList ref1) (VList ref2) = do
--   refs1 <- readIORef ref1
--   refs2 <- readIORef ref2
--   newListRef <- newIORef (refs1 ++ refs2)  -- Só copia ponteiros!
--   return $ VList newListRef
-- concatLists _ _ = error "Cannot concat non-lists"
