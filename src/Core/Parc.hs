-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Core.Parc ( parcCore ) where

import qualified Lib.Trace
import Control.Applicative hiding (empty)
import Control.Monad
import Data.List ( intersperse, partition )
import Data.Maybe ( catMaybes )
import Data.Char
-- import Data.Maybe
-- import Data.Monoid ( mappend )
import qualified Data.Set as S

import Kind.Kind
import Kind.Newtypes
import Type.Type
import Type.TypeVar
import Type.Kind( getKind )
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.Name
import Common.Range
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty
import Core.CoreVar

trace s x =
  Lib.Trace.trace s
    x

--------------------------------------------------------------------------
-- Reference count transformation
--------------------------------------------------------------------------

parcCore :: Pretty.Env -> Newtypes -> Int -> Core -> (Core,Int)
parcCore penv newtypes u core
  = let (defs',u1) = runParc penv newtypes u (parcDefGroups (coreProgDefs core))
    in (core{ coreProgDefs  = defs' }, u1)

{--------------------------------------------------------------------------
  definition groups
--------------------------------------------------------------------------}

parcDefGroups :: DefGroups -> Parc DefGroups
parcDefGroups defGroups
  = do traceDoc (\penv -> text "parcDefGroups")
       return defGroups


{-
val x = y
f(g(x),y,x)
~>

f(g(dup(x)),y,x) | {x,y,f}

~>

f(g(x),y,dup(x)) | {x,y,f}

fun foo(x) {
  val f = (dup(x); allocate_fun(x)) + fun(y | x){ x + y }
  if (x==1) then 2 else f(3) + f(5)
}


parcExpr :: Expr -> Parc Expr
parcExpr (App fun args)
  = do args' <- reverseMapM parcExpr args
       fun'  <- parcExpr fun
       return (App fun' args')
       
parcExpr (Lam pars body)
  = do body' <- withNoUse $ parcExpr body
       -- for each par in pars, if inuse = ok, otherwise drop
       -- remove all pars from "inuse"
       
parcExpr (Let dgs body)
  = do body' <- parcExpr body
       dgs'  <- parcDefGroups dgs
       return (Let dgs' body')

parcExpr expr@(Con cname info)   
  = do if availableReuse then allocReuse else ..
    
parcExpr expr@(Var vname InfoNone)   -- InfoArity, InfoExternal
  = do inuse <- getInUse(vname)
       if (inuse) 
        then return expr  -- dup it
        else do addInUse(vname)
                return expr 
       
parcExpr expr  
  = return expr

-}

-- Generate a "drop match" 
genDropMatch :: TName -> [TName] -> [TName] -> Parc Expr
genDropMatch con dups drops
  = do xdrops <- mapM genDrop drops
       xdups  <- mapM genDup dups
       cdrop  <- genDrop con
       return $ makeIfExpr (genIsUnique con)
                  (makeStats (catMaybes xdrops ++ [genFree con]))
                  (makeStats (catMaybes (xdups ++ [cdrop])))

genKeepMatch :: TName -> [TName] -> [TName] -> Parc Expr
genKeepMatch con dups drops
  = do xdups  <- mapM genDup dups
       cdrop  <- genDrop con
       return $ makeStats (catMaybes (xdups ++ [cdrop]))

-- Generate a "reuse match" 
genReuseMatch :: TName -> [TName] -> [TName] -> Parc Expr
genReuseMatch con dups drops
 = do xdrops <- mapM genDrop drops
      xdups  <- mapM genDup dups
      cdrop  <- genDrop con
      return $ makeIfExpr (genIsUnique con)
                 (makeStats (catMaybes xdrops ++ [genReuse con]))
                 (makeStats (catMaybes (xdups ++ [cdrop]) ++ [genNoReuse]))



makeStats :: [Expr] -> Expr
makeStats []
  = failure "Core.Parc.makeStats: no expressions"
makeStats exprs
  = Let [DefNonRec (makeDef nameNil expr) | expr <- init exprs]
        (last exprs)
      

makeDef :: Name -> Expr -> Def
makeDef name expr
  = Def name (typeOf expr) expr Private DefVal InlineNever rangeNull ""
  
-- Generate a test if a (locally bound) name is unique
genIsUnique :: TName -> Expr
genIsUnique tname
  = App (Var (TName nameIsUnique funTp) (InfoExternal [(C, "constructur_is_unique(#1)")]))
        [Var tname InfoNone]
  where 
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeBool


-- Generate a free of a constructor
genFree :: TName -> Expr
genFree tname
  = App (Var (TName nameFree funTp) (InfoExternal [(C, "constructor_free(#1)")]))
        [Var tname InfoNone]
  where 
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeUnit

-- Generate a reuse of a constructor
genReuse :: TName -> Expr
genReuse tname
  = App (Var (TName nameReuse funTp) (InfoExternal [(C, "constructor_reuse(#1)")]))
        [Var tname InfoNone]
  where 
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeReuse


-- Generate a reuse of a constructor
genNoReuse :: Expr
genNoReuse 
  = App (Var (TName nameNoReuse funTp) (InfoArity 0 0)) []
  where
    funTp = TFun [] typeTotal typeReuse

genDup  tname = genDupDrop True tname
genDrop tname = genDupDrop False tname

-- Generate a dup/drop over a given (locally bound) name
-- May return Nothing if the type never needs a dup/drop (like an `int` or `bool`)
genDupDrop :: Bool -> TName -> Parc (Maybe Expr)
genDupDrop isDup tname 
  = do let tp = typeOf tname
       (dataDef,dataRepr) <- getDataDefRepr tp
       case dataDef of
         DataDefValue _ 0 -> return Nothing    -- no need to dup/drop a value type with no pointer fields (like int)
         _ -> return (Just (App (dupDropFun isDup tp) [Var tname InfoNone]))

  
dupFun tp  = dupDropFun True tp 
dropFun tp = dupDropFun False tp

dupDropFun isDup tp  
  = Var (TName name (coerceTp )) (InfoExternal [(C, (if isDup then "dup" else "drop") ++ "(#1)")])    
  where 
    name = if isDup then nameDup else nameDrop
    coerceTp = TFun [(nameNil,tp)] typeTotal (if (isDup) then tp else typeUnit)

    
{--------------------------------------------------------------------------
 Parc monad
--------------------------------------------------------------------------}
newtype Parc a = Parc (Env -> State -> Result a)

data Env = Env{ currentDef :: [Def],
                prettyEnv :: Pretty.Env,
                newtypes  :: Newtypes 
              }

data State = State{ uniq :: Int }

data Result a = Ok a State

runParc :: Pretty.Env -> Newtypes -> Int -> Parc a -> (a,Int)
runParc penv newtypes u (Parc c)
 = case c (Env [] penv newtypes) (State u) of
     Ok x st -> (x,uniq st)     

instance Functor Parc where
 fmap f (Parc c)  = Parc (\env st -> case c env st of
                                       Ok x st' -> Ok (f x) st')

instance Applicative Parc where
 pure  = return
 (<*>) = ap

instance Monad Parc where
 return x       = Parc (\env st -> Ok x st)
 (Parc c) >>= f = Parc (\env st -> case c env st of
                                     Ok x st' -> case f x of
                                                   Parc d -> case d env st' of
                                                               Ok x' st'' -> Ok x' st'')

instance HasUnique Parc where
 updateUnique f = Parc (\env st -> Ok (uniq st) st{ uniq = (f (uniq st)) })
 setUnique  i   = Parc (\env st -> Ok () st{ uniq = i})

withEnv :: (Env -> Env) -> Parc a -> Parc a
withEnv f (Parc c)
 = Parc (\env st -> c (f env) st)

getEnv :: Parc Env
getEnv
 = Parc (\env st -> Ok env st)

updateSt :: (State -> State) -> Parc State
updateSt f
 = Parc (\env st -> Ok st (f st))


withCurrentDef :: Def -> Parc a -> Parc a
withCurrentDef def action
 = -- trace ("Parcing: " ++ show (defName def)) $
   withEnv (\env -> env{currentDef = def:currentDef env}) $
   action

traceDoc :: (Pretty.Env -> Doc) -> Parc ()
traceDoc f
 = do env <- getEnv
      parcTrace (show (f (prettyEnv env)))

parcTrace :: String -> Parc ()
parcTrace msg
 = do env <- getEnv
      trace ("Core.Parc: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()


getNewtypes :: Parc Newtypes
getNewtypes
  = do env <- getEnv
       return (newtypes env)


getDataDefRepr :: Type -> Parc (DataDef,DataRepr)
getDataDefRepr tp
  = case extractDataDefType tp of
      Nothing -> return (DataDefNormal,DataNormal)
      Just name -> do newtypes <- getNewtypes
                      case newtypesLookupAny name newtypes of
                        Nothing -> failure $ "Core.Parc.getDataInfo: cannot find type: " ++ show name
                        Just di -> return (dataInfoDef di, fst (getDataRepr di))

extractDataDefType tp
  = case expandSyn tp of
      TApp t _      -> extractDataDefType t
      TForall _ _ t -> extractDataDefType t
      TCon tc       -> Just (typeConName tc)
      _             -> Nothing
