{-# LANGUAGE TupleSections
           , OverloadedStrings
           , ScopedTypeVariables
           , NoMonomorphismRestriction
           , BangPatterns
           , TemplateHaskell
           , LambdaCase
  #-}
module Database.PostgreSQL.Simple.DBmoreTH where

import           Prelude hiding (null)
import           Control.Applicative (Applicative(..), (<$>), (<|>), (*>))
import           Control.Monad (replicateM, replicateM_)
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import Data.Maybe
import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as B
-- import           Data.Vector (Vector)
-- import qualified Data.Vector as V
import           Database.PostgreSQL.Simple.Types (Only(..))
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple.Internal
-- import           Database.PostgreSQL.Simple.Compat
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.Types ((:.)(..), Null)
import           Database.PostgreSQL.Simple.TypeInfo
import Language.Haskell.TH.Syntax
import Language.Haskell.TH

-- instance (FromField a, FromField b) => FromRow (a,b) where fromRow = (,) <$> field <*> field

null :: RowParser Null
null =  field

qr :: Int -> Q [Dec]
qr k = do
  ns <- replicateM k (newName "a")
  let pre = map (\x -> ClassP (''FromField) [VarT x]) ns
  return [ InstanceD pre (loop k ns) [fun ns]
         , InstanceD pre (loop2 k ns) [fun2 ns]
         ]
   where
    loop 0 ns = AppT (TupleT k) (VarT (head ns))
    loop i ns | i < k = AppT (loop (i-1) ns ) (VarT (ns !! i))
              | otherwise = AppT (ConT ''FromRow) (loop (i-1) ns )
    loop2 i ns | i == k = AppT (ConT ''FromRow) (AppT (ConT ''Maybe)(loop (i-1) ns ))
    fun ns  = ValD (VarP 'fromRow)
                   (NormalB $ iterate (sta .) dol !! (k-1) $ ConE (tupleDataName k))
                   -- ConE $ tupleTypeName k)
                   []
    sta x = InfixE (Just x) (VarE '(<*>)) (Just (VarE 'field))
    dol x = InfixE (Just x) (VarE '(<$>)) (Just (VarE 'field))
    fun2 ns = ValD (VarP 'fromRow)
                  (NormalB
                     (inJ
                         (inJ
                             (iterate (sta2  .) sta2  !! (k-1) $ VarE 'null)
                             '(*>)
                             (AppE (VarE 'pure) (ConE 'Nothing)))
                         '(<|>)
                         (inJ
                               (ConE 'Just)
                               '(<$>)
                               (VarE 'fromRow)
                            )))
                     []
    inJ a o b = InfixE (Just a) (VarE o) (Just b)
    sta2 a = inJ a '(*>) (VarE 'null)

--     fun2 ns = VarP ('fromRow) [ Clause [TupP (map VarP ns)] (NormalB $ ListE $ map (AppE (VarE 'field) . VarE) ns ) [] ]

--   [ValD (VarP fromRow) (NormalB (InfixE (Just (InfixE (Just (ConE GHC.Tuple.(,))) (VarE Data.Functor.<$>) (Just (VarE Database.PostgreSQL.Simple.FromRow.field)))) (VarE Control.Applicative.<*>) (Just (VarE field)))) []]]



{-
instance (FromField a, FromField b) => FromRow (a,b) where fromRow = (,) <$> field <*> field
[InstanceD
   [ClassP Database.PostgreSQL.Simple.FromField.FromField [VarT a_0]
   ,ClassP Database.PostgreSQL.Simple.FromField.FromField [VarT b_1]]

   (AppT (ConT Database.PostgreSQL.Simple.FromRow.FromRow)
         (AppT (AppT (TupleT 2) (VarT a_0)) (VarT b_1)))

   [ValD (VarP Database.PostgreSQL.Simple.FromRow.fromRow)
     (NormalB (InfixE (Just (InfixE (Just (ConE GHC.Tuple.(,))) (VarE Data.Functor.<$>) (Just (VarE Database.PostgreSQL.Simple.FromRow.field)))) (VarE Control.Applicative.<*>) (Just (VarE Database.PostgreSQL.Simple.FromRow.field)))) []]]


instance (FromField a, FromField b) => FromRow (Maybe (a,b)) where
  fromRow =  (null *> null *> pure Nothing)
    <|> (Just <$> fromRow)
[
InstanceD [ClassP Database.PostgreSQL.Simple.FromField.FromField [VarT a_2],ClassP Database.PostgreSQL.Simple.FromField.FromField [VarT b_3]] (AppT (ConT Database.PostgreSQL.Simple.FromRow.FromRow) (AppT (ConT Data.Maybe.Maybe) (AppT (AppT (TupleT 2) (VarT a_2)) (VarT b_3)))) [ValD (VarP Database.PostgreSQL.Simple.FromRow.fromRow) (NormalB (InfixE (Just (InfixE (Just (InfixE (Just (VarE GHC.List.null)) (VarE Control.Applicative.*>) (Just (VarE GHC.List.null)))) (VarE Control.Applicative.*>) (Just (AppE (VarE Control.Applicative.pure) (ConE Data.Maybe.Nothing))))) (VarE Control.Applicative.<|>) (Just (InfixE (Just (ConE Data.Maybe.Just)) (VarE Data.Functor.<$>) (Just (VarE Database.PostgreSQL.Simple.FromRow.fromRow)))))) []]]
[InstanceD [ClassP Database.PostgreSQL.Simple.FromField.FromField [VarT a_2],ClassP Database.PostgreSQL.Simple.FromField.FromField [VarT b_3]]
(AppT (ConT Database.PostgreSQL.Simple.FromRow.FromRow) (AppT (ConT Data.Maybe.Maybe) (AppT (AppT (TupleT 2) (VarT a_2)) (VarT b_3))))

[ValD (VarP fromRow)
      (NormalB
         (InfixE
           (Just (InfixE
                    (Just (InfixE
                          (Just (VarE null))
                          (VarE *>)
                          (Just (VarE null))))
                    (VarE *>)
                    (Just (AppE (VarE pure) (ConE Nothing)))))
           (<|>)
           (Just (InfixE (Just (Just))
       (VarE <$>)
       (Just (VarE fromRow)))))) []]]

-}


{--
( qp
, qr
)
where

import Database.PostgreSQL.Base ()
import Database.PostgreSQL.Simple.QueryParams
import Database.PostgreSQL.Simple.Param
import Database.PostgreSQL.Simple.Result
import Database.PostgreSQL.Simple.QueryResults


import Language.Haskell.TH.Syntax
import Control.Monad

import Language.Haskell.Meta.Parse()

import Prelude (map, (!!), (<), (-), (.) ,($), head, otherwise, zipWith3, Int, toInteger)

-- maybe add case 0 + 1 and make it being generated

qp :: Int -> Q [Dec]
qp k =  do
    ns <- replicateM k (newName "a")
    let pre = map (\x -> ClassP (''Param) [VarT x]) ns
    return [ InstanceD pre (loop k ns) [fun ns] ]
       where
         loop 0 ns = AppT (TupleT k) (VarT (head ns))
         loop i ns | i < k = AppT (loop (i-1) ns ) (VarT (ns !! i))
                   | otherwise = AppT (ConT ''QueryParams) (loop (i-1) ns )
         fun ns = FunD ('renderParams) [ Clause [TupP (map VarP ns)] (NormalB $ ListE $ map (AppE (VarE 'render) . VarE) ns ) [] ] 

qr :: Int -> Q [Dec]
qr k =  do
    nsa <- replicateM k (newName "a")
    nsv <- replicateM k (newName "v")
    nsf <- replicateM k (newName "f")
    fs <- newName "fs"
    vs <- newName "vs"

    let pre = map (\x -> ClassP (''Result) [VarT x]) nsa
    return [ InstanceD pre (loop k nsa) [fun nsa nsf nsv fs vs] ]
       where
         loop 0 ns = AppT (TupleT k) (VarT (head ns))
         loop i ns | i < k = AppT (loop (i-1) ns ) (VarT (ns !! i))
                   | otherwise = AppT (ConT ''QueryResults) (loop (i-1) ns )
         fun nsa nsf nsv fs vs= FunD ('convertResults) 
                             [ Clause [ListP (map VarP nsf), ListP (map VarP nsv)] 
                                  (NormalB $ TupE $ map VarE nsa )
                                     (zipWith3 (\a f v -> ValD (BangP (VarP a)) (NormalB (AppE (AppE (VarE 'convert) (VarE f)) (VarE v))) [] ) nsa nsf nsv)
                             , Clause [VarP fs,VarP vs] (NormalB (AppE (AppE (AppE (VarE 'convertError) (VarE fs)) (VarE vs)) (LitE $ IntegerL $ toInteger k))) []
                             ]
--}