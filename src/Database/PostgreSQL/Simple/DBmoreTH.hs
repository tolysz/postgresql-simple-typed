{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
module Database.PostgreSQL.Simple.DBmoreTH (qr) where

import           Control.Applicative                  (Applicative (..), (*>), (<$>), (<|>))
import           Control.Monad                        (replicateM)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types     (Null)
import           Language.Haskell.TH
import           Prelude                              hiding (null)

null :: RowParser Null
null =  field

qr :: Int -> Q [Dec]
qr k = do
  ns <- replicateM k (newName "a")
  let pre = map (\x -> ClassP (''FromField) [VarT x]) ns
  return [ InstanceD pre (loop k ns) [fun]
         , InstanceD pre (loop2 ns) [fun2]
         ]
   where
    loop 0 ns = AppT (TupleT k) (VarT (head ns))
    loop i ns | i < k = AppT (loop (i-1) ns ) (VarT (ns !! i))
              | otherwise = AppT (ConT ''FromRow) (loop (i-1) ns )
    loop2 ns = AppT (ConT ''FromRow) (AppT (ConT ''Maybe)(loop (k-1) ns ))

    fun = ValD (VarP 'fromRow)
                   (NormalB $ iterate (sta .) dol !! (k-1) $ ConE (tupleDataName k))
                   []
    sta x = InfixE (Just x) (VarE '(<*>)) (Just (VarE 'field))
    dol x = InfixE (Just x) (VarE '(<$>)) (Just (VarE 'field))
    fun2 = ValD (VarP 'fromRow)
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
