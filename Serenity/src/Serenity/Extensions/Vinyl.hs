{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Serenity.Extensions.Vinyl
(	module Data.Vinyl
,	makeBinaryNewtype
) where

import Data.Vinyl
import Data.Binary
import Test.QuickCheck.Arbitrary
import Language.Haskell.TH


instance Binary (Rec '[]) where
	put RNil = return ()
	get = return RNil

instance (Binary t, Binary (Rec fs)) => Binary (Rec ((sy ::: t) ': fs)) where
	put ((_,x) :& xs) = put x >> put xs
	get = do
		x <- get
		xs <- get
		return ((Field, x) :& xs)


instance Arbitrary (Rec '[]) where
	arbitrary = return RNil

instance (Arbitrary t, Arbitrary (Rec fs)) => Arbitrary (Rec ((sy ::: t) ': fs)) where
	arbitrary = do
		x <- arbitrary
		xs <- arbitrary
		return $ Field =: x <+> xs

makeBinaryNewtype :: Name -> String -> String -> Q [Dec]
makeBinaryNewtype typ name tname = do
	n   <- return $ mkName name
	tn0 <- return $ mkName tname
	tn1 <- return $ mkName tname
	x <- return $ mkName "x"
	shDef <- return $ FunD 'show [Clause [ConP tn1 [VarP x]] (NormalB (AppE (VarE 'show) (VarE x))) []]
	(InstanceD cxt1 t1 _) <- fmap head $ [d| instance Show $(return $ ConT tn0) |]
	a <- return $ mkName "a"
	b <- return $ mkName "b"
	eqDef <- return $ FunD '(==) [Clause [ConP tn1 [VarP a],ConP tn1 [VarP b]] (NormalB (InfixE (Just (VarE a)) (VarE '(==)) (Just (VarE b)))) []]
	(InstanceD cxt2 t2 _) <- fmap head $ [d| instance Eq $(return $ ConT tn0) |]
	s <- return $ mkName "s"
	newt <- return $ NewtypeD [] tn0 [] (RecC tn1 [(n,NotStrict,ConT typ)]) []
	putDef <- return $ [FunD 'put [Clause [RecP tn1 [(n, VarP s)]] (NormalB $ AppE (VarE 'put) (VarE s)) []]]
	(InstanceD cxt3 t3 getDef) <- fmap head 
		[d| 
			instance Binary $(return $ ConT tn0) where
				get = do s <- get; return ($(return $ (ConE tn1)) s)
		|]
	arDef <- return $ FunD 'arbitrary [Clause [ConP tn1 [VarP x]] (NormalB (AppE (VarE 'arbitrary) (VarE x))) []]
	(InstanceD cxt4 t4 _) <- fmap head $ [d| instance Arbitrary $(return $ ConT tn0) |]
	return $ [newt, (InstanceD cxt1 t1 [shDef]), (InstanceD cxt2 t2 [eqDef]), (InstanceD cxt3 t3 (getDef++putDef)), (InstanceD cxt4 t4 [eqDef])]


