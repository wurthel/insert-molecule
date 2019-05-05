{-|
Module      : Trigonometry
Copyright   : (c) Vusal Salmanov, 2019
Maintainer  : salmanov.vh@gmail.com
Stability   : experimental

Данный модуль был составлен для работы с тригонометрическими функциями, 
аргументами которых являются углы в градусах (по-умолчанию тригонометрические
функции в @Haskell@ работаю c углами в радианах).
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TrigonometryDegree where

newtype Degree a =
  Degree a
  deriving (Num, Enum, Show, Eq, Ord, Fractional)

toDegree :: Floating a => a -> Degree a
toDegree = Degree

rad2Deg :: Floating a => a -> Degree a
rad2Deg x = Degree $ x * 180 / pi

deg2Rad :: Floating a => Degree a -> a
deg2Rad (Degree x) = x * pi / 180

cosd :: Floating a => Degree a -> a
cosd = cos . deg2Rad

sind :: Floating a => Degree a -> a
sind = sin . deg2Rad

acosd :: Floating a => a -> Degree a
acosd = rad2Deg . acos

asind :: Floating a => a -> Degree a
asind = rad2Deg . asin
