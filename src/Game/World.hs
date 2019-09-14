{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Game.World where

import Apecs
import Apecs.Gloss
import Linear

newtype Position =
  Position (V2 Float)
  deriving (Show)

instance Component Position where
  type Storage Position = Map Position

newtype Direction =
  Direction (V2 Float)
  deriving (Show)

instance Component Direction where
  type Storage Direction = Map Direction

newtype Velocity =
  Velocity (V2 Float)
  deriving (Show)

instance Component Velocity where
  type Storage Velocity = Map Velocity

newtype AcceleratePedal =
  AcceleratePedal Bool
  deriving (Show)

instance Component AcceleratePedal where
  type Storage AcceleratePedal = Map AcceleratePedal

newtype BrakePedal =
  BrakePedal Bool
  deriving (Show)

instance Component BrakePedal where
  type Storage BrakePedal = Map BrakePedal

newtype Time =
  Time Float
  deriving (Show, Num)

instance Semigroup Time where
  (<>) = (+)

instance Monoid Time where
  mempty = 0

instance Component Time where
  type Storage Time = Global Time

data Player =
  Player
  deriving (Show)

instance Component Player where
  type Storage Player = Unique Player

data Machine =
  Machine
  deriving (Show)

instance Component Machine where
  type Storage Machine = Map Machine

newtype Skin =
  Skin Picture
  deriving (Show)

instance Component Skin where
  type Storage Skin = Map Skin

makeWorld
  "World"
    [ ''AcceleratePedal
    , ''BrakePedal
    , ''Camera
    , ''Direction
    , ''Machine
    , ''Player
    , ''Position
    , ''Time
    , ''Skin
    , ''Velocity
    ]

type System' a = System World a
