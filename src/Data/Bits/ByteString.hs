{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-------------------------------------------------------------------------------
-- |
-- Module       : Data.Bits.ByteString
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module Data.Bits.ByteString where

import            Data.Bits
import qualified  Data.ByteString as B
import            Data.Word

instance Bits B.ByteString where

  (.&.) a b = B.pack $ B.zipWith (.&.) a b
  {-# INLINE (.&.) #-}

  (.|.) a b = B.pack $ B.zipWith (.|.) a b
  {-# INLINE (.|.) #-}

  xor a b = B.pack $ B.zipWith xor a b
  {-# INLINE xor #-}

  complement = B.map complement
  {-# INLINE complement #-}

  shift x i
    | i < 0     = x `shiftR` (-i)
    | i > 0     = x `shiftL` i
    | otherwise = x
  {-# INLINE shift #-}

  shiftR bs 0 = bs
  shiftR "" _ = B.empty
  shiftR bs i
      | i `mod` 8 == 0 =
        B.take (B.length bs) $ B.append
          (B.replicate (i `div` 8) 0)
          (B.drop (i `div` 8) bs)
      | i `mod` 8 /= 0 =
        B.pack $ take (B.length bs)
          $ (replicate (i `div` 8) (0 :: Word8))
          ++ (go (i `mod` 8) 0 $ B.unpack (B.take (B.length bs - (i `div` 8)) bs))
    where
    go _ _ [] = []
    go j w1 (w2:wst) = (maskR j w1 w2) : go j w2 wst
    maskR j w1 w2 = (shiftL w1 (8-j)) .|. (shiftR w2 j)
  shiftR _ _ = error "I can't believe you've done this."
  {-# INLINE shiftR #-}

  shiftL bs 0 = bs
  shiftL "" _ = B.empty
  shiftL bs i
      | i `mod` 8 == 0 =
        B.take (B.length bs) $ B.append
          (B.drop (i `div` 8) bs)
          (B.replicate (i `div` 8) 0)
      | i `mod` 8 /= 0 =
        B.pack $ drop ((i `div` 8) - B.length bs)
          $ (tail (go (i `mod` 8) 0 $ B.unpack (B.drop (i `div` 8) bs)))
          ++ (replicate (i `div` 8) 0)
    where
    go j w1 [] = [shiftL w1 j]
    go j w1 (w2:wst) = (maskL j w1 w2) : go j w2 wst
    maskL j w1 w2 = (shiftL w1 j) .|. (shiftR w2 (8-j))
  shiftL _ _ = error "I can't believe you've done this."
  {-# INLINE shiftL #-}

  rotate x i
    | i < 0     = x `rotateR` (-i)
    | i > 0     = x `rotateL` i
    | otherwise = x
  {-# INLINE rotate #-}

  rotateR bs 0 = bs
  rotateR bs i
      | B.length bs == 0 = B.empty
      | B.length bs == 1 = B.singleton (rotateR (bs `B.index` 0) i)
      | B.length bs > 1 = do
        let shiftedWords =
              B.append
                (B.drop (nWholeWordsToShift i) bs)
                (B.take (nWholeWordsToShift i) bs)
        let tmpShiftedBits = (shiftR shiftedWords (i `mod` 8))
        let rotatedBits = (shiftL (B.last shiftedWords) (8 - (i `mod` 8))) .|. (B.head tmpShiftedBits)
        rotatedBits `B.cons` (B.tail tmpShiftedBits)
    where
    nWholeWordsToShift n =  (B.length bs - (n `div` 8))
  rotateR _ _ = error "I can't believe you've done this."
  {-# INLINE rotateR #-}

  rotateL bs 0 = bs
  rotateL bs i
      | B.length bs == 0 = B.empty
      | B.length bs == 1 = B.singleton (rotateL (bs `B.index` 0) i)
      | i `mod` 8 == 0 = B.append
                          (B.drop (i `div` 8) bs)
                          (B.take (i `div` 8) bs)
      | B.length bs > 1 = do
        let shiftedWords =
              B.append
                (B.drop (i `div` 8) bs)
                (B.take (i `div` 8) bs)
        let tmpShiftedBits = (shiftL shiftedWords (i `mod` 8))
        let rotatedBits = (shiftR (B.head shiftedWords) (8 - (i `mod` 8))) .|. (B.last tmpShiftedBits)
        (B.init tmpShiftedBits) `B.snoc` rotatedBits
  rotateL _ _ = error "I can't believe you've done this."
  {-# INLINE rotateL #-}

  bitSize x = 8 * B.length x
  {-# INLINE bitSize #-}

  bitSizeMaybe x = Just (8 * B.length x)
  {-# INLINE bitSizeMaybe #-}

  isSigned _ = False
  {-# INLINE isSigned #-}

  testBit x i = testBit (B.index x (B.length x - (i `div` 8) - 1)) (i `mod` 8)
  {-# INLINE testBit #-}

  bit i = (bit $ mod i 8) `B.cons` (B.replicate (div i 8) (255 :: Word8))
  {-# INLINE bit #-}

  setBit x i
    | B.length x >= B.length (bit i) = x .|. (paddingZeroes`B.append` bit i)
    | otherwise = x
    where paddingZeroes = B.replicate (B.length x - (B.length $ bit i)) (0 :: Word8)
  {-# INLINE setBit #-}

  popCount x = sum $ map popCount $ B.unpack x
  {-# INLINE popCount #-}
