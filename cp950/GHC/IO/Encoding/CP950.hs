{-# OPTIONS_GHC -XDoAndIfThenElse -XNoImplicitPrelude -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding.Cp950
-- Copyright   :  (c) Fhopecc, 2012
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- CP950 Codecs for the IO library
--
-----------------------------------------------------------------------------
module GHC.IO.Encoding.CP950 (
  cp950,
  cp950_decode,
  cp950_encode
  ) where

import GHC.Base
import GHC.Real
import GHC.Num
import GHC.IO.Exception
import GHC.IO.Buffer
import GHC.IO.Encoding.Types
import Data.Word
import Data.Bits
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Tuple
import Data.ByteString.CP950
import Data.ByteString.UnicodeCP950
import System.IO

-- -----------------------------------------------------------------------------
-- Cp950

cp950 :: TextEncoding
cp950 = TextEncoding { textEncodingName = "CP950",
                        mkTextDecoder = cp950_DF,
                        mkTextEncoder = cp950_EF }

cp950_DF :: IO (TextDecoder ())
cp950_DF =
  return (BufferCodec {
             encode   = cp950_decode,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

cp950_EF :: IO (TextEncoder ())
cp950_EF =
  return (BufferCodec {
             encode   = cp950_encode,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

cp950_decode :: DecodeBuffer
cp950_decode 
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let 
       loop !ir !ow
         | ow >= os || ir >= iw =  done ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              case c0 of
                _ | c0 <= 0x7f -> do 
                       ow' <- writeCharBuf oraw ow (unsafeChr (fromIntegral c0))
                       loop (ir+1) ow'
                  | (c0 >= 0xa1 && c0 <= 0xc7) || (c0 >= 0xc9 && c0 <= 0xf9) ->
                      if iw - ir < 2 then done ir ow else do
                         c1 <- readWord8Buf iraw (ir+1)
                         if  (c1 >= 0x40 && c1 < 0x7f) || (c1 >= 0xa1 && c1 < 0xff) 
                         then case chr2 (fromIntegral c0) (fromIntegral c1) of
                              Just c -> do
                                   ow' <- writeCharBuf oraw ow c
                                   loop (ir+2) ow'
                              Nothing -> invalid
                         else do
                              ow' <- writeCharBuf oraw ow (unsafeChr (fromIntegral c0))
                              loop (ir+1) ow'
           where
             invalid = if ir > ir0 then done ir ow else ioe_decodingError

             chr2::Int -> Int -> Maybe Char
             chr2 c0 c1 = 
                  let c0c1 = c0 `shiftL` 8 .|. c1
                  in case M.lookup c0c1 mapCP950Unicode of
                          Just ucp  -> Just (unsafeChr ucp)
                          Nothing   -> Nothing 

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                          else input{ bufL=ir },
                         output{ bufR=ow })

    in
    loop ir0 ow0

ioe_decodingError :: IO a
ioe_decodingError = ioException
     (IOError Nothing InvalidArgument "CP950_decode"
          "invalid CP950 byte sequence" Nothing Nothing)

cp950_encode :: EncodeBuffer
cp950_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done !ir !ow = return (if ir == iw then input{ bufL=0, bufR=0 }
                                         else input{ bufL=ir },
                             output{ bufR=ow })
      loop !ir !ow
        | ow >= os || ir >= iw = done ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           if os - ow < 2 then done ir ow else do
                let n = ord c
                case M.lookup (fromIntegral n) mapUnicodeCP950 of
                    Just x -> do
                        let (c1,c2) = ord2 (fromIntegral x)
                        writeWord8Buf oraw ow     c1
                        writeWord8Buf oraw (ow+1) c2
                        loop ir' (ow+2)
                    Nothing -> do
                        writeWord8Buf oraw ow (fromIntegral (ord c))
                        loop ir' (ow+1)
   in
   loop ir0 ow0

ord2   :: Int -> (Word8,Word8)
ord2 n = (x1,x2)
    where
      x1 = fromIntegral $ (n `shiftR` 8)
      x2 = fromIntegral $ (n .&. 0x00FF)

toCP950 :: Int -> Int
toCP950 unicode = 
    case M.lookup (fromIntegral unicode) mapUnicodeCP950 of
        Just x  -> fromIntegral x
        Nothing -> 0xB169
