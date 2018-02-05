{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- Basado en el ejemplo dado en: https://stackoverflow.com/questions/39661287/gaussianblurimage-in-haskell-opencv-haskell-binding-to-opencv-3-1

module Threshold (threshold) where

import Control.Monad ( void )
import Control.Monad.Except
import qualified Data.ByteString as B
import Data.Word
import Data.Proxy
import qualified OpenCV as CV
import Linear.V2
import OpenCV.TypeLevel
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.Core.Types.Size as S
import qualified OpenCV.ImgProc.GeometricImgTransform as GIT
import GHC.Int (Int32)

thresholdImage :: (depth `In` '[Word8, Float]) => (M.Mat shape ('S 1) ('S depth)) -> CV.CvExcept (M.Mat shape ('S 1) ('S depth), Double) 
thresholdImage image = CV.threshold (CV.ThreshVal_Abs 100) (CV.Thresh_Binary 255) image

threshold :: forall height0 width0 channels depth . ( depth `In` '[Word8, Float]) => M.Mat ('S '[height0, width0]) ('S 1) ('S depth) -> IO (M.Mat ('S '[height0, width0]) ('S 1) ('S depth), Double)
threshold image = do
    thresh  <- return $ thresholdImage image     
    return $ CV.exceptError $ thresh
