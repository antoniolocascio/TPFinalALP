{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}


-- Basado en el ejemplo dado en: https://stackoverflow.com/questions/39661287/gaussianblurimage-in-haskell-opencv-haskell-binding-to-opencv-3-1

module BlurImage (blurImage) where

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

gaussianBlurImage :: (depth `In` '[Word8, Word16, Float, Double], channels `In` '[1, 3, 4]) => (M.Mat shape ('S channels) ('S depth)) -> CV.CvExcept (M.Mat shape ('S channels) ('S depth)) 
gaussianBlurImage image = CV.gaussianBlur (V2 13 13 :: V2 Int32) 0 0 image

-- Funcion para difuminar una imagen
blurImage :: forall height0 width0 channels depth . ( depth `In` '[Word8, Word16, Float, Double] , channels `In` '[1, 3, 4]) => M.Mat ('S '[height0, width0]) ('S channels) ('S depth) -> IO (M.Mat ('S '[height0, width0]) ('S channels) ('S depth))
blurImage image = do
    gaussianBlurred   <- return $ gaussianBlurImage image     
    return $ CV.exceptError $ gaussianBlurred