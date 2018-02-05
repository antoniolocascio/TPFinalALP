{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Basado en el ejemplo dado en: https://stackoverflow.com/questions/39661287/gaussianblurimage-in-haskell-opencv-haskell-binding-to-opencv-3-1
-- y en el algoritmo explicado en https://www.pyimagesearch.com/2016/02/08/opencv-shape-detection/

module ImageRec where

import Control.Monad (void)
import Control.Monad.Primitive 
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV as CV
import qualified Data.ByteString as B
import BlurImage
import Threshold
import Data.Word
import qualified OpenCV.Internal.Mutable as Mutable
import Data.Vector as V
import Linear.V4
import Linear.V2
import GHC.Int (Int32)
import Data.List as L
import GHC.TypeLits
import AST
import Control.Exception as E

-- Funciones principales
scanImage :: FilePath -> IO (Either Error StructPage)
scanImage filepath = E.catch (tryScanImage filepath) (\(e :: E.IOException) -> return (Left $ show e))

tryScanImage :: FilePath -> IO (Either Error StructPage)
tryScanImage filepath = do  img <- CV.imdecode CV.ImreadGrayscale <$> B.readFile filepath 
                            blurred <- blurImage ((CV.exceptError $ M.coerceMat img) :: M.Mat (CV.S [ CV.D, CV.D]) (CV.S 1) (CV.S Word8))
                            (thresh, _) <- threshold ((CV.exceptError $ M.coerceMat blurred) :: CV.Mat (CV.S [CV.D, CV.D]) (CV.S 1) (CV.S Word8))
                            t' <- Mutable.thaw thresh
                            cnts <- CV.findContours CV.ContourRetrievalTree CV.ContourApproximationSimple t'
                            let cnt = V.head cnts
                                totalArea = let (h,w) = matDims img in fromIntegral $ h * w :: Double 
                                contorno = aContorno cnt
                                c = V.head $ V.map ((removeSmaller totalArea) . (removeInner totalArea))  (ignoreOutmost contorno)                 
                            return $ Right $ toStructPage img c


-- Se ignora el contorno mas externo, correspondiente a los limites de la imagen
ignoreOutmost :: Contorno -> V.Vector Contorno
ignoreOutmost cnt | (V.head (cPuntos cnt)) == (0, 0) = cHijos cnt
                  | otherwise = V.singleton $ cnt

-- Utilizada para ignorar la duplicación de contornos
removeInner :: Double -> Contorno -> Contorno
removeInner totalArea c@(C a puntos hijos)
  | V.length hijos == 1 && isInner totalArea (cPuntos $ hijos V.! 0) puntos = C a puntos (V.map (removeInner totalArea) (cHijos $ hijos V.! 0))
  | otherwise = C a puntos (V.map (removeInner totalArea) hijos) 

-- Se descartan los contornos menores a un area minima calculada en proporcion al area total
-- para ignorar los contornos provenientes de letras y ruido
removeSmaller :: Double -> Contorno -> Contorno
removeSmaller totalArea (C a puntos hijos) = C a puntos (V.filter (\c -> (cArea c) > minArea) (V.map (removeSmaller totalArea) hijos))
  where minScale = 8000 
        minArea = max 200 (totalArea / minScale)
-- Conversiones
toStructPage :: M.Mat (CV.S [CV.D, CV.D]) CV.D CV.D -> Contorno -> StructPage
toStructPage img c@(C _ puntos hijos) =  let n = V.length puntos 
                                        in if n == 4 
                                          then Rectangle (V.toList (V.reverse (V.map (toStructPage img) hijos))) 
                                          else Circle marked
  where
    thresIntensity = 160 
    intesnsity = (meanIntensityCont ((CV.exceptError $ M.coerceMat img) :: 
                  M.Mat (CV.S [CV.D, CV.D]) (CV.S 1) CV.D) c)
    marked = intesnsity < thresIntensity

aContorno :: CV.Contour -> Contorno
aContorno cnt = let
                  per = CV.arcLength (CV.contourPoints cnt) True
                  approx = CV.approxPolyDP (CV.contourPoints cnt) ((CV.exceptError per) * 0.04) True
                  puntos = V.map aPunto approx
                  area = pArea puntos
                in C  area puntos (V.map aContorno (CV.contourChildren cnt))

aPunto :: CV.Point2i -> (Int32, Int32)
aPunto p = let (V2 a b) = (CV.fromPoint p) in (a, b) 


-- Operaciones sobre contornos
cPuntos :: Contorno -> V.Vector Punto
cPuntos (C _ puntos _ ) = puntos

cHijos :: Contorno -> V.Vector Contorno
cHijos (C _ _ hijos) = hijos

cArea :: Contorno -> Double
cArea (C a _ _) = a


-- Determina si un contorno (los puntos que lo describen) corresponde al interior de otro contorno 
-- Esto sucede por el grosor de la linea que lo forma
isInner :: Double -> V.Vector Punto -> V.Vector Punto -> Bool
isInner totalArea v1 v2 = let 
                            l1 = L.sortBy pointOrder (V.toList v1)
                            l2 = L.sortBy pointOrder (V.toList v2)
                          in 
                            (V.length v1 == V.length v2) && L.all (\(x, y) -> withinRange x y) (L.zip l1 l2)
  where
    range = max 10 (floor (totalArea / 300000)) 
    withinRange :: Punto -> Punto -> Bool
    withinRange (x1, y1) (x2, y2) = abs (x1 - x2) < range && abs (y1 - y2) < range
    rangeComp :: Int32 -> Int32 -> Ordering
    rangeComp a b 
      | a < b && abs (a - b) > range = LT
      | a > b && abs (a - b) > range = GT
      | otherwise = EQ 
    pointOrder :: Punto -> Punto -> Ordering
    pointOrder (x1, y1) (x2, y2) = case rangeComp x1 x2 of
        LT -> LT
        GT -> GT
        EQ -> rangeComp y1 y2


-- Calcula el area dentro de la figura descripta por un vector de puntos 
pArea :: V.Vector Punto -> Double
pArea puntos =  let points = V.map dePunto puntos 
                in  CV.exceptError $ CV.contourArea points CV.ContourAreaAbsoluteValue
  where
    dePunto :: Punto -> CV.Point2f
    dePunto (a, b) = CV.toPoint (V2 (fromIntegral a) (fromIntegral b))

-- Calcula la intensidad media de un contorno
meanIntensityCont :: (1 GHC.TypeLits.<= channels, channels GHC.TypeLits.<= 4) =>
  M.Mat (CV.S [height, width]) (CV.S channels) depth -> Contorno -> Double
meanIntensityCont img cnt = let v = fst $ CV.exceptError (CV.meanStdDev (CV.exceptError $ cropFitCont img cnt) Nothing)
                                (V4 x _ _ _) = (CV.fromScalar v) :: V4 Double
                            in x

-- Devuelve la matriz correspondiente a el area de la imagen que contiene un contorno dado
cropFitCont :: (1 GHC.TypeLits.<= channels, channels GHC.TypeLits.<= 4) =>
  M.Mat (CV.S [height, width]) (CV.S channels) depth -> Contorno -> CV.CvExcept (CV.Mat (CV.S [CV.D, CV.D]) (CV.S channels) depth)
cropFitCont img cont = CV.matSubRect img (fitRect $ cPuntos cont)

-- Calcula el rectangulo que contiene un contorno dado por los puntos que lo describen
fitRect :: V.Vector Punto -> CV.Rect2i
fitRect puntos =  let
                    xs = V.map (\(x,_) -> x) puntos
                    ys = V.map (\(_,y) -> y) puntos
                    minx = V.minimum xs
                    miny = V.minimum ys
                    maxx = V.maximum xs
                    maxy = V.maximum ys
                    h = maxy - miny
                    w = maxx - minx
                  in CV.toRect $ CV.HRect (V2 minx miny) (V2 h w)
    
-- Devuelve las dimensiones de una matriz, con el formato (nfilas, ncolumnas)
matDims :: CV.Mat shape channels depth -> (Int32, Int32)
matDims img = let (M.MatInfo sh _ _) = M.matInfo img in (sh !! 0, sh !! 1)




 


