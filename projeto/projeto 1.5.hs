1° Codigo : {-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text as T
import Data.List
main = drawingOf ( dilataGrafo [275, 214.3, 331.9, 25.69, 47.42, 59.11] (centralize (barras "População em milhões" [275, 214.3, 331.9, 25.69, 47.42, 59.11] assortedColors ["INDONESIA", "BRASIL", "EEUU", "AUSTRALIA", "ESPANHA", "ITALIA"])))
larguraBarra = 1
sep = 2
escMax = 9
dist = larguraBarra + sep

dilataGrafo :: [Double]-> Picture -> Picture 
dilataGrafo ds pic = if (length ds) <= 6 then dilated 1.0 pic else if (length ds) <= 12 then dilated 0.5 pic else dilated 0.3 pic
 where
 ds = [275, 214.3, 331.9, 25.69, 47.42, 59.11]
 
centralize :: Picture -> Picture
centralize barras = translated (-((fromIntegral(length ds -1 )* dist)/2)) (-5) barras
  where
   ds = [275, 214.3, 331.9, 25.69, 47.42, 59.11]

barras :: T.Text -> [Double] -> [Color] -> [T.Text] -> Picture
barras t ds cs ts = (tituloCentral t) &  pictures [ translated (i*dist) (a/2) (barra a c) & (nomePais i d) & linhaVertical & linhaHorizontal  | (i, a, c, d) <- zip4 [0..] (escala ds) cs ts ]
 & pictures [ linha a | a <- (escala ds)] & pictures [ numerosLado a b | (a,b) <- zip (escala ds) ds ]
 where
   barra a c = colored c (solidRectangle larguraBarra a)
   linha a = colored gray (polyline [(-1,a) , (tamanhoMax,a)])
   nomePais i d = translated ( i * dist) (-1) (lettering (T.pack(take 4( T.unpack d ))))
   tituloCentral t = translated ((fromIntegral (length ds) -1) * dist/2 ) 10 (lettering t)
   tamanhoMax = (fromIntegral(length ds)) * dist
   linhaVertical = polyline [(-1,0), (-1,10)]
   linhaHorizontal = polyline [(-1,0) , (tamanhoMax,0)] 
   numerosLado a b  = translated (-2) (a) (lettering (T.pack (show (truncate b))))
  
escala :: [Double] -> [Double]
escala rs = [ r * esc  | r <- rs ]
  where
    esc = escMax / maximum rs

link do codeWorld :https://code.world/haskell#PpQGHcOBg54QsWMp9yrtSFg

2° codigo: import CodeWorld

main = animationOf solNascendo

raiointerno = 8
raioexterno = 10
numRaios = 35

sol :: Double -> Double -> Double -> Picture
sol raiointerno raioexterno numRaios = colored orange (solidCircle raiointerno) & solares numRaios
  where
    solares numRaios = colored  yellow (pictures [rotated x (translated 0 raiointerno (raio)) | x <- [ 0,2 * pi / numRaios .. 2 * pi]])
    raio = solidPolygon [(-(raioexterno - raiointerno) / 2, 0), ((raioexterno - raiointerno) / 2, 0), (0, raioexterno - raiointerno)]

solNascendo :: Double -> Picture
solNascendo t = translated 0 ((t-20) * 1) (sol raiointerno raioexterno numRaios)

link do codeWorld :https://code.world/haskell#PvE8pKKkBcYtrf_qJr3VUAw 