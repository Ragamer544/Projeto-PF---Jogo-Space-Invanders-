1:
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
-- import Data.Text

main = activityOf mundoInicial atualize visualize

type World = (Point, Double) -- posição da bola, velocidade em x

incVelX = 0.5
mundoInicial = ((0, 0), 1)

visualize :: World -> Picture
visualize ((x, y), _) = translated x y (solidRectangle 1 1 )

atualize :: Event -> World -> World
atualize (TimePassing t) ((x, y), velX)
 | newX <= -9 ||
   newX >= 9     = ((newX, y), -velX)
 | otherwise     = ((newX, y), velX) 
  where
    newX = x + velX*t  
atualize (KeyPress "Right") ((x, y), velX) = ((x, y), velX+incVelX)
atualize (KeyPress "Left") ((x, y), velX) = ((x, y), velX-incVelX)


atualize _ w                    = w

2.--Implemente um programa que movimente horizontalmente um quadrado preto a
--cada meio segundo. Quando toque o limite esquerdo ou direito, o quadrado deverá
--mudar de direção para retornar, e, ao mesmo, descer meia unidade. Dica: utilize um
--timer de meio segundo para decidir quando movimentar o quadrado.

{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Prelude
import Data.Text

main = activityOf mundoInicial velocidade quadrado

type Vetor = (Double,Double)
type Mundo = (Point,Vetor,Timer)
type Timer = Double

mundoInicial =((0,0),(20,0),0.5)

velocidade :: Event -> Mundo -> Mundo
velocidade (TimePassing t) ((x,y),(a,b),c)
 |(abs x >= 9.5) && att = ((x,y - 0.5),(-a,b) , 0.1)
 | abs x >= 9.5             = ((x - a * t ,y - 0.5),(-a,b) , c-t)
 | att                      = ((x + a * t,y),(a,b) , 0.1)
 | otherwise                = ((x,y),(a,b),c-t)
  where
  att = c <= 0
 
quadrado :: Mundo -> Picture
quadrado ((x,y),_,_) = translated x y (solidRectangle 1 1)
 
-- 3. Implemente um programa que permita controlar a movimentação lateral de uma
-- nave localizada na parte inferior da tela. A direção da movimentação será controlada
-- através das teclas “Left” e “Right”. A nave se moverá à esquerda enquanto a tecla
-- “Left” estiver sendo apertada e à direita caso for a tecla “Right”. A magnitude da
-- velocidade da nave será sempre constante. Se nenhuma tecla estiver sendo
-- apertada a nave deverá ficar parada.

{-# Language OverloadedStrings #-}

import CodeWorld
import CodeWorld.Sketches
import Data.Text

main = activityOf inicial atualize visualize

type World = Nave

atualizeNave n n1 = n1

type Nave = (Double, Double) -- PosX, Direção

naveY = -8.5
velNave = 4
naveInicial = (0, 0)

navePraDireita (p, _) = (p, 1)

navePraEsquerda (p, _) = (p, -1)

naveParada (p, _) = (p, 0)

inicial :: World
inicial = naveInicial

visualize nv = visualizeNave nv 

visualizeNave (x, _) = translated x naveY (solidRectangle 2 1)

atualize :: Event -> World -> World
atualize (KeyPress "Left")    w@ nv        = atualizeNave w (navePraEsquerda nv)
atualize (KeyRelease "Left")  w@ nv        = atualizeNave w (naveParada nv)
atualize (KeyPress "Right")   w@ nv        = atualizeNave w (navePraDireita nv)
atualize (KeyRelease "Right") w@ nv        = atualizeNave w (naveParada nv)
atualize (TimePassing t)      w@ nv        = ( pasaTempoNave t nv)
atualize _ w = w

pasaTempoNave t (x, d) = (x + d*velNave*t, d)

-- 4. Implemente um programa que se comporte como o programa da questão anterior,
--mas que adicionalmente possa disparar uma bala. A bala será disparada apertando
--a tecla “SPACE”. A nave poderá fazer vários disparos, no entanto, somente
--disparará quando não houver outra bala viva, visível na tela. A velocidade da bala
--em y será constante. A velocidade da bala em x será a mesma velocidade da nave
--no momento do disparo.

{-# Language OverloadedStrings #-}

import CodeWorld
import CodeWorld.Sketches
import Data.Text
import Prelude

main = activityOf inicial atualize visualize

type World = (Nave, Bala)

atualizeNave (n, ba) n1 = (n1, ba)

atualizeBala (n, ba) ba1 = (n, ba1)

type Nave = (Double, Double) -- PosX, Direção
type Bala = (Point, Bool, Double) -- Pos, Ativa, velocidade em x

naveY = -8.5
velNave = 6
naveInicial = (0, 0)

alturaNave = 2

navePraDireita (p, _) = (p, 1)

navePraEsquerda (p, _) = (p, -1)

naveParada (p, _) = (p, 0)

velBalaY = 5
balaInicial = ((0, 10.1), False, 0)

inicial :: World
inicial = (naveInicial,balaInicial)

visualize (nv, ba) = visualizeNave nv &
                         visualizeBala ba 

-- com 0.27 o a altura fica 2
visualizeNave (x, _) = translated x naveY (solidRectangle 2 1)

visualizeBala ((x, y), True, _) = translated x y (colored black (solidRectangle 0.9 1)) 
visualizeBala _ = blank

atualize :: Event -> World -> World
atualize (KeyPress "Left")    w@(nv, _) = atualizeNave w (navePraEsquerda nv)
atualize (KeyRelease "Left")  w@(nv, _) = atualizeNave w (naveParada nv)
atualize (KeyPress "Right")   w@(nv, _) = atualizeNave w (navePraDireita nv)
atualize (KeyRelease "Right") w@(nv, _) = atualizeNave w (naveParada nv)
atualize (KeyPress " ")       w@(nv, ba) = atualizeBala w (atira nv ba)
atualize (TimePassing t)      w@(nv, ba) = ( pasaTempoNave t nv, 
                                                 pasaTempoBala t ba)
atualize _ w = w

atira _ ba@(_, True, _) = ba
atira (xNav, dNav) _    = ((xNav, naveY + alturaNave/2), True, dNav*velNave)
-- a bala sai da ponta da nave

pasaTempoNave t (x, d) = (x + d*velNave*t, d)


pasaTempoBala t ba@(_, False, _) = ba
pasaTempoBala t (p, True, vx) 
  | dentroEspaco newP = (newP, True, vx)
  | otherwise         = (p, False, vx)
  where
    newP = vectorSum p (scaledVector t (vx, velBalaY))
    
-- *** constantes 10, 10
dentroEspaco (x,y) =  -10 <= x && x <= 10 &&
                      -10 <= y && y <= 10

-- 5. Implemente um programa que junte os comportamentos das Questões 2. e 4.

{-# Language OverloadedStrings #-}

import CodeWorld
import CodeWorld.Sketches
import Data.Text

main = debugActivityOf inicial atualize visualize

type World = (Nave, Bala, Bola)

atualizeNave (n, ba, bo) n1 = (n1, ba, bo)

atualizeBala (n, ba, bo) ba1 = (n, ba1, bo)

type Nave = (Double, Double) -- PosX, Direção

naveY = -8.5
velNave = 4
naveInicial = (0, 0)

alturaNave = 2

navePraDireita (p, _) = (p, 1)

navePraEsquerda (p, _) = (p, -1)

naveParada (p, _) = (p, 0)

type Bala = (Point, Bool, Double) -- Pos, Ativa, velocidade em x

velBalaY = 3
balaInicial = ((0, 10.1), False, 0)

type Bola = (Double, Double, Double) -- PosX, Direção, Timer

bolaY = 8
deslocamentoX = 0.5
bolaTimer = 0.5
bolaInicial = (0, -1, bolaTimer)

inicial :: World
inicial = (naveInicial, balaInicial, bolaInicial)

visualize (nv, ba, bo) = visualizeNave nv &
                         visualizeBala ba &
                         visualizeAlvo bo

-- com 0.27 o a altura fica 2
visualizeNave (x, _) = translated x naveY (dilated 0.27 sketchedRocket)

visualizeBala ((x, y), True, _) = translated x y (colored red (solidCircle 0.1)) 
visualizeBala _ = blank

visualizeAlvo (x, _, _) = translated x bolaY (solidRectangle 1 1)

atualize :: Event -> World -> World
atualize (KeyPress "Left")    w@(nv, _, _) = atualizeNave w (navePraEsquerda nv)
atualize (KeyRelease "Left")  w@(nv, _, _) = atualizeNave w (naveParada nv)
atualize (KeyPress "Right")   w@(nv, _, _) = atualizeNave w (navePraDireita nv)
atualize (KeyRelease "Right") w@(nv, _, _) = atualizeNave w (naveParada nv)
atualize (KeyPress " ")       w@(nv, ba, _) = atualizeBala w (atira nv ba)
atualize (TimePassing t)      w@(nv, ba, bo) = ( pasaTempoNave t nv,
                                                 pasaTempoBala t ba,
                                                 pasaTempoBola t bo )
atualize _ w = w

atira _ ba@(_, True, _) = ba
atira (xNav, dNav) _    = ((xNav, naveY + alturaNave/2), True, dNav*velNave)
-- a bala sai da ponta da nave

pasaTempoNave t (x, d) = (x + d*velNave*t, d)


pasaTempoBala t ba@(_, False, _) = ba
pasaTempoBala t (p, True, vx) 
  | dentroEspaco newP = (newP, True, vx)
  | otherwise         = (p, False, vx)
  where
    newP = vectorSum p (scaledVector t (vx, velBalaY))
    
-- *** constantes 10, 10
dentroEspaco (x,y) =  -10 <= x && x <= 10 &&
                      -10 <= y && y <= 10

-- *****Definir constantes pra 9 e -9
pasaTempoBola t (x, d, tmr)
   | tmr > 0               = (x, d, tmr-t)
   | d == 1 && newX > 9 ||
     d == -1 && newX < -9  =  (x, -d, bolaTimer)
   | otherwise             = (newX, d, bolaTimer)
   where
     newX = x + d*deslocamentoX
     
atualizaTimer (p, d, _) newTmr = (p, d, newTmr)
