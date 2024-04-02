{-# Language OverloadedStrings #-}

import CodeWorld
type World = (Nave, Bala , Alien)

--Função Principal

main = activityOf mundoInicial atualizaMundo visualizaMundo

mundoInicial :: World
mundoInicial = (naveInicial, balaInicial, alienInicial)


--Desenha nave
type Nave = (Double, Double) -- PosX, Direção

posiNaveY = -8.5
velNave = 4
naveInicial = (0, 0)


navePraDireita (p, _) = (p, 1)

navePraEsquerda (p, _) = (p, -1)

naveParada (p, _) = (p, 0)

visualizaNave :: (Double, Double) -> Picture
visualizaNave (x,d) = translated x posiNaveY $ scaled 2.2 2.2 $ picToPicture navePic
  where
    navePic = 
      [ "   #   ",
        "  ###  ",
        " ##### ",
        "#######"]
        
atualizeNave (n, ba,ali) n1 = (n1, ba, ali)

passaTempoNave t (x, d) = (x + d*velNave*t,  d)

        
-- Definição do tipo Pic
type Pic = [String]

picToPicture :: Pic -> Picture
picToPicture pic = scaled 0.1 0.1 $ pictures (map transformaParaPicture ( zip [0..] pic))

-- Função auxiliar para transformar uma linha da Pic em uma Picture
transformaParaPicture :: (Double, String) -> Picture
transformaParaPicture (coordY, pic') = pictures ( map (pixelToPicture coordY) ( zip [-3..] pic'))

-- Função auxiliar para transformar um pixel da Pic em uma Picture
pixelToPicture :: Double -> (Double, Char) -> Picture
pixelToPicture coordY (coordX, pixel)
  | pixel == '#' = translated (coordX) (-coordY) (colored black (solidRectangle 1 1))
  | otherwise    = blank
      
      
      
-- Imagens dos aliens
alien1_1 = [     "  #       #    ",
                 "   #     #     ",
                 "   #######     ",
                 "  ## ### ##    ",
                 " ###########   ",
                 " # ####### #   ",
                 " # #     # #   ",
                 "   ##   ##     "]


alien1_2 = [     "  #     #    ",
                 "#  #   #  #  ",
                 "# ####### #  ",
                 "### ### ###  ",
                 " #########   ",
                 " #########   ",
                 "  #     #    ",
                 "###     ###  "]
                 
alien2_1 = [     "    ###       ",
                 " #########    ",
                 "###########   ",
                 "##  ###  ##   ",
                 "###########   ",
                 "  ##   ##     ",
                 " ## ### ##    ",
                 "##       ##   "]
                 
alien2_2 = [     "    ###       ",
                 " #########    ",
                 "###########   ",
                 "##  ###  ##   ",
                 "###########   ",
                 "  ##   ##     ",
                 "##  ###  ##   ",
                 " ##     ##    "]
  
alien3_1=[      "    ###        ",
                "   #####       ",
                "  #######      ",
                " ## ### ##     ",
                " #########     ",
                "  # ### #      ",
                " #       #     ",
                "  #     #      "]
                
alien3_2= [     "    ###       ",
                "   #####      ",
                "  #######     ",
                " ## ### ##    ",
                " #########    ",
                "   #   #      ",
                "  # ### #     ",
                " # #   # #    "]


--Funções Para a Bala

type Bala = ((Double, Double), Bool, Double,Double) -- Pos, Ativa, velocidade em x, velocidade em Y

visualizaBala ((x, y), True, _,_) = translated x y (colored gray (solidRectangle 0.2 0.5)) -- Gera a bala na tela
visualizaBala _ = blank

velBalaY = 20
balaInicial = ((0,-3), False, 0, 3)

atualizeBala (n, ba, ali) ba1 = (n, ba1, ali) -- Atualiza a Bala

passaTempoBala t _ ba@(_, False, _, velY ) = ba -- atualiza a o tempo da bala
passaTempoBala t nv@ (x',dirX) (p@(x,y), True, vx,velY) 
  | dentroEspaco newP = (newP, True, vx,velY)
  | otherwise         = ((x',y+velY*t), False, dirX,velY)
  where
    newP = vectorSum p (scaledVector t (vx, velBalaY))
    
   
--Verifica se a Bala tá dentro do espaço delimitado

dentroEspaco (x,y) =  -10 <= x && x <= 10 &&
                      -10 <= y && y <= 10
    
-- Função que verifica se o projetio tá "vivo ou morto"

projetil _ ba@(_, True, _,velBalaY) = ba
projetil (xDaNav, dDaNav) _    = ((xDaNav, posiNaveY - 1), True, dDaNav*velNave,velBalaY)

-- Função Alien

type Alien = (Double, Double,Double, Double) -- Pos, Direção, Timer

deslocamentoX = 1.5 
deslocamentoY = 1
alienTimer = 0.5
alienInicial = (0, 0 , -1, alienTimer)


imagemAlien'  = [alien2_1,alien2_1,alien1_1,alien1_1,alien3_1] -- Lista das imagens
imagemAlien'' = [alien2_2,alien2_2,alien1_2,alien1_2,alien3_2] -- Lista das imagens 2

colunas1 :: [[Pic]]
colunas1 =  replicate 11 imagemAlien' -- replica as imagens dos aliens
colunas2 =  replicate 11 imagemAlien'' -- replica as imagens dos aliens

coordenadas picss =  zip picss [0..]  -- Gera as coordenas da Pic
coordX picss = map coordenadas picss -- Gera as coordenas em X da Pic
coordY picss = map criaPosicaoY (coordenadas picss) -- Gera as coordenas em Y da Pic

criaPosicaoY (picss, a) = map y picss -- mapea a Posição Y da pic
  where
  y x = (x, a)
      

organizaAlien :: [[a]] -> [(a,Double,Double)]     -- Organiza os aliens com base nas suas coordenadas 
organizaAlien as = concatena( concat(coordX(coordY as)))

concatena xs =  map agrupa xs -- Junta tudo em uma tripla
agrupa ((p ,x), y) = (p, x, y)

colunas1' = organizaAlien colunas1 -- Organiza os as imagens aliens que serão ultilizados
colunas2' = organizaAlien colunas2

matriz pics =  map desenhaColuna pics -- Gerador de Matriz
 where
   desenhaColuna (pic, x, y) = translated (deslocamentoX*x) (deslocamentoY*y) (picToPicture pic)


matrizAlien timer -- Gera e alterna a Matriz de alien com base no tempo
 | timer <= 0.25      = matriz colunas1'
 | otherwise          = matriz colunas2'

      
visualizaAlien (x, y, timer, _) = translated x y $ translated (-7.5) 4 $ pictures $ matrizAlien timer -- junta tudo e coloca os aliens da forma desejada 

-- Atualiza o alien movendo para o lado e para baixo com base no tempo.

passaTempoAlien t (x, y, timer, d)
   | mudaTimer && (abs x >= 1)        = (x-deslocamentoX'*d, y-0.2, alienTimer ,-d )
   | mudaTimer                        = (x+deslocamentoX'*d, y, alienTimer, d)
   | otherwise                        = (x, y, timer - t, d)
   where
     mudaTimer = timer <= 0
     deslocamentoX' = 0.5
     
-- Visualiza Tudo

visualizaMundo :: World -> Picture
visualizaMundo (nv, ba, ali) = visualizaNave nv & visualizaBala ba & visualizaAlien ali
                         
                         
--Função que atualiza todas as informações do Mundo

atualizaMundo :: Event -> World -> World
atualizaMundo (KeyPress "Left")    w@(nv, _, _) = atualizeNave w (navePraEsquerda nv)
atualizaMundo (KeyRelease "Left")  w@(nv, _, _) = atualizeNave w (naveParada nv)
atualizaMundo (KeyPress "Right")   w@(nv, _, _) = atualizeNave w (navePraDireita nv)
atualizaMundo (KeyRelease "Right") w@(nv, _, _) = atualizeNave w (naveParada nv)
atualizaMundo (KeyPress " ") w@(nv, ba, ali) = atualizeBala w (projetil nv ba)
atualizaMundo (TimePassing t)      w@(nv, ba, ali) = (passaTempoNave t nv, passaTempoBala t nv ba,passaTempoAlien t ali)
atualizaMundo _ w = w

link codeWorld : https://code.world/haskell#Pi5sKfoKa79AV5iVeumqX_A