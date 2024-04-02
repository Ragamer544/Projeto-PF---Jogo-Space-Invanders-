{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Prelude
import System.Random
import qualified Data.Text as T

--------------------[Tipos e registros]--------------------

type Alien     = Entity
type Bullet    = Entity
type Player    = Entity

data GameMode = Menu | Playing | GameOver deriving (Show,Eq,Ord)

data Entity    = Entity {ip      :: Int,
                         point   :: Point,
                         vector  :: Vector,
                         hBase   :: Double,
                         hHeight :: Double,
                         life    :: Int,    
                         score   :: Int,
                         timer1  :: Double, -- no player: usado para cooldown do tiro do player
                         timer2  :: Double, -- no player: usado para animar a morte do player, nos aliens: usado para animar a morte do alien
                         timer3  :: Double,
                         bool1   :: Bool    -- no player: usado para travar movimento do player quando ele morre, nos aliens: usado para alternar picture dos aliens
                         } deriving (Show,Eq,Ord)
                    
data World = World { player          :: Entity, 
                     bullets         :: [Entity], 
                     aliens          :: [Entity],
                     timer10         :: Double, -- usado na movimentaçao e animacao dos aliens
                     timer11         :: Double, -- usado para renovar orda de aliens e mover player pro centro
                     timer12         :: Double, -- usado para fazer os aliens atirarem
                     bool10          :: Bool,   -- usado para fazer os aliens atirarem
                     sprain          :: Double,
                     alienShip       :: Entity,  -- Novo campo para a nave alienígena
                     playerLives     :: Int,
                     gamemode        :: GameMode,
                     difficulty      :: Double, 
                     assorted        :: [Int],
                     crazy           :: [Int]
                     } deriving (Show,Eq,Ord)

--------------------[Mundo Inicial & constantes]--------------------                    
                 
-----Constantes

constTimeDeathPlayer         = 2    -- tempo de tela da morte do player
constTimeDeathAlien          = 0.5  -- Tempo de tela da morte do alien
constTimeToResetScreen       = 1    -- Tempo para reiniciar a tela
constQuantityOfAliensPerLine = 11   -- Quantidade de aliens por linha, minimo 1 maximo 11
constTimeShotPlayer          = 1.5  -- intervalo minimo entre disparos
constSizeDrop                = 0.5  -- o quanto os mobs descem ao bater na lateral 
constAliensUpdateTimer       = 0.5  -- tempo de atualizacao dos aliens
constSprainPerFrame          = 0.2  -- o quanto os mobs andam a cada passagem de tempo -- velocidade lateral dos mobs
constDistributionFromX       = -6.5 -- é onde a grade inicia, da esquerda pra direita
constSpaceBetweenAliensX     = 1.5  -- espacamento entre coordenadas X dos pontos centrais dos mobs
constInitialHeightOfAliens   = 7.5  -- altura maxima da grade de mobs
constSpaceBetweenAliensY     = 1.2  -- espacamento entre coordenadas Y dos pontos centrais dos mobs 
constFiringForce             = 9    -- velocidade da bala em Y do player
constBulletLateralMovement   = 50   -- o quanto a velocidade em X da nave afeta a velocidade em X da bala (unidade em porcentagem)              
constPlayerSpeed             = 5    -- velocidade lateral do player
constTimeToShotBulletAlien   = 0.5  -- Tempo minimo para o alien disparar o projétil          
constPowerBulletOfAlien        = 8    -- forca da Bala em y do alien
constTimeBetweenAlienShips   = 15   -- Tempo mínimo entre aparições de naves alienígenas
constTimerAlienShipShot      = 4    -- Tempo minimo pra alien ship atirar
constTimeDeathBullet         = 0.3  -- tempo de tela da morte da bala
constAddDifficulty           = 6    -- dobra a dificuldade apos quantas ordas elimininadas (vai aumentando aos poucos)
consthHeightAlienShip        = 8    -- altura em que passa a nave alen

defaultPlayerShot = defaultShot {ip=1,life=1,hBase=0.08, hHeight=0.2} 
defaultAlienShot = defaultShot {ip = 10}
defaultAlienShipShot = defaultShot {ip = 9} 
  
defaultShot = Entity { ip      = 10,  --sujeito a alteracoes
                       point   = (0,0),
                       vector  = (0,0),
                       hBase   = 0.2,
                       hHeight = 0.4,
                       life    = 1,
                       score   = 0,
                       timer1  = 0,
                       timer2  = constTimeDeathBullet,
                       timer3  = 0,
                       bool1   = False}           

alien0 = Entity { ip      = 20,  --sujeito a alteracoes
                  point   = (0,0),
                  vector  = (0,0),
                  hBase   = 0.5,
                  hHeight = 0.5,
                  life    = 1,
                  score   = 0,
                  timer1  = 0,
                  timer2  = constTimeDeathAlien,
                  timer3  = 0,
                  bool1   = False}  
                                      
alien1           = alien0 {ip = 11 , score = 40}
alien2           = alien0 {ip = 12 , score = 20, hBase = 0.6}
alien3           = alien0 {ip = 13 , score = 10, hBase = 0.6}  
defaultAlienShip = alien0{ip = 14 , point = (0,-15), timer1 = constTimeBetweenAlienShips,
                          hBase   = 0.8, hHeight = 0.45}
                          
defaultAlienDeath = alien0{ip=21}    
    
-----Mundo inicial

player1 = Entity { ip      = 0,
                   point   = (0,-9),
                   vector  = (0,0),
                   hBase   = 0.65,
                   hHeight = 0.7,
                   life    = 1,
                   score   = 0,
                   timer1  = 0,
                   timer2  = constTimeDeathPlayer,
                   timer3  = 0,
                   bool1   = False}

defaultWorld = initialWorld {playerLives = -1}

initialWorld = World { player          = player1,
                       bullets         = [],
                       aliens          = [],
                       timer10         = constAliensUpdateTimer,
                       timer11         = constTimeToResetScreen,
                       timer12         = constTimeToShotBulletAlien,
                       sprain          = constSprainPerFrame,
                       alienShip       = defaultAlienShip ,
                       bool10          = True,
                       playerLives     = 3,
                       gamemode        = Menu,
                       difficulty      = 1,
                       assorted        = [],
                       crazy           = []}

--------------------[Main]--------------------

main = do
       g <- newStdGen
       h <- newStdGen
       let rnds = randomRs (2,12) g :: [Int] 
       let crr = randomRs (-9,9) h :: [Int]
       debugActivityOf initialWorld {assorted = rnds,crazy = crr}
                       update
                       view  

--------------------[update world]--------------------

update :: Event -> World -> World 
update (KeyPress "K")   w@World{..} = changeGameMode w
update (KeyPress "Right")   w@World{..} = applyPlayerVector constPlayerSpeed w 
update (KeyRelease "Right") w@World{..} = applyPlayerVector 0 w
update (KeyPress "Left")   w@World{..} = applyPlayerVector (-constPlayerSpeed) w
update (KeyRelease "Left") w@World{..} = applyPlayerVector 0 w
update (KeyPress " ")   w@World{..} = shootBulletPlayer w
update (TimePassing dt) w@World{..} = updateWorld dt w
update _                w = w

updateWorld :: Double -> World -> World
updateWorld dt world =  applyAlienShip         .
                        applyDifficulty        . 
                        applySwitchGameMode    .
                        applyKillWorld         .
                        applyWorldTimers dt    .  
                        applyEnemyBullets      .
                        applyWorldCollisions   .
                        applyWorldMovements dt .
                        applyPlayerLives       $ world
                         
--------------------[Vizualiza Mundo]--------------------
 
view :: World -> Picture
view w@World{..} 
  | gamemode == Menu    = drawMenu w
  | gamemode == Playing = drawPlaying w
  | otherwise           = drawGameOver w
    
drawMenu :: World -> Picture
drawMenu World{..} = (move (0,8) . colored white . lettering . T.pack $ "Space Invaders") & 
                     (move (0,6.5) . colored white . lettering . T.pack $ "Press K to start") & 
                     (move (0,-9) . colored white . lettering . T.pack $ "Develope By : Lucas Calumbi e Rafael Gomes") & 
                     allPictures &
                     retanguloS 21 21 
  where
    allPictures = pictures (zipWith move points (pics++pics))
    points = zip as bs
    (as,bs) = splitAt 25 . map fromIntegral . take 50 $ crazy
    pics = [alien1'1,alien1'2,alien2'1,alien2'2,alien3'1,alien3'2,alienShip',mario'1]
    
drawGameOver :: World -> Picture 
drawGameOver World{..} = 
                         (move (0,6) . colored white . lettering . T.pack $ fraseMotivadora) &
                         (move (0,4) . colored white . lettering . T.pack $ "Your score: " ++ show (score player)) & 
                         (move (0,2) . colored white . lettering . T.pack $ "Press K to go back to Menu") & 
                         retanguloS 21 21 
  where
    (r1:rs) = assorted
    fraseMotivadora 
      | r1 <= 3   = "Tomou na jabiraca"
      | r1 <= 5   = "Bem-vindo ao gigante da colina"
      | r1 == 6   = "Você foi de submarino"
      | r1 == 7   = "Você foi de arrasta pra cima"
      | r1 == 8   = "Você foi de americanas"
      | r1 == 9   = "bem-vindo ao mundo da equipolência"
      | r1 == 10  = "Faltou fé no amigo"
      | r1 == 11  = "você apertou a mão de Irandilson"
      | otherwise = "você confiou demais no amigo"
     
     
drawPlaying :: World -> Picture  
drawPlaying World{..} = entitys & 
                        playerScore & 
                        playersLife & 
                        retanguloS 21 21       
  where
    playersLife          = pictures . zipWith move [(-9,9),(-8,9),(-7,9)] . replicate playerLives $ heart  
    playerScore          = move (7,9) . colored white . lettering . T.pack $ "Score: " ++ show (score player) -- Converte a pontuação em texto
    entitys              = pictures (map drawEntity ([player] ++ bullets ++ aliens ++ [alienShip])) 
  
     
drawEntity :: Entity -> Picture
drawEntity enti@Entity {..} 
  | ip == 0    = move point ship1 
  | ip == 1    = bullet red 
  | ip == 8    = move point deathAlienBullet'
  | ip == 9    = move point bulletAlienShip'
  | ip == 10   = alternateDraw alienBullet1' alienBullet2' 
  | ip == 11   = alternateDraw alien1'1 alien1'2 
  | ip == 12   = alternateDraw alien2'1 alien2'2 
  | ip == 13   = alternateDraw alien3'1 alien3'2 
  | ip == 14   = move point alienShip' 
  | ip == 15   = move point mario'1 
  | ip == 16   = drawLuckyScore
  | ip == 21   = move point alienDead' 
  | otherwise  = blank 
  where 
    bullet cor              = move point . colored cor $ retanguloS (2*hBase) (2*hHeight)
    alternateDraw pic1 pic2 = move point $ alternatePic pic1 pic2
    alternatePic pic1 pic2  = if bool1 then pic1 else pic2
    hitBox                  = move point . colored gray $ solidRectangle (2*hBase) (2*hHeight)
    drawLuckyScore          = move point . colored white . lettering . T.pack . show $ score 
    
--------------------[Funcoes gerais]-------------------- 

-----Funcao para atualizar os timers

applyWorldTimers :: Double -> World -> World
applyWorldTimers dt world@World{..} = 
  resetAlienOrder world{player      = deathEntityTimer . applyEntiCooldown $ player,
                        bullets     = map deathEntityTimer . map applyAlternate $ bullets,
                        aliens      = map deathEntityTimer . map applyAlternate $ aliens,
                        timer10     = resetTimer timer10 constAliensUpdateTimer,
                        timer12     = newTimer12,
                        bool10      = newBool10,
                        alienShip   = deathEntityTimer . applyTimer3AlienShip . applyEntiCooldown $ alienShip} 
  where
  applyEntiCooldown p@Entity{..}  = p{timer1    = timer1 - dt}
  applyAlternate alien@Entity{..} = alien{bool1 = negateBool timer10 bool1}
  
  negateBool p q
     | p <= 0     = not q
     | otherwise  = q
  
  resetTimer timer constan 
     | timer <= 0 = constan/difficulty
     | otherwise  = timer - dt 
     
  resetAlienOrder world
     | null aliens && timer11 <= 0 && not (bool1 player) 
                                   = world{timer11 = constTimeToResetScreen,
                                           aliens  = createAlienOrder,
                                           player  = player {point = (0,-9)}}
     | null aliens                 = world{timer11 = timer11 - dt }
     | otherwise                   = world
     
  deathEntityTimer enti@Entity{..} = if life <= 0 then enti{timer2 = timer2-dt} else enti
  
  (newTimer12,newBool10) = applyAlienTimerShot timer12 bool10  
  applyAlienTimerShot timer bool
    | timer <= 0                   = (constTimeToShotBulletAlien + 0.2 * fromIntegral r1,True)
    | otherwise                    = (timer-dt,bool)
  (r1:r2:rs)                       = assorted  
  
  applyTimer3AlienShip aS@Entity{..}
    | timer3 <= 0 = aS{timer3 = constTimerAlienShipShot,bool1 = True}
    | otherwise   = aS{timer3 = timer3-dt}
  
-----Funcao para movimentar as entidades

putVector :: Double -> Point -> Vector -> Point
putVector dt (x,y) (vx,vy) = (x+vx*dt,y+vy*dt)

applyWorldMovements :: Double -> World -> World
applyWorldMovements dt w@World{..} = 
  if (bool1 player) then w else w{player    = playerMov dt player, 
                                  bullets   = map (entityMov dt) bullets, 
                                  aliens    = newAliens,
                                  sprain    = desl,
                                  alienShip = entityMov dt alienShip}
    where
      (newAliens,desl) = aliensMov dt timer10 sprain aliens

playerMov :: Double -> Player -> Player 
playerMov dt player@Entity{point = p@(x,y), vector = v@(vx,vy), bool1 = locked} 
   | (x >= 9 && vx > 0) || (x <= (-9) && vx < 0) || locked = player 
   | otherwise                                             = player{ point = putVector dt p v}

entityMov :: Double -> Entity -> Entity 
entityMov dt bullet@Entity{..} = if life <= 0 then bullet else bullet{ point = putVector dt point vector} 

aliensMov :: Double -> Double -> Double -> [Alien] -> ([Alien],Double)
aliensMov dt timer sprain aliens 
  | updated && xr >= 9    = (map (moveAlien (-constSprainPerFrame) constSizeDrop) aliens, -sprain)
  | updated && xl <= (-9) = (map (moveAlien   constSprainPerFrame  constSizeDrop) aliens, -sprain)
  | updated               = (map (moveAlien           sprain             0      ) aliens,  sprain)
  | otherwise             = (aliens,sprain)
  where
    (alienL@Entity{point = (xl,yl)},alienR@Entity{point = (xr,yr)}) = referenceAliens aliens
    updated = timer <= 0
    moveAlien sprain fall enti@Entity{point = (x,y),life=l}
      | l <= 0    = enti
      | otherwise = enti {point = (x+sprain,y-fall)}
    
referenceAliens :: [Alien] -> (Alien,Alien) 
referenceAliens []     = (alien0,alien0)
referenceAliens aliens = (foldr1 (f (\x y -> x < y)) $ aliens, foldr1 (f (\x y -> x > y)) aliens)
  where                   --alien mais a esquerda               --alien mais a direita
    f g x@Entity{point = (x1,y1)} y@Entity{point = (x2,y2)} 
      | x1 `g` x2 && y1 >= y2 = x
      | x1 `g` x2             = x
      | otherwise             = y

-----Funcao para verificar colisoes 

applyWorldCollisions :: World -> World -- aplica todas as colisoes do mundo
applyWorldCollisions w@World{..} = w{player    = head player1,
                                     bullets   = bullets4, 
                                     aliens    = aliens1, 
                                     alienShip = head alienShip1}
  where
    (bullets1,_)          = collisionEntiXEnti bullets bullets
    (bullets2,aliens1)    = collisionEntiXEnti bullets1 aliens
    (bullets3,alienShip1) = collisionEntiXEnti bullets2 [alienShip]
    (bullets4,player1)    = collisionEntiXEnti bullets3 [player]

myFoldr :: (a -> b -> a) -> a -> [b] -> a
myFoldr f x [] = x
myFoldr f x (y:ys) = myFoldr f (f x y) ys
  
productCartesian :: (a -> b -> a) -> [a] -> [b] -> [a]
productCartesian f [] _      = []
productCartesian f (x:xs) ys = (myFoldr f x ys) : productCartesian f xs ys

collisionEntiXEnti :: [Entity] -> [Entity] -> ([Entity],[Entity])
collisionEntiXEnti enti1 enti2 = (productCartesian collisionEntiEnti enti1 enti2, productCartesian collisionEntiEnti enti2 enti1)
  where
    collisionEntiEnti enti1@Entity{point = (x1,y1), hBase = b1, hHeight = h1, ip = ip1,life = l1} 
                      enti2@Entity{point = (x2,y2), hBase = b2, hHeight = h2, ip = ip2,life = l2}
      | l2 <= 0 = enti1              
      | not (friendly1 && friendly2 || enemy1 && enemy2) 
        && (distanceIn x1 x2 <= b1+b2) && (distanceIn y1 y2 <= h1+h2) = enti1 {life = l1 - l2}
      | otherwise                                                     = enti1
      where
        distanceIn a b = max a b - min a b   
        friendly1      = ip1 <= 5
        friendly2      = ip2 <= 5
        enemy1         = ip1 >= 8 
        enemy2         = ip2 >= 8

-----Funcao para filtrar entidades vivas  
  
applyKillWorld :: World -> World
applyKillWorld w@World{..} = w{ aliens     = activeAliens,
                                bullets    = activeBullets,
                                player     = pWithScore modifiedPlayer,
                                alienShip  = activeAlienShip}
  where 
    activeAliens       = filter (\alien -> (timer2 alien) > 0) modifiedAliens 
    modifiedAliens     = map (\alien -> if (life alien) > 0 then alien else alien{ip=21}) aliveAliens
    modifiedPlayer     = if (life player) > 0 then player else player{ip=21} 
    activeBullets      = filter (\bullet -> (timer2 bullet) > 0 && insidePlane (point bullet)) modifiedBullets    
    modifiedBullets    = map (\bullet -> if (life bullet) > 0 then bullet else bullet{ip=8}) bullets
    activeAlienShip    = if (timer2 modifiedAlienShip) > 0 then modifiedAlienShip else defaultAlienShip   
    modifiedAlienShip  = if (life alienShip) > 0 then alienShip else alienShip{ip=16} 
    insidePlane (x,y)  = abs x <= 10 && abs y <= 10 
    
    playerScore       = (+) scoreAlienShip . sum $ map (\alien -> score alien) deadAlien       -- Calcule a pontuação ganha destruindo os aliens
    pWithScore      p = p { score = score p + playerScore }                                    -- Atualize a pontuação do jogador
    
    (aliveAliens,deadAlien) = aliveDeadAliens aliens
    aliveDeadAliens [] = ([],[])
    aliveDeadAliens (x@Entity{..}:xs)
      | life <= 0 && timer2 <= 0 =(x:as,x:bs)
      | otherwise =(x:as,bs)
      where
        (as,bs)= aliveDeadAliens xs
        
    (scoreAlienShip,newAlienShip) = luckyScoreAlienShip alienShip
    luckyScoreAlienShip alien
      | timer2 alienShip <= 0 = ((score alien), defaultAlienShip)
      | otherwise           = (0,alienShip)
    
-----Funcao para aplicar as vidas do player

applyPlayerLives :: World -> World
applyPlayerLives w@World{..}
  | (timer2 player) <= 0 = putScore initialWorld{assorted = assorted, playerLives = playerLives - 1, aliens = aliens,gamemode = Playing}
  | (bool1 player)       = w{timer10 = 0.5,bullets = []}
  | (life player)   <= 0 = w{player = player{bool1 = True}}  
  | otherwise            = w
   where
    ss = score player
    putScore m@World{..} = m{player = player{score = ss}}
-----Funcao para aplicar a troca de gamemode

applySwitchGameMode :: World -> World
applySwitchGameMode w@World{..}
  | gamemode == Menu            = defaultWorld{ gamemode = Menu, assorted = assorted,crazy=crazy}
  | gamemode == GameOver        = defaultWorld{ player = player, gamemode = GameOver, assorted = assorted,crazy=crazy}
  | playerLives == 0 || abs (y-yl) <= 0.2 = w{gamemode = GameOver}
  | otherwise                   = w
  where 
    (_,y) = (point player)
    alien@Entity{point = (_,yl)} = theLowestAlien aliens
    theLowestAlien [] = alien0
    theLowestAlien xs = foldr1 f xs
      where
        f x@Entity{point = (x1,y1)} y@Entity{point = (x2,y2)}
          | y1 < y2   = x
          | otherwise = y
  
changeGameMode :: World -> World
changeGameMode w@World{..} 
  | gamemode == Menu     = initialWorld{gamemode = Playing, assorted = assorted}
  | gamemode == GameOver = w{gamemode = Menu}
  | otherwise            = w
  
----- Funcao para aumentar a dificuldade do jogo  
  
applyDifficulty :: World -> World 
applyDifficulty w@World{..} = w{difficulty = newDifficulty}
  where
    newDifficulty = 1 + fromIntegral (score player) / (constAddDifficulty*1100)
  
  
--------------------[Funcoes do Player]--------------------    
    
-----Funcao para alterar velocidade lateral do player    
    
applyPlayerVector :: Double -> World -> World
applyPlayerVector v w@World{..} = w{ player = player{vector = (v,0)}}
    
-----Funcao para disparar bala padrao do player    
    
shootBulletPlayer :: World -> World 
shootBulletPlayer w@World{..}
  | (timer1 player) <= 0 = w {player  = player{timer1 = constTimeShotPlayer},
                              bullets = defaultPlayerShot {point = (point player),
                              vector  = shot (vector player)} : bullets}
  | otherwise            = w 
  where
    shot (vx,vy)         = (vx*constBulletLateralMovement/100,vy+constFiringForce)
    
--------------------[Funcoes dos aliens]--------------------   

-----Funcao para gerar grade de aliens   
   
createAlienOrder :: [Alien]
createAlienOrder = concat $ zipWith sortedLineOfAliens [constInitialHeightOfAliens,constInitialHeightOfAliens - constSpaceBetweenAliensY..] [alien1,alien2,alien2,alien3,alien3]   

sortedLineOfAliens :: Double -> Alien -> [Alien]
sortedLineOfAliens h alien = zipWith order1Alien
                              (zip [constDistributionFromX,constDistributionFromX+constSpaceBetweenAliensX..] . replicate constQuantityOfAliensPerLine $ h) .
                              replicate constQuantityOfAliensPerLine $ alien
  where
    order1Alien (x,y) alien = alien{point = (x,y)}
    
    
----Funcao para aplicar a AlienShip 

applyAlienShip :: World -> World --defaultAlienShipShot
applyAlienShip w@World {..} 
  | bool1 alienShip                    = w {alienShip = alienShip {bool1 = False}, bullets = defaultAlienShipShot{point=(x,y),vector = (0,-constPowerBulletOfAlien)}:bullets}
  | abs x > 11                         = w {alienShip = defaultAlienShip}
  | ableApparitionAlienShip && lucky   = w {alienShip = newAlienShip }
  | ableApparitionAlienShip            = w {alienShip = newAlienShip }
  | otherwise                          = w
  where
   (x,y) = point alienShip
   ableApparitionAlienShip       = (timer1 alienShip) + fromIntegral r3 / 2 <= 0  
   (r1:r2:r3:r4:r5:r6:r7:r8:rs)  = assorted
   luckyIp      = if (mod (r2+r3) 3) == 0 then 15 else 14 
   lucky        = mod r4 3 == 0
   newAlienShip = defaultAlienShip { vector = (v,0), point = (k,consthHeightAlienShip),timer1 = constTimeBetweenAlienShips, ip = luckyIp , score = r6*20} 
   k            = if even (r1+r4) then 11 else -11
   v            = if k > 0 then -3 else fromIntegral 3 
    
-----Funcao para disparar aleatoriamente o projétil do alien

applyEnemyBullets :: World -> World 
applyEnemyBullets w@World{..}
  | bool10 && shooterAliens /= [] && not (bool1 player) 
                                  = w {bullets  = (createAlienBullet luckyAlien) : bullets,
                                       bool10   = False,
                                       assorted = rs}
  | otherwise                     = w 
  where
    luckyAlien              = shooterAliens !! mod r2 (length shooterAliens)   
    shooterAliens           = filter (\alien -> (ip alien) == 11 ) aliens
    createAlienBullet alien = defaultAlienShot {point = (point alien), vector = (0,-constPowerBulletOfAlien)}
    (r1:r2:rs)              = assorted

--------------------[Funcoes auxiliares]--------------------

move :: Point -> Picture -> Picture
move (x,y) pic = translated x y pic

retanguloS b h = solidRectangle b h

--------------------[Desenhar Bit-map]--------------------
               
picToPicture :: [[Char]] -> Picture
picToPicture xs = scaled 0.1 0.1 . converteDados . transformaEmDados $ xs

converteDados :: [(Double,Double,Char)] -> Picture
converteDados xs = pictures . map converte $ xs
  where
    converte (x,y,p) 
      | p == '#'  = quadrado black
      | p == 'v'  = quadrado red
      | p == 'b'  = quadrado white
      | p == 'a'  = quadrado (light blue)
      | p == 'z'  = quadrado (dark blue)
      | p == 'y'  = quadrado yellow
      | p == 'm'  = quadrado brown
      | p == 'g'  = quadrado gray
      | otherwise = blank
       where
         quadrado cor = move (x,y) . colored cor $ retanguloS 1.1 1.1

transformaEmDados :: [[Char]] -> [(Double,Double,Char)]   
transformaEmDados xs@(c:cs) = concat . map ordena . botaCoordenadaY . map botaCoordenadaX $ xs 
  where
    t                  = fromIntegral . length $ xs
    u                  = fromIntegral . length $ c
    ordena (y,xps)     = map (criaTripla y) xps
    criaTripla y (x,p) = (x,y,p)
    botaCoordenadaY    = zip [t/2,(t-2)/2..(-t/2)]
    botaCoordenadaX    = zip [(-u/2+0.5)..]

--------------------[Matrizes dos aliens]--------------------

alien1'1 = picToPicture alien1_1 
alien1_1 =   -- 11x8
 ["    bbb    ",
  "   bbbbb   ",
  "  bbbbbbb  ",
  " bb bbb bb ",
  " bbbbbbbbb ",
  "  b bbb b  ",
  " b       b ",
  "  b     b  "]

alien1'2 = picToPicture alien1_2 
alien1_2 =
 ["    bbb    ",
  "   bbbbb   ",
  "  bbbbbbb  ",
  " bb bbb bb ",
  " bbbbbbbbb ",
  "   b   b   ",
  "  b bbb b  ",
  " b b   b b "]

alien2'1 = picToPicture alien2_1
alien2_1 =  
 [" b       b ",
  "  b     b  ",
  "  bbbbbbb  ",
  " bb bbb bb ",
  "bbbbbbbbbbb",
  "b bbbbbbb b",
  "b b     b b",
  "  bb   bb  "]

alien2'2 = picToPicture alien2_2
alien2_2 = 
 ["  b     b  ",
  "b  b   b  b",
  "b bbbbbbb b",
  "bbb bbb bbb",
  " bbbbbbbbb ",
  " bbbbbbbbb ",
  "  b     b  ",
  "bbb     bbb"]

alien3'1 = picToPicture alien3_1
alien3_1 = 
 ["    bbb    ",
  " bbbbbbbbb ",
  "bbbbbbbbbbb",
  "bb  bbb  bb",
  "bbbbbbbbbbb",
  "  bb   bb  ",
  " bb bbb bb ",
  "bb       bb"]

alien3'2 = picToPicture alien3_2
alien3_2 = 
 ["    bbb    ",
  " bbbbbbbbb ",
  "bbbbbbbbbbb",
  "bb  bbb  bb",
  "bbbbbbbbbbb",
  "  bb   bb  ",
  "bb  bbb  bb",
  " bb     bb "]

heart = scaled 0.8 0.8 . picToPicture $ heart'
heart' = -- 16x16
  ["   ###    ###   ",
   "  #vvv#  #vvv#  ",
   " #vvbvv##vvvvv# ",
   "#vvbvvvvvvvvvvv#",
   "#vbvvvvvvvvvvvv#",
   "#vvvvvvvvvvvvvv#",
   "#vvvvvvvvvvvvvv#",
   " #vvvvvvvvvvvv# ",
   " #vvvvvvvvvvvv# ",
   "  #vvvvvvvvvv#  ",
   "   #vvvvvvvv#   ",
   "    #vvvvvv#    ",
   "     #vvvv#     ",
   "      #vv#      ",
   "       ##       "]
   
ship1 = picToPicture ship_1
ship_1 = -- 17x16
  ["       #       ",
   "      #v#      ",
   "     #vvv#     ",
   "    #vvvvv#    ",  
   "    #aaaaa#    ",
   "    #azyza#    ",
   "    #zyyyz#    ",
   "   ##azyza##   ",
   "  #z#zzzzz#z#  ",
   " #az#######za# ",
   "#aazz#zzz#zzaa#",
   " #### ### #### ",
   " vyv vyyyv vyv ",
   " vyv vvyvv vyv ",
   "  v   vvv   v  ",
   "       v       "]

ship2 = picToPicture ship_2
ship_2 = -- 16x16
  ["       #       ",
   "      #v#      ",
   "     #vvv#     ",
   "    #vvvvv#    ",  
   "    #aaaaa#    ",
   "    #azyza#    ",
   "    #zyyyz#    ",
   "   ##azyza##   ",
   "  #z#zzzzz#z#  ",
   " #az#######za# ",
   "#aazz#zzz#zzaa#",
   " #### ### #### ",
   " vyv vyyyv vyv ",
   " vyv vvyvv vyv ",
   "  v   vvv   v  ",
   "       v       "]   
   
mario'1 = picToPicture mario1   
mario1 = -- 16x16
  ["     vvvvv      ",
   "    vvvvvvvvv   ",
   "    mmmbbgb     ",
   "   mbmbbbgbbb   ",  
   "   mbmmbbbmbbb  ",
   "    mbbbbmmmm   ",
   "    vvavvavv    ",
   "   vvvavvavvv   ",
   "  vvvvaaaavvvv  ",
   "  bbvayaayavbb  ",
   "  bbbaaaaaabbb  ",
   "  bbaaaaaaaabb  ",
   "    aaa  aaa    ",
   "    mmm  mmm    ",
   "   mmmm  mmmm   "]

alienDead' = picToPicture alienDead
alienDead =  
 ["     b      ",
  " b   b  b   ",
  "  b     b  b",
  "   b   b  b ",
  "bb          ",
  "          bb",
  " b  b   b   ",
  "b  b     b  ",
  "   b  b   b ",
  "      b     "]
  
alienShip'  = picToPicture alienShip''
alienShip'' =
 ["12345vvvvvv23456",
  "123vvvvvvvvvv456",
  "12vvvvvvvvvvvv56",
  "1vv4vv7vv0vv3vv6",
  "vvvvvvvvvvvvvvvv",
  "12vvv678901vvv56",
  "123v56789012v456"]
  
bulletAlienShip' = picToPicture bulletAlienShip
bulletAlienShip =
 ["vvv",
  "1v3",
  "1v3",
  "1v3",
  "1v3",
  "1v3"]
  
alienBullet1' = picToPicture alienBullet1
alienBullet1 =
 ["b23",
  "1b3",
  "12b",
  "1b3",
  "b23",
  "1b3"]
  
alienBullet2' = picToPicture alienBullet2
alienBullet2 =
 ["12b",
  "1b3",
  "b23",
  "1b3",
  "12b",
  "1b3"]
  
deathAlienBullet' = picToPicture deathAlienBullet
deathAlienBullet =
 ["b2b4",
  "1b3b",
  "b23b",
  "b2b4",
  "1b3b"]  

link do CodeWorlds : https://code.world/haskell#Po0vfHp_GLnqmC9rNI0TXcw
  