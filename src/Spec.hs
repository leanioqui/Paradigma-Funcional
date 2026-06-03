module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
-- Test para la función impacto
  describe "\nimpacto" $ do
    it "Zelda: clásico que se reinventó (antes 1990, >3 exp)" $
      impacto zelda `shouldBe` 12000

    it "Pac-Man: valor de culto (antes 1990, <=3 exp)" $
      impacto pacman `shouldBe` 2200

    it "Hollow Knight: sin comunidad (sin expansiones)" $
      impacto hollowKnight `shouldBe` 250

    it "Cyberpunk: post-1990 con expansiones" $
      impacto cyberpunk `shouldBe` 4050
-- Test para la función expansionDeCulto      
  describe "\nexpansionDeCulto" $ do
    it "Alan Wake: tiene una expansión con 'Director' en el nombre?" $
      expansionDeCulto alanWake `shouldBe` True

    it "Death Stranding: tiene una expansión con 'Director' en el nombre?" $
      expansionDeCulto deathStranding `shouldBe` False

    it "Control: tiene una expansión con 'Director' en el nombre?" $
      expansionDeCulto control `shouldBe` False

    it "Hollow Knight: tiene una expansión con 'Director' en el nombre?" $
      expansionDeCulto hollowKnight `shouldBe` False
-- Test para la función esNicho    
  describe "\nesNicho" $ do
    it "¿Es esNicho Rimworld ? (max permitido 2)" $
      esNicho 2 rimworld `shouldBe` True
      
    it "¿Es esNicho Stellaris ? (max permitido 0)" $
      esNicho 0 stellaris `shouldBe` False
      
    it "¿Es esNicho Hades ? (max permitido 1)" $
      esNicho 1 hades `shouldBe` True
-- Test para la función esDenso  
  describe "\nesDenso" $ do
    it "Cyberpunk 2077 es denso con 3" $
      esDenso 3 cyberpunk `shouldBe` True

    it "The Legend of Zelda NO es denso con 3" $
      esDenso 3 zelda `shouldBe` False

    it "Hollow Knight es denso con 4" $
      esDenso 4 hollowKnight `shouldBe` True
-- Test para la función lanzarExpansion
  describe "\nlanzarExpansion" $ do
    it "Lanzar expansión 'Revenge' sobre 'Hades'" $
      expansiones (lanzarExpansion "Revenge" hades) `shouldBe` ["Revenge"]
    it "Lanzar expansión 'Revenge' sobre 'Hades'" $
      precio (lanzarExpansion "Revenge" hades) `shouldBe` 1100

    it "Lanzar expansión 'Blood and Wine' sobre 'The Witcher 3'" $
      expansiones (lanzarExpansion "Blood and Wine" witcher3) `shouldBe` ["Hearts of Stone","Blood and Wine"]
    it "Lanzar expansión 'Blood and Wine' sobre 'The Witcher 3'" $
      precio (lanzarExpansion "Blood and Wine" witcher3) `shouldBe` 2130

-- Test para la función parchear
  describe "\nparchear" $ do
    it "Parchear expansiones de largo >= 8 en 'Skyrim'" $
      expansiones (parchear 8 skyrim) `shouldBe` ["Dragonborn","Dawnguard"]
    it "Parchear expansiones de largo >= 8 en 'Skyrim'" $
      precio (parchear 8 skyrim) `shouldBe` 1200

    it "Parchear expansiones de largo >= 10 en 'Skyrim'" $
      expansiones (parchear 10 skyrim) `shouldBe` ["Dragonborn"]
    it "Parchear expansiones de largo >= 10 en 'Skyrim'" $
      precio (parchear 10 skyrim) `shouldBe` 1100

    it "Parchear expansiones de largo >= 15 en 'Skyrim'" $
      expansiones (parchear 15 skyrim) `shouldBe` []
    it "Parchear expansiones de largo >= 15 en 'Skyrim'" $
      precio (parchear 15 skyrim) `shouldBe` 1000

    it "Parchear expansiones de largo >= 5 en 'Hades'" $
      expansiones (parchear 5 hades) `shouldBe` []
    it "Parchear expansiones de largo >= 5 en 'Hades'" $
      precio (parchear 5 hades) `shouldBe` 1000
  
-- Test para la función remasterizar
  describe "\nremasterizar" $ do
    it "Remaster de Hades" $
      precio (remasterizar hades) `shouldBe` 1500
    it "Remaster de Hades" $
      titulo (remasterizar hades) `shouldBe` "Hades Remastered"
    it "Remaster de Age of Empire II" $
      precio (remasterizar ageOfEmpires) `shouldBe` 2200
    it "Remaster de Age of Empire II" $
      titulo (remasterizar ageOfEmpires) `shouldBe` "Age of Empires II Remastered"

-- Test para la función relanzar
  describe "\nrelanzar" $ do
    it "Relanzar Hades: precio" $
      precio (relanzar hades) `shouldBe` 2000

    it "Relanzar Hades: título" $
      titulo (relanzar hades) `shouldBe` "Hades Legacy"

    it "Relanzar Hades: expansiones" $
      expansiones (relanzar hades) `shouldBe` ["Director's Cut"]

    it "Relanzar Witcher 3: precio" $
      precio (relanzar witcher3) `shouldBe` 4000

    it "Relanzar Witcher 3: título" $
      titulo (relanzar witcher3) `shouldBe` "The Witcher 3 Legacy"

    it "Relanzar Witcher 3: expansiones" $
      expansiones (relanzar witcher3) `shouldBe` ["Hearts of Stone", "Director's Cut"]


--Test para pasarTemporada


  describe "\npasarTemporada" $ do
    it "Pasar temporada 2022 a The Witcher 3: Temporada = 2022 [parchear 8, remasterizar, relanzar]" $
      pasarTemporada (Temporada 2022 [parchear 8, remasterizar, relanzar]) witcher3  
      `shouldBe` Videojuego "The Witcher 3 Remastered Legacy" 2015 ["Hearts of Stone", "Director's Cut"] 6800

    it "Pasar temporada 2021 a The Witcher 3: Temporada = 2021 []" $
      pasarTemporada (Temporada 2021 []) witcher3  
      `shouldBe` witcher3

--Test para mejoroSegun
  describe "\nmejoroSegun" $ do
    it "Mejoro el criterio precio luego del evento parchear 10 en el juego 'Hollow Knight'" $
       hollowKnight `shouldNotSatisfy` mejoroSegun precio (parchear 10)

    it "Mejoro el criterio precio luego del evento lanzarExpansion 'DLC1' en el juego 'Hollow Knight'" $
       hollowKnight `shouldSatisfy` mejoroSegun precio (lanzarExpansion "DLC1")

--Test para aplicarEventosQueMejoranPrecio
  describe "\naplicarEventosQueMejoranPrecio" $ do
    it "Aplicar solo los eventos que mejoran el precio de la temporada 2022 sobre HollowKnight" $
       aplicarEventosQueMejoranPrecio (Temporada 2022 [parchear 8, remasterizar, relanzar]) hollowKnight
       `shouldBe` Videojuego "Hollow Knight Remastered Legacy" 2017 ["Director's Cut"] 3600

    it "Aplicar todos eventos que suben el precio sobre Hades" $
       aplicarEventosQueMejoranPrecio (Temporada 2026 [lanzarExpansion "X", remasterizar]) hades
       `shouldBe` Videojuego "Hades Remastered" 2020 ["X"] 1600

    it "Aplicar ningun evento que mejore el precio sobre HollowKnight" $
       aplicarEventosQueMejoranPrecio (Temporada 2025 [parchear 10]) hollowKnight
       `shouldBe` hollowKnight

    it "Aplicar temporada vacia sobre cualquier juego" $
       aplicarEventosQueMejoranPrecio (Temporada 2020 []) zelda
       `shouldBe` zelda

-- Test para aplicarEventosQueAchicanCatalogo
  describe "\naplicarEventosQueAchicanCatalogo" $ do
    it "Aplicar solo eventos que achican sobre Skyrim (solo se mantiene parchear 15)" $
       aplicarEventosQueAchicanCatalogo (Temporada 2022 [parchear 15, lanzarExpansion "X", remasterizar]) skyrim
       `shouldBe` Videojuego "Skyrim" 2011 [] 1000

    it "Múltiples eventos achican sobre Zelda (pasan ambos parches en orden)" $
       aplicarEventosQueAchicanCatalogo (Temporada 2022 [parchear 7, parchear 10]) zelda
       `shouldBe` Videojuego "The Legend of Zelda" 1986 [] 3100

    it "Ningún evento achica sobre Hades (queda sin cambios)" $
       aplicarEventosQueAchicanCatalogo (Temporada 2026 [lanzarExpansion "X", remasterizar]) hades
       `shouldBe` hades

    it "Temporada vacía sobre cualquier juego (queda sin cambios)" $
       aplicarEventosQueAchicanCatalogo (Temporada 2020 []) zelda
       `shouldBe` zelda

-- Test para aplicarEventosQueMejoranImpacto
  describe "\naplicarEventosQueMejoranImpacto" $ do
    it "Aplicar solo eventos que mejoran el impacto sobre Zelda (remasterizar y relanzar en orden)" $
       aplicarEventosQueMejoranImpacto (Temporada 2022 [parchear 8, remasterizar, relanzar]) zelda
       `shouldBe` Videojuego "The Legend of Zelda Remastered Legacy" 1986 ["Breath","Tears","Echoes","Awakening","Director's Cut"] 9800

    it "Ningún evento mejora el impacto sobre Hollow Knight (queda sin cambios)" $
       aplicarEventosQueMejoranImpacto (Temporada 2025 [parchear 5]) hollowKnight
       `shouldBe` hollowKnight

    it "Temporada vacía sobre cualquier juego (queda sin cambios)" $
       aplicarEventosQueMejoranImpacto (Temporada 2020 []) zelda
       `shouldBe` zelda

-- Test para preciosAscendentes
  describe "\npreciosAscendentes" $ do
    it "Un único evento que sube el precio sobre Hollow Knight" $
       hollowKnight `shouldSatisfy` preciosAscendentes [lanzarExpansion "X"]

    it "Un único evento que no sube el precio (parchear 10) sobre Hollow Knight" $
       hollowKnight `shouldNotSatisfy` preciosAscendentes [parchear 10]

    it "Lista de múltiples eventos que suben de forma acumulativa sobre Hollow Knight" $
       hollowKnight `shouldSatisfy` preciosAscendentes [lanzarExpansion "DLC1", remasterizar, relanzar]

    it "Un paso intermedio que no sube el precio (parchear 100) hace que falle" $
       hollowKnight `shouldNotSatisfy` preciosAscendentes [lanzarExpansion "X", parchear 100, relanzar]

-- Test para juegosOrdenadosPor
  describe "\njuegosOrdenadosPor" $ do
    it "Lista de juegos ordenada de forma ascendente según el precio base" $
       [hollowKnight, witcher3, cyberpunk] `shouldSatisfy` juegosOrdenadosPor precio

    it "Lista de juegos desordenada según el precio base hace que falle" $
       [hollowKnight, cyberpunk, witcher3] `shouldNotSatisfy` juegosOrdenadosPor precio


-- Test para impactosAscendentes
  describe "\nimpactosAscendentes" $ do
    it "Una sola temporada que sube el impacto sobre Hollow Knight" $
       hollowKnight `shouldSatisfy` impactosAscendentes [temporada2022]

    it "Una sola temporada vacía sobre cualquier juego hace que falle" $
       hollowKnight `shouldNotSatisfy` impactosAscendentes [temporadaVacia]

    it "Temporadas 2021, 2022 y 2023 aplicadas en cadena son estrictamente crecientes" $
       hollowKnight `shouldSatisfy` impactosAscendentes [temporada2021, temporada2022, temporada2023]

    it "Una temporada intermedia vacía que no sube el impacto hace que falle" $
       hollowKnight `shouldNotSatisfy` impactosAscendentes [temporada2022, temporadaVacia]