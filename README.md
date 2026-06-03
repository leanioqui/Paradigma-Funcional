[![build](https://github.com/pdep-mn-utn/tp-funcional-pdepedilo/actions/workflows/build.yml/badge.svg)](https://github.com/pdep-mn-utn/tp-funcional-pdepedilo/actions/workflows/build.yml)
========================================================================
PROYECTO: PARADIGMAS DE PROGRAMACIÓN - PARADIGMA FUNCIONAL (HASKELL)
========================================================================

DESCRIPCIÓN DEL PROYECTO
------------------------
Este proyecto contiene la resolución de la práctica de Paradigma Funcional 
para la materia Paradigmas de Programación (UTN-FRBA). El objetivo principal 
es modelar el dominio de videojuegos, eventos y temporadas aplicando conceptos 
avanzados del paradigma funcional sobre estructuras de datos puras.

TECNOLOGÍAS UTILIZADAS
----------------------
* Lenguaje: Haskell
* Herramienta de construcción: Stack
* Compilador: GHC (Glasgow Haskell Compiler)

CONCEPTOS CLAVE DEL PARADIGMA IMPLEMENTADOS
-------------------------------------------
1. Aplicación Parcial y Currificación:
   Permite fijar argumentos en funciones multiparámetro para generar nuevas 
   funciones intermedias dinámicamente.
   
2. Composición de Funciones ((.)):
   Utilizada para encadenar transformaciones complejas de manera declarativa 
   e impecable sin necesidad de abusar de variables explícitas o paréntesis.
   Ejemplo clave: (impacto . pasarTemporada temporada1) juego

3. Evaluación Diferida (Lazy Evaluation):
   Aprovechada para manipular de forma segura colecciones y flujos de datos 
   infinitos (como listas de parches infinitas o catálogos infinitos), 
   permitiendo que el motor de Haskell evalúe estructuras solo bajo demanda 
   y rompa la recursividad de manera óptima sin colgarse.

4. Funciones de Orden Superior:
   Uso intensivo de abstracciones nativas como `map`, `filter`, y abstracciones 
   propias parametrizadas con comportamientos dinámicos.

CÓMO EJECUTAR EL PROYECTO LOCALMENTE
------------------------------------
1. Asegurate de tener instalado `stack` en tu entorno de desarrollo.
2. Cloná o descargá tu repositorio privado de Git.
3. Abrí una terminal parada en la carpeta raíz del proyecto y ejecutá:
   ```bash
   > stack ghci
   ```
4. Una vez que levante el entorno interactivo, podés probar las consultas:

    ```stack
   > preciosAscendentes (take 4 (listaDeEventos temporada2024)) hollowKnight
   > (impacto . pasarTemporada temporada2023) hades
    ```
Entre muchas otras

------------------------------------------------------------------------
Desarrollado por Leandro Nicolas Quintela.
Universidad Tecnológica Nacional (UTN) - Ingeniería en Sistemas de Información.
