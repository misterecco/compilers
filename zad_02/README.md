# Kompilator języka Latte
## Tomasz Kępa
## MRJP 2017/2018

Kompilator został napisany w języku **Haskell**, budowany jest 
przy użyciu narzędzia **cabal**.

Wszystkie używane biblioteki zadeklarowane są w pliku `src/latte.cabal`. 
Jedyna biblioteka, która na `students` nie jest zainstalowana standardowo 
(albo jest w złej wersji) to `process`, którą można zainstalować przez wywołanie 
`cabal install process`

Projekt kompiluje się przez wywołanie `make` w głównym katalogu projektu.
`make mrproper` usuwa wszystkie pliki wygenerowane w czasie kompilacji.

Struktura projektu: 
    - wszystkie źródła znajdują się w katalogu `src`
    - kompilator, poza plikami generowanymi przez BNFC, składa się
      z następujących części  
        - `Compiler.hs` - zawiaduje wywałaniami innych modułów (lekser, parser,
          preprocesor) oraz obsługę IO (operacje na plikach, wywoływanie
          zewnętrznych programów) 
        - `PreState.hs` - definicja monady i pomocniczych funkcji używanych
          przez preprocesor
        - `Preprocessor.hs` - optymalizacja na poziomie AST, sprawdzanie
          poprawności programów 
