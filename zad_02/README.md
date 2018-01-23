# Kompilator języka Latte
## Tomasz Kępa
## MRJP 2017/2018

Kompilator został napisany w języku **Haskell**, budowany jest 
przy użyciu narzędzia **cabal**.

Wszystkie używane biblioteki zadeklarowane są w pliku `src/latte.cabal`. 
Jedyne biblioteki, która na `students` nie są zainstalowane standardowo 
(albo są w złej wersji) to `process` i `containers`, które można zainstalować
 przez wywołanie `cabal install process` oraz `cabal install containers`

Projekt kompiluje się przez wywołanie `make` w głównym katalogu projektu.
`make mrproper` usuwa wszystkie pliki wygenerowane w czasie kompilacji.

Struktura projektu: 
  - wszystkie źródła znajdują się w katalogu `src`
  - kompilator, poza plikami generowanymi przez BNFC, składa się
    z następujących części  
      - `Compiler.hs` - zawiaduje wywałaniami innych modułów (lekser, parser,
        preprocesor, itd.) oraz obsługę IO (operacje na plikach, wywoływanie
        zewnętrznych programów) 
      - `PreState.hs` - definicja monady i pomocniczych funkcji używanych
        przez preprocesor
      - `Preprocessor.hs` - optymalizacja na poziomie AST, sprawdzanie
        poprawności programów 
      - `IRDef.hs` - definicja typów danych reprezentujących kod czwórkowy
      - `IRGenState.hs` - definicja monady i pomocnicznych funkcji dla 
        generatora kodu czwórkowego
      - `IRGen.hs` - generator kodu czwórkowego
        - wyrażenia muszą wiedzieć czy są częścia `if`a w celu generowania
          kodu skaczącego w jednym kroku (kod czwórkowy nie zawiera && ani ||)
      - `CFG.hs` - transformacja kodu liniowego do Control Flow Graph
      - `SSA.hs` - transformacja CFG to postaci SSA
      - `CGDef.hs` - definicja typów danych reprezentucjących kod assemblera
      - `CGState.hs` - definicja modany i pomocniczych funkcji dla 
        generatora assemblera
      - `CodeGen.hs` - generator assemblera
      - `PeepHole.hs` - optymalizacja "przez dziurkę od klucza"

Konwencja wołania zgodna z linuxową dla architektury x86-64
Inty są 64-bitowe, stringi reprezentowane tak samo jak w C

Alokacja rejestrów (na poziomie pojedynczego bloku) robiona jest 
algorytmem LinearScan. Opis w pracy:
[Linear Scan Register Allocation](http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf)

Optymalizacje:
  - redukcja gałęzi if(true), if(false), while(false)
  - propagacja stałych, propagacja kopii
  - w przejściach między blokami żywe zmienne wpisywane są bezpośrednio do
    nowych rejestrów, nie występuje tutaj odsyłanie do pamięci, chyba że brakuje 
    rejestórw
  - funkcja odkłada na stos tylko te rejestry spośród resjtrów callee-saved,
    które rzeczywiście są używane w tej funkcji 
  - eliminacja martwego kodu
