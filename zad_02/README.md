# Kompilator języka Latte
## Tomasz Kępa
## MRJP 2017/2018

Kompilatory zostały napisane w języku **Haskell**, budowane są za 
przy użyciu narzędzia **cabal**.

Wszystkie używane biblioteki zadeklarowane są w pliku `src/latte.cabal`. 
Jedyna biblioteka, która na `students` nie jest zainstalowana standardowo 
(albo jest w złej wersji) to `process`, którą można zainstalować przez wywołanie 
`cabal install process`

Projekt kompiluje się przez wywołanie `make` w głównym katalogu projektu.
`make mrproper` usuwa wszystkie pliki wygenerowane w czasie kompilacji.

Struktura projektu: 
    - wszystkie źródła znajdują się w katalogu `src`
    - kompilator, poza plikami generowanymi przez BNFC, składa się z dwóch plików 
        - jeden odpowiada za odpalenie leksera i parsera oraz obsługę IO 
        i wywołanie `gas` (`Compiler.hs`),
        drugi jest czysto funkcyjny i odpowiada za generację kodu (`Latte.h`)
