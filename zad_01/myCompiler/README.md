# Kompilatory języka Instant
## Tomasz Kępa
## MRJP 2017/2018

Kompilatory zostały napisane w języku **Haskell**, budowane są za 
przy użyciu narzędzia **cabal**.

Wszystkie używane biblioteki zadeklarowane są w pliku `src/instant.cabal`. 
Jedyna biblioteka, która na `students` nie jest zainstalowana standardowo 
(albo jest w złej wersji) to `process`, którą można zainstalować przez wywołanie 
`cabal install process`

Projekt kompiluje się przez wywołanie `make` w głównym katalogu projektu.
`make mrproper` usuwa wszystkie pliki wygenerowane w czasie kompilacji.

Struktura projektu: 
    - wszystkie źródła znajdują się w katalogu `src`
    - pliki generowane przez BNFC są wspólne
    - oba kompilatory mają taką samą strukturę - poza plikami generowanymi 
        przez BNFC składają się z dwóch plików - jeden odpowiada za odpalenie
        leksera i parsera oraz obsługę IO i wywołanie `llvm-as` / `jasmin`
        (są to odpowiednio `CompileLLVM.hs` i `CompileJVM.hs`), drugi
        jest czysto funkcyjny i odpowiada za generację kodu (`LLVMInstant.hs`
        / `JVMInstant.hs`)
    - w katalogu `lib` znajduje się najnowsza wersja programu jasmin `jasmin.jar`
