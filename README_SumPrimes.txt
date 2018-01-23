

algorytm oblicza sume tych podanych liczb, ktore sa pierwsze
parametry umieszczone sa w programie, aby poprawnie zbadac czas jego wykonania 

ex sumPrimary 99839 99961


WINDOWS:

ghc --make -threaded nazwa.hs
nazwa.exe +RTS -N2 
 
-- -N2 dla dwóch rdzeni, wtedy program dziala najszybciej (dwa rozgalezienia)
-- -N4 dla 4 - wtedy szybciej sumuje liczby w przedziale (wiecej rozglezien)

