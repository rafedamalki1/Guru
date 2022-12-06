/*1XDARIN5.p*/
{AMERICANEUROPEAN.I}
OUTPUT TO f:\elpnj\darwin\sven\sydkraft\stor.txt 
CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
NO-ECHO.
FOR EACH storningstab no-lock:
    PUT UNFORMATTED 
       storningstab.foret-id ";" storningstab.distr-id ";" storningstab.lopnr-id ";"
       storningstab.rapp-typ ";" storningstab.dat-id ";" storningstab.tid-id FORMAT "9999" ";"
       storningstab.C1 ";" storningstab.C2 ";" storningstab.D1 ";" storningstab.D2 ";"
       storningstab.E1 ";" storningstab.E2 ";" storningstab.F1 ";" storningstab.F2 ";"
       storningstab.G1 ";" storningstab.G2 ";" storningstab.H ";" 
       storningstab.J1 ";" storningstab.J2 ";" storningstab.J3 ";"
       storningstab.K1 ";" storningstab.K2 ";" storningstab.L1 ";" storningstab.L2 ";"
       storningstab.M1 ";" storningstab.M2 ";" storningstab.N1 ";" storningstab.N2 ";"
       storningstab.P1 ";" storningstab.P2 ";" storningstab.Q ";" storningstab.R ";"
       storningstab.S ";" storningstab.T1 ";" storningstab.T2 ";" storningstab.T3
    SKIP.
END.
OUTPUT CLOSE.
{EUROPEANAMERICAN.I}
