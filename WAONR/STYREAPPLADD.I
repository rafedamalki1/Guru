
/*------------------------------------------------------------------------
    File        : STYREAPPLADD.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Nov 23 11:54:49 CET 2022
    Notes       :
  ----------------------------------------------------------------------*/

Guru.Konstanter:varforetypval = varforetypval.
Guru.Konstanter:varforetypchar = varforetypchar.  
IF Guru.Konstanter:varforetypval[55] = 1 THEN  Guru.GlobalaVariabler:Kryptonit = TRUE.
ELSE Guru.GlobalaVariabler:Kryptonit = FALSE.
