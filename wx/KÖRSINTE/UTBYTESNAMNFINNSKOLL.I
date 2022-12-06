
/*------------------------------------------------------------------------
    File        : UTBYTESNAMNFINNSKOLL.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Oct 01 11:36:31 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/
/*
FIND FIRST UTBYTESLISTA WHERE UTBYTESLISTA.UI =  0 NO-LOCK NO-ERROR.
IF AVAILABLE UTBYTESLISTA THEN DO:
*/
 IF Guru.Konstanter:varforetypval[59] = 1 THEN DO:
