
/*------------------------------------------------------------------------
    File        : CHECK3264.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Aug 17 09:54:52 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER pa64 AS LOGICAL NO-UNDO.
IF PROCESS-ARCHITECTURE = 32 THEN pa64 = FALSE.
ELSE pa64 = TRUE. 