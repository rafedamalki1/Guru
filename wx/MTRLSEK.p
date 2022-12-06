
/*------------------------------------------------------------------------
    File        : MTRLSEK.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Oct 27 11:10:30 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST XSEK WHERE XSEK.AV-LEVEL = 0 AND XSEK.MENYVART = "MTRL" EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE XSEK THEN DO:
   XSEK.SEK[6] = FALSE.
END.   