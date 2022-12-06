
/*------------------------------------------------------------------------
    File        : HKEYADMPER11.p
    Purpose     : 

    Syntax      :går ej att kompilera i version 10

    Description : 

    Author(s)   : 
    Created     : Thu Jan 19 20:21:04 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER hkeyvar AS CHARACTER NO-UNDO.
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
   IF SESSION:WC-ADMIN-APP = TRUE THEN hkeyvar = "HKEY_LOCAL_MACHINE".
   ELSE hkeyvar = "HKEY_CURRENT_USER".
END.
ELSE hkeyvar = "HKEY_LOCAL_MACHINE".