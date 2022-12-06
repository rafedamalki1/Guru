
/*------------------------------------------------------------------------
    File        : HKEYADMPER10.p
    Purpose     : 

    Syntax      :

    Description : GER WEBCLIENTENS INSTALLATION

    Author(s)   : 
    Created     : Thu Jan 19 20:21:04 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER hkeyvar AS CHARACTER NO-UNDO.
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
   LOAD "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION  BASE-KEY "HKEY_LOCAL_MACHINE" NO-ERROR.
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      LOAD "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION  BASE-KEY "HKEY_CURRENT_USER" NO-ERROR. 
      hkeyvar = "HKEY_CURRENT_USER".
   END.
   ELSE hkeyvar = "HKEY_LOCAL_MACHINE".
END.
ELSE hkeyvar = "HKEY_ LOCAL_MACHINE". 