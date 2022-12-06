
/*------------------------------------------------------------------------
    File        : HKEYSTARTLOAD.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Jan 19 08:19:11 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/


IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO: 
   {HKEYADMPER.I}
   
END.
ELSE DO: 
   LOAD "SOFTWARE\PSC\" BASE-KEY "HKEY_LOCAL_MACHINE" NO-ERROR.
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      LOAD "SOFTWARE\PSC\" BASE-KEY "HKEY_CURRENT_USER" NO-ERROR.
      hkeyvar = "HKEY_CURRENT_USER".
   END.   
   ELSE hkeyvar = "HKEY_LOCAL_MACHINE".
END.   
