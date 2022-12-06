
/*------------------------------------------------------------------------
    File        : inktt.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Feb 10 12:57:49 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/
   DEFINE VAR namfil AS CHARACTER NO-UNDO.
   DEFINE VAR infillong AS LONGCHAR NO-UNDO.

COPY-LOB FROM FILE "D:\delad\ftpfel.txt" TO infillong.
namfil = "TESTAR.FILER".
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn3 AS CHARACTER NO-UNDO.
   prognamn = SESSION:TEMP-DIRECTORY.    
   prognamn = prognamn + "gurubest" + "\ahlsell\". 
   OS-CREATE-DIR VALUE(prognamn) NO-ERROR.
   prognamn3 = SESSION:TEMP-DIRECTORY.
   prognamn3 = prognamn3 + "gurubest" + "\" + "bestkopia\". 
   prognamn3 = prognamn3 + STRING(namfil) + STRING(TODAY,"99999999") + STRING(TIME) + ".guru". 
   prognamn = prognamn + STRING(namfil) + ".guru".     
   prognamn2 = STRING(namfil) + ".guru".                                    
   COPY-LOB FROM infillong TO FILE prognamn.
   OS-COPY VALUE(prognamn) VALUE(prognamn3).
  
      