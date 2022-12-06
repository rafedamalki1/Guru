
/*------------------------------------------------------------------------
    File        : NYASERVERPROG.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Dec 20 09:33:58 CET 2013
    Notes       :
       
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER globforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE oeversion AS CHARACTER NO-UNDO.
DEFINE VARIABLE dlcvarin AS CHARACTER NO-UNDO.
DEFINE VARIABLE guruvarin AS CHARACTER NO-UNDO.
DEFINE VARIABLE companyname AS CHARACTER NO-UNDO.
DEFINE VARIABLE appnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE hkeyvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "x(69)"  NO-UNDO.
DEFINE VARIABLE kommandofran AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandotill AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcstart AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE provag
   FIELD VAG AS CHARACTER.
REPEAT i = 1 TO NUM-ENTRIES(PROPATH):
    CREATE provag.
    provag.VAG = TRIM(STRING(ENTRY(i,PROPATH),"x(78)")).
    provag.VAG = REPLACE(provag.VAG,"PROPATH=","").
END.

  
   LOAD "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION  BASE-KEY "HKEY_LOCAL_MACHINE".
   USE "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION.
   GET-KEY-VALUE SECTION "STARTUP" KEY "DLC" VALUE dlcvarin.
   UNLOAD "SOFTWARE\PSC\WEBCLIENT\" + PROVERSION.
   {APPLICATIONLOAD.I}
   USE companyname.
   GET-KEY-VALUE SECTION appnamn KEY "ApplicationDirectory" VALUE guruvarin.
   
   UNLOAD companyname.
   kommandofran = guruvarin + "\WTID".
    
   ASSIGN 
   companyname = "SOFTWARE\PSC\PROGRESS\"
   hkeyvar = "HKEY_LOCAL_MACHINE"
   appnamn = "11.2".
   LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      /*
      appnamn = "10.2B".
      LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         appnamn = "10.2A".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      END.
      */
   END.  
   IF ERROR-STATUS:NUM-MESSAGES = 0 THEN UNLOAD companyname + appnamn + "\".  
   LOAD companyname BASE-KEY hkeyvar NO-ERROR.
   USE companyname. 
   GET-KEY-VALUE SECTION appnamn KEY "installPath" VALUE guruvarin.
   UNLOAD companyname.
   kommandotill = REPLACE(guruvarin,"DLC","GURU\WTID").
   OS-CREATE-DIR VALUE(kommandotill) NO-ERROR.
   
   kommando = 'robocopy "' + kommandofran + '" "' + kommandotill + '" *.* /mir'.
  
   OS-COMMAND VALUE(kommando) NO-ERROR.
   
   


 




