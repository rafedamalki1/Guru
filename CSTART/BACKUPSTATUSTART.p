
/*------------------------------------------------------------------------
    File        : BACKUPSTATUSTART.p
    Purpose     : 

    Syntax      :för att beräkna antal 
                 ta bort dubbletter  
                 ta bort one licenser
                 ta bort de som kör på egna licenser ex rejlers tectel elkraft beredning och sweco köpta
                 uppsaggda

    Description : 

    Author(s)   : 
    Created     : Tue Jun 14 12:26:41 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/
{VALDBTEMP.I} 
{VALDBALL.I}
DEFINE INPUT  PARAMETER FILL-IN-APPSERVER AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER vad AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER conappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE bckstatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.

DEFINE VARIABLE gforetag AS CHARACTER NO-UNDO.

  

DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE Computer_LanIP AS CHARACTER NO-UNDO.
/* ***************************  Definitions  ************************** */
RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).
IF TRIM(outanvanv) NE CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN QUIT.     
FIND FIRST valdbtemp  WHERE valdbtemp.GFORETAG = vad NO-LOCK NO-ERROR.  
Computer_LanIP = SUBSTRING(valdbtemp.DBCON,INDEX(valdbtemp.DBCON,"-H"),16).


FOR EACH valdbtemp:
   IF {TAEJMEDDB.I} THEN DELETE valdbtemp.
   ELSE IF valdbtemp.APPCON NE conappvar THEN DO: 
      IF valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" THEN. 
      ELSE DELETE valdbtemp.
   END. 
END.
   
FOR EACH valdbtemp WHERE NO-LOCK:
   IF FILL-IN-APPSERVER = TRUE THEN DO:
      ASSIGN
      gforetag = valdbtemp.GFORETAG
      conappvar = valdbtemp.APPCON.
     
      CREATE SERVER Guru.Konstanter:apphand.
      IF conappvar = "" THEN DO:
         MESSAGE "Kontakta Elpool tel 090/184540 för du kan inte ansluta korrekt!"
         VIEW-AS ALERT-BOX.
      END. 
      /*obs case-sensitv -AppService appguru9*/
      ELSE DO:
         Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR.  
      END.
      IF NOT Guru.Konstanter:appcon THEN DO:
          MESSAGE 
         ERROR-STATUS:NUM-MESSAGES 
         " fel uppkom vid anslutningen." SKIP 
         "Det går ej att ansluta appserver och databasen i Guru." SKIP
         "Kontakta system ansvarig." SKIP
         "Kontakta Elpool tel 090/184540." SKIP
         SKIP
         "Vill du se felmeddelandena ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel på anslutningen"
         UPDATE view-errs AS LOGICAL .       
         IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
            MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
            ERROR-STATUS:GET-MESSAGE(ivar)
            VIEW-AS ALERT-BOX.
         END.     
         RETURN NO-APPLY.
      END.
      ELSE DO:
         RUN BACKUPSTATUS.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT (OUTPUT dbnamn, OUTPUT bckstatus).
      END.   
   END.
   ELSE DO:
      RUN appcon9.P (INPUT {APPCON1.i},INPUT {APPCON2.i},INPUT valdbtemp.GFORETAG).
      RUN BACKUPSTATUS.p (OUTPUT dbnamn, OUTPUT bckstatus).
      RUN DBDISCONN.p (INPUT valdbtemp.DBNAMN).
      
   END.       
   MESSAGE  dbnamn bckstatus
   VIEW-AS ALERT-BOX. 
   
   IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
   IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.

END.

MESSAGE conappvar " är klar!"
VIEW-AS ALERT-BOX.
   

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
