
/*------------------------------------------------------------------------
    File        : mutistart.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Jun 14 12:26:41 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/
{VALDBTEMP.I} 

{VALDBALL.I}

DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
DEFINE VARIABLE utbivar AS CHARACTER NO-UNDO.
DEFINE VARIABLE gforetag AS CHARACTER NO-UNDO.

  

DEFINE INPUT  PARAMETER vad AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER conappvar AS CHARACTER NO-UNDO.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE anvtth AS HANDLE NO-UNDO.
DEFINE VARIABLE anvBuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE userbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE usertth AS HANDLE NO-UNDO.
DEFINE VARIABLE AnvDS AS HANDLE NO-UNDO.
DEFINE VARIABLE AppServerHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE BerKalkDS AS HANDLE NO-UNDO.
/*
RUN C:\DELAD\pro9\guru\wx\MULTIDS.P (OUTPUT DATASET-HANDLE BerKalkDS BIND).  

IF VALID-HANDLE(BerKalkDS) THEN DO:
         anvBuffh = BerKalkDS:GET-BUFFER-HANDLE(1).
END.
RUN visabk_UI.
DELETE OBJECT BerKalkDS NO-ERROR.
   BerKalkDS = ?. 
RUN C:\DELAD\pro9\guru\wx\MULTIDS.P (OUTPUT DATASET-HANDLE BerKalkDS BIND).


IF VALID-HANDLE(BerKalkDS) THEN DO:
         anvBuffh = BerKalkDS:GET-BUFFER-HANDLE(1).
END.
RUN visabk_UI.
*/

CREATE TEMP-TABLE anvtth. 
IF vad = "www2" THEN DO:
   FOR EACH valdbtemp:
      IF valdbtemp.DBNAMN = "UTBI" THEN DELETE valdbtemp.
      IF AVAILABLE valdbtemp THEN DO:
         IF valdbtemp.APPCON NE conappvar THEN DELETE valdbtemp. 
      END.   
   END.
END.

DEFINE VARIABLE brwproc AS HANDLE NO-UNDO.
   
FOR EACH valdbtemp WHERE  NO-LOCK:
   IF VALID-HANDLE(brwproc) THEN DELETE PROCEDURE brwproc NO-ERROR.
   ASSIGN
   gforetag = valdbtemp.GFORETAG
   conappvar = valdbtemp.APPCON.
   /*valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"  */
   CREATE SERVER Guru.Konstanter:apphand.
   
   IF conappvar = "" THEN DO:
      MESSAGE "Kontakta Elpool tel 090/184540 för du kan inte ansluta korrekt!"
      VIEW-AS ALERT-BOX.
   END. 
   /*obs case-sensitv -AppService appguru9*/
   ELSE DO:
      Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT(conappvar,"","",gforetag) NO-ERROR.  
   END.
   IF NOT Guru.Konstanter:appcon THEN DO:
      MESSAGE 
      ERROR-STATUS:NUM-MESSAGES 
      " fel uppkom vid anslutningen." SKIP 
      "Det går ej att ansluta appserver." SKIP 
      "Felet kan bero på för många samtidiga användare." SKIP
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
      DELETE OBJECT AnvDS NO-ERROR.
      AnvDS = ?.
      DELETE OBJECT userbuffh NO-ERROR.
      userbuffh = ?.
      RUN C:\DELAD\pro9\guru\wx\MULTIDS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (OUTPUT DATASET-HANDLE AnvDS BIND).
      
      /*
      RUN ANTALUSERS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (OUTPUT TABLE-HANDLE usertth).
      RUN C:\DELAD\pro9\guru\wx\MULTIDS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (OUTPUT DATASET-HANDLE AnvDS BIND).
      RUN ANTALUSERS.P (OUTPUT DATASET-HANDLE AnvDS APPEND).
     */
   END.   
   IF VALID-HANDLE(AnvDS) THEN DO:
      IF userbuffh = ? THEN DO:
         
         userbuffh = AnvDS:GET-BUFFER-HANDLE(1).
          
      END.  
      /*
      IF anvBuffh = ? THEN DO: 
         anvtth:CREATE-LIKE(userbuffh).
         anvtth:TEMP-TABLE-PREPARE("usrAnvtt").
         anvBuffh = anvtth:DEFAULT-BUFFER-HANDLE.
      END.
      */   
   END.
   RUN UVISA_ui.
   MESSAGE "1"
   VIEW-AS ALERT-BOX.
   userbuffh:EMPTY-TEMP-TABLE (). 
   DELETE OBJECT userbuffh NO-ERROR.
   userbuffh = ?. 
   DELETE OBJECT AnvDS NO-ERROR.
   AnvDS = ?.
   RUN C:\DELAD\pro9\guru\wx\MULTIDS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (OUTPUT DATASET-HANDLE AnvDS BIND).
   userbuffh = AnvDS:GET-BUFFER-HANDLE(1).
   RUN UVISA_ui.
   MESSAGE "2"
   VIEW-AS ALERT-BOX.
   IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
   IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.

END.
/*
RUN visa_UI.
*/
PROCEDURE UVISA_ui :
   kommandoquery = "FOR EACH " + userbuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT userbuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      
      DISPLAY  userbuffh:BUFFER-FIELD("FORETAG"):BUFFER-VALUE 
      userbuffh:BUFFER-FIELD("DATADB"):BUFFER-VALUE userbuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE WITH FRAME CC DOWN. 
      DOWN 1 WITH FRAME CC.
      
      
      /*
      anvBuffh:BUFFER-CREATE. 
      anvBuffh:BUFFER-COPY(userbuffh).
      */
      qH:GET-NEXT().
   END.
   RUN CloseCustomQuery (input qH).
   AnvDS:EMPTY-DATASET ().
END PROCEDURE.
PROCEDURE visa_UI :
   kommandoquery = "FOR EACH " + anvBuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT anvBuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      kommandoquery = anvBuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE.
      DISPLAY  anvBuffh:BUFFER-FIELD("FORETAG"):BUFFER-VALUE 
      anvBuffh:BUFFER-FIELD("DATADB"):BUFFER-VALUE anvBuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE WITH FRAME CC DOWN. 
      DOWN 1 WITH FRAME CC.
      qH:GET-NEXT().
   END.
  
END PROCEDURE.
PROCEDURE visabk_ui :
   kommandoquery = "FOR EACH " + anvBuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT anvBuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      kommandoquery = anvBuffh:BUFFER-FIELD("bernr"):BUFFER-VALUE.
      
      qH:GET-NEXT().
   END.
  
END PROCEDURE.
PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryH.
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.
   
PROCEDURE CloseCustomQuery:
   DEFINE INPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CustomQueryh:QUERY-CLOSE()  NO-ERROR.
   CustomQueryh = ?.
END PROCEDURE.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
