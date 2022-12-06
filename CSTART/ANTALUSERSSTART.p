
/*------------------------------------------------------------------------
    File        : ANTALUSERSSTART.p
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
DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
DEFINE VARIABLE utbivar AS CHARACTER NO-UNDO.
DEFINE VARIABLE gforetag AS CHARACTER NO-UNDO.
.
DEFINE INPUT  PARAMETER vad AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER conappvar AS CHARACTER NO-UNDO.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE anvtth AS HANDLE NO-UNDO.
DEFINE VARIABLE anvBuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE anvSummtth AS HANDLE NO-UNDO.
DEFINE VARIABLE anvSummBuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE userbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE usertth AS HANDLE NO-UNDO.
DEFINE VARIABLE AnvDS AS HANDLE NO-UNDO.
DEFINE VARIABLE Computer_LanIP AS CHARACTER NO-UNDO.
DEFINE VARIABLE breakbyqueryvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE antalbreakby AS INTEGER NO-UNDO.
DEFINE VARIABLE antalvar AS INTEGER NO-UNDO.
DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).
IF TRIM(outanvanv) NE CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN QUIT.       

CREATE TEMP-TABLE anvtth. 

IF vad = "www2" THEN DO:
   Computer_LanIP = {www2db.I}.
   FOR EACH valdbtemp:
      IF {TAEJMEDDB.I} THEN DELETE valdbtemp.
      IF AVAILABLE valdbtemp THEN DO:
         IF valdbtemp.DBcon MATCHES "*" + Computer_LanIP + "*"  THEN.
         ELSE DELETE valdbtemp. 
      END.   
   END.
END.
IF vad = "EGNA" THEN DO:
   Computer_LanIP = {egnadb.I}.
   FOR EACH valdbtemp:
      IF {TAEJMEDDB.I} THEN DELETE valdbtemp.
      IF AVAILABLE valdbtemp THEN DO:
         IF valdbtemp.DBcon MATCHES "*" + Computer_LanIP + "*"  THEN. 
         ELSE DELETE valdbtemp. 
      END.   
   END.
END.

   
FOR EACH valdbtemp WHERE NO-LOCK:
   IF valdbtemp.FORETAG = "ELPA" THEN NEXT.
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
      DELETE OBJECT AnvDS NO-ERROR.
      AnvDS = ?.
      
      DELETE OBJECT userbuffh NO-ERROR.
      userbuffh = ?.
      /*
      MESSAGE valdbtemp.FORETAG valdbtemp.gFORETAG
      VIEW-AS ALERT-BOX.
      */
      RUN ANTALUSERS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (OUTPUT DATASET-HANDLE AnvDS).
     
   END.   
   IF userbuffh = ? THEN DO:
      userbuffh = AnvDS:GET-BUFFER-HANDLE(1). 
   END.  
   IF anvBuffh = ? THEN DO: 
      anvtth:CREATE-LIKE(userbuffh).
      anvtth:ADD-NEW-FIELD("ANTAL","INTEGER").
      anvtth:TEMP-TABLE-PREPARE("usrAnvtt").
      anvBuffh = anvtth:DEFAULT-BUFFER-HANDLE.
      CREATE TEMP-TABLE anvSummtth.
      anvSummtth:CREATE-LIKE(userbuffh).
      anvSummtth:ADD-NEW-FIELD("ANTAL","INTEGER").
      anvSummtth:TEMP-TABLE-PREPARE("sumAnvtt").
      anvSummBuffh = anvSummtth:DEFAULT-BUFFER-HANDLE.
   END.
   
   kommandoquery = "FOR EACH " + userbuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT userbuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      IF userbuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN.
      ELSE DO:
         anvBuffh:BUFFER-CREATE. 
         anvBuffh:BUFFER-COPY(userbuffh).
         anvBuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 1.
      END.
      qH:GET-NEXT().
   END.
   RUN CloseCustomQuery (input qH).
   IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
   IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.

END.
breakbyqueryvar = " BREAK BY DATADB BY FORETAG ".
antalbreakby = 2. 
kommandoquery = "FOR EACH " + anvBuffh:TABLE + " NO-LOCK" + breakbyqueryvar.
RUN CreateCustomQuery(INPUT anvBuffh,INPUT kommandoquery,OUTPUT qh).
qH:GET-FIRST().  
REPEAT:
   IF  anvBuffh:AVAILABLE THEN.
   ELSE LEAVE .
   antalvar = antalvar + anvBuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.  
   IF qH:LAST-OF(antalbreakby) = TRUE THEN DO:
      
      anvSummBuffh:BUFFER-CREATE. 
      anvSummBuffh:BUFFER-FIELD("FORETAG"):BUFFER-VALUE = anvBuffh:BUFFER-FIELD("FORETAG"):BUFFER-VALUE.
      anvSummBuffh:BUFFER-FIELD("DATADB"):BUFFER-VALUE = anvBuffh:BUFFER-FIELD("DATADB"):BUFFER-VALUE.
      anvSummBuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = antalvar.
      anvSummBuffh:BUFFER-FIELD("FAKTURAREF"):BUFFER-VALUE = "Totalt antal".
      antalvar = 0.
   END.
   qH:GET-NEXT().
END.
kommandoquery = "FOR EACH " + anvSummBuffh:TABLE + " NO-LOCK". 
RUN CreateCustomQuery(INPUT anvSummBuffh,INPUT kommandoquery,OUTPUT qh).
qH:GET-FIRST().
DO WHILE qH:QUERY-OFF-END = FALSE:
   anvBuffh:BUFFER-CREATE. 
   anvBuffh:BUFFER-COPY(anvSummBuffh).
   qH:GET-NEXT().
END. 
RUN visa_UI.
MESSAGE vad " är klar!"
VIEW-AS ALERT-BOX.
PROCEDURE visa_UI :
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE temph AS HANDLE NO-UNDO.
   DEFINE VARIABLE VisaExcel AS Controls.GuruExcelInterop NO-UNDO.
   DEFINE VARIABLE anvbuffhE AS HANDLE NO-UNDO.
   
   CREATE BUFFER anvbuffhE FOR TABLE anvbuffh.
   VisaExcel = NEW Controls.GuruExcelInterop().
   VisaExcel:direkticell = TRUE. 
   VisaExcel:InteropInitialize(""). 
   VisaExcel:SetVisible(TRUE).
   VisaExcel:irad = 1.   
   VisaExcel:cColname = "A".
   VisaExcel:ColumnRad().
   VisaExcel:DataOut("Ta bort köpta, dubbleter av users, ONE databaser, ej hyrda konsulter").
   VisaExcel:Rowdown().
   tempcounter = 1.
   DO WHILE tempcounter LE anvBuffh:NUM-FIELDS:
      temph = anvBuffh:BUFFER-FIELD(tempcounter).
      VisaExcel:DataOut(temph:NAME).
      VisaExcel:ColWidth(VisaExcel:getcolname(tempcounter) + STRING(VisaExcel:iRad),LENGTH(temph:NAME) + 5).
      VisaExcel:ColRight().
      tempcounter = tempcounter + 1.
   END.
   /*
   VisaExcel:DataOut("Antal").
  */
   
   kommandoquery = "FOR EACH " + anvBuffh:TABLE + "  NO-LOCK by DATADB by FAKTURAREF by anvandare". 
   RUN CreateCustomQuery(INPUT anvBuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      VisaExcel:cColname = "A".
      VisaExcel:Rowdown().
      VisaExcel:ColumnRad().
      tempcounter = 1.
      anvbuffhE:FIND-FIRST("WHERE ANVANDARE ='" + STRING(anvBuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE) + "' AND DATADB NE '" + STRING(anvBuffh:BUFFER-FIELD("DATADB"):BUFFER-VALUE) + "'",NO-LOCK) NO-ERROR.
      IF anvbuffhE:AVAILABLE THEN DO:
         VisaExcel:FontExcel("A" + STRING(VisaExcel:iRad) ,"A" + STRING(VisaExcel:iRad),"",0,?,0,5 ).
      END.
      IF anvBuffh:BUFFER-FIELD("FAKTURAREF"):BUFFER-VALUE = "Totalt antal" THEN DO:
        
         VisaExcel:FontExcel("M" + STRING(VisaExcel:iRad) ,"R" + STRING(VisaExcel:iRad),"",0,?,0,3 ).
         VisaExcel:DataOut("R" + STRING(VisaExcel:iRad),STRING(anvBuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE)).
         anvBuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 0.
      END.         
      DO WHILE tempcounter LE anvBuffh:NUM-FIELDS:
         temph = anvBuffh:BUFFER-FIELD(tempcounter).
         VisaExcel:DataOut(STRING(anvBuffh:BUFFER-FIELD(temph:NAME):BUFFER-VALUE)).
         VisaExcel:ColRight().
         tempcounter = tempcounter + 1.
      END.
      /*
      VisaExcel:DataOut("1").
      */
      qH:GET-NEXT().
   END.
   VisaExcel:ReleaseExcel(false).
  
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
