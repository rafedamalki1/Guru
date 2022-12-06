/* BERKALKBKID.I*/

       
PROCEDURE BKID_UI :
   DEFINE INPUT  PARAMETER fkalknr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER fomrade AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER tkalknr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER tomrade AS CHARACTER NO-UNDO.
   DEFINE VARIABLE berkopptabbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE berkalktabbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE berkalktabbuff2h AS HANDLE NO-UNDO.
   DEFINE VARIABLE kalkquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kalkqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE LocalAppServerHandle AS HANDLE NO-UNDO.
   DEFINE VARIABLE kalkiberfinns AS LOGICAL NO-UNDO.
   DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nyKalkNrvar AS INTEGER NO-UNDO.
   RUN KalkyleraiBeredningStart.p PERSISTENT SET LocalAppServerHandle (INPUT Guru.Konstanter:globanv).
   CREATE BUFFER berkopptabbuffh FOR TABLE "BERKALKOPPLA" IN WIDGET-POOL "DynTableBH".
   berkopptabbuffh:FIND-FIRST("WHERE BERNR = " + fkalknr + " AND OMRADE = '" + fomrade + "'", NO-LOCK) NO-ERROR.
   IF berkopptabbuffh:AVAILABLE THEN DO:
       /*koppling och kalkyl skapas*/
      RUN KalkyleraiBeredning_UI IN LocalAppServerHandle (INPUT 0,INPUT INTEGER(tkalknr),INPUT tomrade,OUTPUT felmedd,OUTPUT nyKalkNrvar, OUTPUT kalkiberfinns).
      IF felmedd = "" THEN kalkiberfinns = TRUE.
   END.
   ELSE kalkiberfinns = FALSE.
IF kalkiberfinns = TRUE THEN DO:
      CREATE BUFFER berkalktabbuffh FOR TABLE "KALKNUM" IN WIDGET-POOL "DynTableBH".
      CREATE BUFFER berkalktabbuff2h FOR TABLE "KALKNUM" IN WIDGET-POOL "DynTableBH".
      kalkquery = "FOR EACH " + berkalktabbuffh:TABLE + " WHERE KALKNR = " + berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE + 
      " AND OMRADE = '" + berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE + "'" + " NO-LOCK".
   END.     
   ELSE DO:  
      CREATE BUFFER berkalktabbuffh FOR TABLE "BERKALK" IN WIDGET-POOL "DynTableBH".
      CREATE BUFFER berkalktabbuff2h FOR TABLE "BERKALK" IN WIDGET-POOL "DynTableBH".
      kalkquery = "FOR EACH " + berkalktabbuffh:TABLE + " WHERE AONR = '" + fkalknr + "' AND OMRADE = '" + fomrade + "'" + " NO-LOCK".
   END.
   
   RUN CreateCustomQuery(INPUT berkalktabbuffh,INPUT kalkquery,OUTPUT kalkqh).
   kalkqh:GET-FIRST(NO-LOCK).
   DO WHILE kalkqh:QUERY-OFF-END = FALSE:
      DO TRANSACTION:
         berkalktabbuff2h:BUFFER-CREATE().
         IF kalkiberfinns = TRUE THEN DO:
            berkalktabbuff2h:BUFFER-COPY(berkalktabbuffh).
            berkalktabbuff2h:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = nyKalkNrvar.
            berkalktabbuff2h:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = tomrade.
         END.
         ELSE DO:
            berkalktabbuff2h:BUFFER-COPY(berkalktabbuffh).
            berkalktabbuff2h:BUFFER-FIELD("AONR"):BUFFER-VALUE = tkalknr.
            berkalktabbuff2h:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = tomrade.
         END.      
      END.   
      kalkqh:GET-NEXT(NO-LOCK).
   END.
   
   IF kalkiberfinns = TRUE THEN DO:
      CREATE BUFFER berkalktabbuffh FOR TABLE "KALKNUMSUB" IN WIDGET-POOL "DynTableBH".
      CREATE BUFFER berkalktabbuff2h FOR TABLE "KALKNUMSUB" IN WIDGET-POOL "DynTableBH".
      /*
      kalkquery = "FOR EACH " + berkalktabbuffh:TABLE + " WHERE BERNR = " + fkalknr + " AND OMRADE = '" + fomrade + "'" + " NO-LOCK".
      */
      kalkquery = "FOR EACH " + berkalktabbuffh:TABLE + " WHERE KALKNR = " + berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE + 
      " AND OMRADE = '" + berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE  + "' NO-LOCK".
      RUN CreateCustomQuery(INPUT berkalktabbuffh,INPUT kalkquery,OUTPUT kalkqh).
      kalkqh:GET-FIRST(NO-LOCK).
      DO WHILE kalkqh:QUERY-OFF-END = FALSE:
         DO TRANSACTION:
            berkalktabbuff2h:BUFFER-CREATE().
            berkalktabbuff2h:BUFFER-COPY(berkalktabbuffh).
            berkalktabbuff2h:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = nyKalkNrvar.
            berkalktabbuff2h:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = tomrade.
         END.   
         kalkqh:GET-NEXT(NO-LOCK).
      END.
   END.  
   berkalktabbuffh:BUFFER-RELEASE () NO-ERROR.
   berkalktabbuff2h:BUFFER-RELEASE () NO-ERROR.
  
   IF VALID-HANDLE(LocalAppServerHandle) THEN DO:
      RUN avsluta_UI IN LocalAppServerHandle.
      DELETE PROCEDURE LocalAppServerHandle.
      LocalAppServerHandle = ?.
   END.   
   DELETE OBJECT berkopptabbuffh NO-ERROR.
   berkopptabbuffh = ?.
   DELETE OBJECT berkalktabbuffh NO-ERROR.
   berkalktabbuffh = ?.
   DELETE OBJECT berkalktabbuff2h NO-ERROR.
   berkalktabbuff2h = ?.
   DELETE OBJECT kalkqh NO-ERROR.
   kalkqh = ?.
END PROCEDURE.
PROCEDURE HKID_UI :
   DEFINE INPUT  PARAMETER fkalknr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER fomrade AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER tkalknr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tomrade AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
   DEFINE VARIABLE hdkalktabkbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE hdkalktabkbuff2h AS HANDLE NO-UNDO.
   DEFINE VARIABLE kalkquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kalkqh AS HANDLE NO-UNDO.
   /*
  
      
   END.
   ELSE DO:  
      RUN FINNSTABELL.P (INPUT "BERKALKOPPLA", OUTPUT logresult).
      CREATE BUFFER hdkalktabkbuffh FOR TABLE "HDKALK" IN WIDGET-POOL "DynTableBH".
      CREATE BUFFER hdkalktabkbuff2h FOR TABLE "HDKALK" IN WIDGET-POOL "DynTableBH".
      kalkquery = "FOR EACH " + hdkalktabkbuffh:TABLE + " WHERE BERNR = " + STRING(fkalknr) + " AND OMRADE = '" + fomrade + "'" + " NO-LOCK".
      RUN CreateCustomQuery(INPUT hdkalktabkbuffh,INPUT kalkquery,OUTPUT kalkqh).
      kalkqh:GET-FIRST(NO-LOCK).
      DO WHILE kalkqh:QUERY-OFF-END = FALSE:
         DO TRANSACTION:
            hdkalktabkbuff2h:BUFFER-CREATE().
            IF logresult = TRUE THEN DO:
               hdkalktabkbuff2h:BUFFER-COPY(hdkalktabkbuffh,"HKID,KALKNUM").
            END.
            ELSE DO:
               hdkalktabkbuff2h:BUFFER-COPY(hdkalktabkbuffh).
            END.   
            hdkalktabkbuff2h:BUFFER-FIELD("BERNR"):BUFFER-VALUE = tkalknr.
            hdkalktabkbuff2h:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = tomrade.   
         END.   
         kalkqh:GET-NEXT(NO-LOCK).
      END.
      hdkalktabkbuffh:BUFFER-RELEASE () NO-ERROR.
      hdkalktabkbuff2h:BUFFER-RELEASE () NO-ERROR.
      
      DELETE OBJECT hdkalktabkbuffh  NO-ERROR.
      hdkalktabkbuffh = ?.
      DELETE OBJECT hdkalktabkbuff2h NO-ERROR.
      hdkalktabkbuff2h = ?.
   END. 
   DELETE OBJECT kalkqh  NO-ERROR.
   kalkqh = ?.
   */   
END PROCEDURE.  

PROCEDURE BerkoppbuffCreate_UI :
   DEFINE INPUT PARAMETER bernrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   
   IF NOT VALID-HANDLE(berkopptabbuffh) THEN CREATE BUFFER berkopptabbuffh FOR TABLE "BERKALKOPPLA" IN WIDGET-POOL "DynTableBH".
   berkopptabbuffh:FIND-FIRST("WHERE BERNR = " + STRING(bernrvar) + " AND OMRADE = '" + omrvar + "'", NO-LOCK) NO-ERROR.
   IF berkopptabbuffh:AVAILABLE THEN DO:
      FIND FIRST KALKHUV WHERE  KALKHUV.KALKNR = berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE AND  KALKHUV.OMRADE = berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE   NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN grundkloid = KALKHUV.KLOGID.
   END.
   ELSE RUN BerkoppbufAvs_UI.
END PROCEDURE.

PROCEDURE BerkoppbuffDelete_UI :
   DEFINE INPUT PARAMETER bernrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   IF NOT VALID-HANDLE(berkopptabbuffh) THEN CREATE BUFFER berkopptabbuffh FOR TABLE "BERKALKOPPLA" IN WIDGET-POOL "DynTableBH".
   DO TRANSACTION:
      berkopptabbuffh:FIND-FIRST("WHERE BERNR = " + STRING(bernrvar) + " AND OMRADE = '" + omrvar + "'", EXCLUSIVE-LOCK) NO-ERROR.
      IF berkopptabbuffh:AVAILABLE THEN berkopptabbuffh:BUFFER-DELETE.
   END.   
   RUN BerkoppbufAvs_UI.
END PROCEDURE.

PROCEDURE BerkoppbufAvs_UI :
   DELETE OBJECT berkopptabbuffh NO-ERROR.
   berkopptabbuffh = ?.
  
END PROCEDURE.

PROCEDURE BerKalkBortStart_UI :
   DEFINE INPUT PARAMETER bernrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   
   RUN BerkoppbuffCreate_UI (INPUT bernrvar,INPUT omrvar). 
   RUN BerKalkBort_UI (INPUT bernrvar,INPUT omrvar).
   RUN BerkoppbufAvs_UI.
END PROCEDURE.
PROCEDURE BerKalkBort_UI:
   DEFINE INPUT PARAMETER bernrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   IF VALID-HANDLE(berkopptabbuffh) THEN DO:
      IF berkopptabbuffh:AVAILABLE THEN DO:
         FOR EACH KALKNUM WHERE KALKNUM.KALKNR = berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE AND KALKNUM.OMRADE = berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE  
         EXCLUSIVE-LOCK:
            FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKNUM.KALKNR AND KALKNUMSUB.OMRADE = KALKNUM.OMRADE AND KALKNUMSUB.NUM = KALKNUM.NUM EXCLUSIVE-LOCK:
               DELETE KALKNUMSUB.
            END.
            DELETE KALKNUM.
         END.
         FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE AND KALKHUV.OMRADE = berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE KALKHUV THEN  DELETE KALKHUV.
      END.
   END.   

END PROCEDURE.

PROCEDURE HDKalkBortStart_UI :
   DEFINE INPUT PARAMETER bernrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER sidnr AS INTEGER NO-UNDO.
   RUN BerkoppbuffCreate_UI (INPUT bernrvar,INPUT omrvar). 
   RUN HDKalkBort_UI (INPUT bernrvar,INPUT omrvar,INPUT sidnr).
   RUN BerkoppbufAvs_UI.
END PROCEDURE.


PROCEDURE HDKalkBort_UI:
   DEFINE INPUT PARAMETER bernrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER sidval AS INTEGER NO-UNDO.
   DEFINE VARIABLE berkalktabbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE kalkquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kalkqh AS HANDLE NO-UNDO.
   IF sidval = 0 THEN DO:
      RETURN.
      
   END.
   ELSE DO:
      IF VALID-HANDLE(berkopptabbuffh) THEN DO:
         IF berkopptabbuffh:AVAILABLE THEN DO:
            CREATE BUFFER berkalktabbuffh FOR TABLE "KALKNUM" IN WIDGET-POOL "DynTableBH".
            kalkquery = "FOR EACH " + berkalktabbuffh:TABLE + " WHERE KALKNR = " + berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE + 
            " AND OMRADE = '" + berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE + "'" + " AND SID = " + STRING(sidval) +  
            " NO-LOCK".
            RUN CreateCustomQuery(INPUT berkalktabbuffh,INPUT kalkquery,OUTPUT kalkqh).
            DO TRANSACTION: 
               kalkqh:GET-FIRST(EXCLUSIVE-LOCK).
               DO WHILE kalkqh:QUERY-OFF-END = FALSE:
                  FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE AND 
                  KALKNUMSUB.OMRADE = berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE AND 
                  KALKNUMSUB.NUM = berkalktabbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE EXCLUSIVE-LOCK:
                     DELETE KALKNUMSUB.
                  END.
                  berkalktabbuffh:BUFFER-DELETE.
                  kalkqh:GET-NEXT(EXCLUSIVE-LOCK).
               END.
            END.     
         END.
      END.   
      RETURN.
   END.
   
END PROCEDURE.

PROCEDURE HdKalkUppStart_UI :
   DEFINE INPUT PARAMETER bernrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER sidnr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER hdkalkfinns AS LOGICAL NO-UNDO.
   RUN BerkoppbuffCreate_UI (INPUT bernrvar,INPUT omrvar). 
   RUN HdKalkUpp_UI (INPUT bernrvar,INPUT omrvar,INPUT sidnr, OUTPUT hdkalkfinns).
   RUN BerkoppbufAvs_UI.
END PROCEDURE.
PROCEDURE HdKalkUpp_UI :
   DEFINE INPUT PARAMETER bernrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER sidnr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER hdkalkfinns AS LOGICAL NO-UNDO.
   DEFINE VARIABLE berkalktabbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE kalkquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kalkqh AS HANDLE NO-UNDO.
   IF VALID-HANDLE(berkopptabbuffh) THEN DO:
      IF berkopptabbuffh:AVAILABLE THEN DO:
         CREATE BUFFER berkalktabbuffh FOR TABLE "KALKNUM" IN WIDGET-POOL "DynTableBH".
         kalkquery = "FOR EACH " + berkalktabbuffh:TABLE + " WHERE KALKNR = " + berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE + 
         " AND OMRADE = '" + berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE + "'" + " AND SID = " + STRING(sidnr) +  
         " NO-LOCK".
         RUN CreateCustomQuery(INPUT berkalktabbuffh,INPUT kalkquery,OUTPUT kalkqh).
         kalkqh:GET-FIRST(NO-LOCK).
         DO WHILE kalkqh:QUERY-OFF-END = FALSE:
            CREATE hdkalktemp.
            BUFFER hdkalktemp:HANDLE:BUFFER-COPY(berkalktabbuffh).
            hdkalktemp.BERNR = bernrvar. 
            hdkalkfinns = TRUE.
            IF berkalktabbuffh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE = 0 THEN DO:
               KLOID:
               FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID =  grundkloid NO-LOCK: 
                  FIND LAST KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLLOPPOSTER.ARBKOD = hdkalktemp.ARBKOD AND KALKYLLOPPOSTER.LOPNR = hdkalktemp.LOPNR  USE-INDEX LOPNR NO-LOCK NO-ERROR.
                  IF AVAILABLE KALKYLLOPPOSTER THEN DO:
                      ASSIGN
                     hdkalktemp.BENAMNING = KALKYLLOPPOSTER.BENAMNING
                     hdkalktemp.ENHET = KALKYLLOPPOSTER.ENHET.
                     LEAVE KLOID.  
                  END.
               END.
               /*
               FIND LAST KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.ARBKOD = hdkalktemp.ARBKOD AND KALKYLLOPPOSTER.LOPNR = hdkalktemp.LOPNR USE-INDEX LOPNR NO-LOCK NO-ERROR.
               IF AVAILABLE KALKYLLOPPOSTER THEN DO:
                 /* IF KALKYLLOPPOSTER.ENHET = "KM" THEN HDKALK.ANTAL = HDKALK.ANTAL / 1000.*/
                  ASSIGN
                  hdkalktemp.BENAMNING = KALKYLLOPPOSTER.BENAMNING
                  hdkalktemp.ENHET = KALKYLLOPPOSTER.ENHET.
               END. 
               */
            END.  
            kalkqh:GET-NEXT(NO-LOCK).
         END.     
      END.
   END.   
   DELETE OBJECT berkalktabbuffh NO-ERROR.
   berkalktabbuffh = ?.
   DELETE OBJECT kalkqh NO-ERROR.
   kalkqh = ?. 
END PROCEDURE.

 
PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL "DynTableBH".
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.
PROCEDURE DelPool_UI :
   DELETE WIDGET-POOL "DynTableBH" NO-ERROR.
 
END PROCEDURE.