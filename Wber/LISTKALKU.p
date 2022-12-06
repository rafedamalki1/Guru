/*LISTKALKU.P.*/

{KONVALTEMP.I}
{STARTFORAPP.I}
DEFINE VARIABLE berkopptabbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE berkalknumbuffh AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE ber_temp  NO-UNDO
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER    
   FIELD ANTAL AS DECIMAL
   FIELD NUM AS INTEGER
   INDEX KOD ARBKOD LOPNR ASCENDING
   INDEX NUM NUM ARBKOD LOPNR ASCENDING.

DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE INPUT PARAMETER valkonst AS LOGICAL.
DEFINE INPUT PARAMETER TABLE FOR kon_val.
CREATE WIDGET-POOL "DynTableLI" NO-ERROR. 
DEFINE OUTPUT PARAMETER TABLE FOR ber_temp.
RUN BerKalkkod_UI.


PROCEDURE BerKalkkod_UI :
   DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   CREATE BUFFER berkopptabbuffh FOR TABLE "BERKALKOPPLA" IN WIDGET-POOL "DynTableLI".
   CREATE BUFFER berkalknumbuffh FOR TABLE "KALKNUM" IN WIDGET-POOL "DynTableLI".
  
   berkopptabbuffh:FIND-FIRST("WHERE BERNR = " + valaonr + " AND OMRADE = '" + valomrade + "'", NO-LOCK) NO-ERROR.
   IF berkopptabbuffh:AVAILABLE THEN.
   ELSE RETURN. 
  
   IF valkonst = FALSE THEN DO:   
      queryvar = "FOR EACH " + berkalknumbuffh:TABLE  + " WHERE KALKNUM.KALKNR = " + berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE + 
      " AND KALKNUM.OMRADE = '" + berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE + "' NO-LOCK".
      RUN CreateCustomQuery(INPUT berkalknumbuffh,INPUT queryvar,OUTPUT qh).
      qH:GET-FIRST(NO-LOCK).
      DO WHILE qH:QUERY-OFF-END = FALSE:
         CREATE ber_temp.
         BUFFER ber_temp:HANDLE:BUFFER-COPY(berkalknumbuffh,"NUM").
         ber_temp.NUM = berkalknumbuffh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE.
         IF berkalknumbuffh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE = 0 THEN DO:
            FIND FIRST KALKBER WHERE KALKBER.ARBKOD = ber_temp.ARBKOD AND KALKBER.LOPNR = ber_temp.LOPNR NO-LOCK NO-ERROR.
            IF AVAILABLE KALKBER THEN DO:
               ASSIGN
               ber_temp.BENAMNING = KALKBER.BENAMNING
               ber_temp.ENHET = KALKBER.ENHET. 
            END.   
         END.        
         qH:GET-NEXT(NO-LOCK).
      END.
   END.
   ELSE DO:
      FOR EACH kon_val:
         queryvar = "FOR EACH " + berkalknumbuffh:TABLE  + " WHERE KALKNUM.KALKNR = " + berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE + 
         " AND KALKNUM.OMRADE = '" + berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE + 
         "' AND KALKNUM.BERNUM = " + STRING(kon_val.NUM) + " NO-LOCK".
         RUN CreateCustomQuery(INPUT berkalknumbuffh,INPUT queryvar,OUTPUT qh).
         qH:GET-FIRST(NO-LOCK).
         DO WHILE qH:QUERY-OFF-END = FALSE:
            CREATE ber_temp.
            BUFFER ber_temp:HANDLE:BUFFER-COPY(berkalknumbuffh,"NUM").
            /*
            ber_temp.NUM = berkalknumbuffh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE.
            */
            ber_temp.NUM = kon_val.ORD.
            IF berkalknumbuffh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE = 0 THEN DO:
               FIND FIRST KALKBER WHERE KALKBER.ARBKOD = ber_temp.ARBKOD AND KALKBER.LOPNR = ber_temp.LOPNR NO-LOCK NO-ERROR.
               IF AVAILABLE KALKBER THEN DO:
                  ASSIGN
                  ber_temp.BENAMNING = KALKBER.BENAMNING
                  ber_temp.ENHET = KALKBER.ENHET. 
               END.   
            END.      
            qH:GET-NEXT(NO-LOCK).
         END.
  
      END.
   END.
   DELETE WIDGET-POOL "DynTableLI" NO-ERROR.
   DELETE OBJECT berkopptabbuffh NO-ERROR.
   berkopptabbuffh = ?.
   DELETE OBJECT berkalknumbuffh NO-ERROR.
   berkalknumbuffh = ?.
END PROCEDURE.
PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL "DynTableLI".
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.
PROCEDURE Kalkkod_UI :
         
   IF valkonst = FALSE THEN DO:   
      OPEN QUERY kalkq FOR EACH BERKALK WHERE BERKALK.AONR = valaonr 
      AND BERKALK.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(BERKALK):      
         CREATE ber_temp.
         ASSIGN
         ber_temp.ARBKOD = BERKALK.ARBKOD
         ber_temp.LOPNR = BERKALK.LOPNR
         ber_temp.BENAMNING = BERKALK.BENAMNING
         ber_temp.ENHET = BERKALK.ENHET
         ber_temp.ANTAL = BERKALK.ANTAL
         ber_temp.NUM = BERKALK.NUM.      
         GET NEXT kalkq NO-LOCK.
      END.
      CLOSE QUERY kalkq.
   END.
   ELSE DO:
      FOR EACH kon_val:
         OPEN QUERY mtrlprisq FOR EACH BERKALK WHERE BERKALK.AONR = kon_val.BERAONR AND
         BERKALK.OMRADE = kon_val.OMRADE AND BERKALK.NUM = kon_val.NUM 
         NO-LOCK.
         GET FIRST mtrlprisq NO-LOCK.
         DO WHILE AVAILABLE(BERKALK):      
            CREATE ber_temp.
            ASSIGN
            ber_temp.ARBKOD = BERKALK.ARBKOD
            ber_temp.LOPNR = BERKALK.LOPNR
            ber_temp.BENAMNING = BERKALK.BENAMNING
            ber_temp.ENHET = BERKALK.ENHET
            ber_temp.ANTAL = BERKALK.ANTAL
            ber_temp.NUM = kon_val.ORD.
            GET NEXT mtrlprisq NO-LOCK.
         END.    
         CLOSE QUERY mtrlprisq. 
      END.
   END.
END PROCEDURE.