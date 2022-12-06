/*XBRWSOK.W*/
DEFINE VARIABLE brwvar AS INTEGER NO-UNDO.     
DEFINE VARIABLE brwopen AS LOGICAL EXTENT 10 NO-UNDO.
DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.
DEFINE VARIABLE brwwh AS WIDGET-HANDLE.
DEFINE VARIABLE brwsortvar AS INTEGER NO-UNDO.
/*enabla ett fält FÖR JOIN QUERY SE ÄVEN HITTAJOIN_UI obs inga row mark*/ 
   /*MAIN*/
   ASSIGN
   &Scoped-define BROWSE-NAME BRW_ANSV
   ANSVARIGTAB.PERSONALKOD:READ-ONLY IN BROWSE   {&BROWSE-NAME} = TRUE    
   &Scoped-define BROWSE-NAME BRW_AVD
   AVDELNING.AVDELNINGNR:READ-ONLY IN BROWSE     {&BROWSE-NAME} = TRUE
   &Scoped-define BROWSE-NAME BRW_GODK
   GODKANNARTAB.PERSONALKOD:READ-ONLY IN BROWSE  {&BROWSE-NAME} = TRUE 
      &Scoped-define BROWSE-NAME BRW_MARK
   markpers.PERSONALKOD:READ-ONLY IN BROWSE      {&BROWSE-NAME} = TRUE
   &Scoped-define BROWSE-NAME BRW_OMR
   OMRADETAB.OMRADE:READ-ONLY IN BROWSE          {&BROWSE-NAME} = TRUE
   &Scoped-define BROWSE-NAME BRW_PERS
   PERSONALTAB.PERSONALKOD:READ-ONLY IN BROWSE   {&BROWSE-NAME} = TRUE
   &Scoped-define BROWSE-NAME BRW_VAVD
   avdelningtemp.AVDELNINGNR:READ-ONLY IN BROWSE {&BROWSE-NAME} = TRUE
   &Scoped-define BROWSE-NAME BRW_VOMR
   omrtemp.OMRADE:READ-ONLY IN BROWSE            {&BROWSE-NAME} = TRUE.
/*TRIGGER*/
ON ANY-PRINTABLE OF BRW_AVD IN FRAME FRAME-B /* Avdelningar */
DO:
   brwsok = LAST-EVENT:LABEL. 
   RUN hitta_UI.
   {musarrow.i}
   multitid = ETIME.
END.
ON START-SEARCH OF BRW_ANSV IN FRAME FRAME-B /* Ansvariga tidredovisare */
DO:
   IF brwvar NE 1 THEN brwsortvar = 0.
   brwvar = 1.
   brwwh = BRW_ANSV:CURRENT-COLUMN.   
   brwsok = LAST-EVENT:LABEL.     
   {muswait.i}   
   RUN opensok_UI.
   {musarrow.i}
END.


/*PROCEDURER*/
PROCEDURE hitta_UI :
   {muswait.i}
   IF ETIME - multitid <= 500 THEN DO:
      multibrwsok = multibrwsok + brwsok.
   END.
   ELSE multibrwsok = brwsok.
   IF brwvar = 1 THEN DO:
      /*ANSVARIG*/
      &Scoped-define BROWSE-NAME BRW_ANSV      
      IF brwsortvar = 1 THEN DO:                      
         FIND NEXT ANSVARIGTAB WHERE ANSVARIGTAB.PERSONALKOD BEGINS multibrwsok
         NO-LOCK NO-ERROR. 
         IF NOT AVAILABLE ANSVARIGTAB THEN DO:
            FIND FIRST ANSVARIGTAB WHERE ANSVARIGTAB.PERSONALKOD BEGINS multibrwsok
            NO-LOCK NO-ERROR. 
         END.                    
      END.   
      IF brwsortvar = 2 THEN DO:
         FIND NEXT ANSVARIGTAB WHERE ANSVARIGTAB.FORNAMN BEGINS multibrwsok
         NO-LOCK NO-ERROR. 
         IF NOT AVAILABLE ANSVARIGTAB THEN DO:
            FIND FIRST ANSVARIGTAB WHERE ANSVARIGTAB.FORNAMN BEGINS multibrwsok
            NO-LOCK NO-ERROR. 
         END.      
        
      END.
      IF brwsortvar = 3 THEN DO:
         FIND NEXT ANSVARIGTAB WHERE ANSVARIGTAB.EFTERNAMN BEGINS multibrwsok
         NO-LOCK NO-ERROR. 
         IF NOT AVAILABLE ANSVARIGTAB THEN DO:
            FIND FIRST ANSVARIGTAB WHERE ANSVARIGTAB.EFTERNAMN BEGINS multibrwsok
            NO-LOCK NO-ERROR. 
         END.      
        
      END.
      IF AVAILABLE ANSVARIGTAB THEN DO:        
         RUN repo_UI (INPUT RECID(ANSVARIGTAB)).
         status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.        
      END.
   END.       
   
   {musarrow.i}
END PROCEDURE.



PROCEDURE opensok_UI :
   {muswait.i}     
   IF brwvar = 1 THEN DO:
      /*ANSVARIG*/    
      CASE brwwh:NAME:   
         WHEN "PERSONALKOD" THEN DO:
            IF brwsortvar = 1 THEN RETURN.
            brwsortvar = 1.
            OPEN QUERY BRW_ANSV FOR EACH ANSVARIGTAB NO-LOCK.      
            RETURN.
         END.
         WHEN "FORNAMN" THEN DO:
            IF brwsortvar = 2 THEN RETURN.
            brwsortvar = 2.
            OPEN QUERY BRW_ANSV FOR EACH ANSVARIGTAB NO-LOCK BY ANSVARIGTAB.FORNAMN.
            RETURN.
         END.
         WHEN "EFTERNAMN" THEN DO:
            IF brwsortvar = 3 THEN RETURN.
            brwsortvar = 3.
            OPEN QUERY BRW_ANSV FOR EACH ANSVARIGTAB NO-LOCK BY ANSVARIGTAB.EFTERNAMN.
            RETURN.
         END.
      END CASE.
   END.
END PROCEDURE.


PROCEDURE hittajoin_UI :
   {muswait.i}
   IF ETIME - multitid <= 500 THEN DO:
      multibrwsok = multibrwsok + brwsok.
   END.
   ELSE multibrwsok = brwsok.
   IF brwvar = 1 THEN DO:
      /*ANvändare*/
      &Scoped-define BROWSE-NAME BRW_ANV      
      IF brwsortvar = 2 THEN DO:
         REPEAT:
            FIND NEXT PERSONALTAB WHERE PERSONALTAB.FORNAMN BEGINS multibrwsok
            NO-LOCK NO-ERROR. 
            IF NOT AVAILABLE PERSONALTAB THEN DO:
               FIND FIRST PERSONALTAB WHERE PERSONALTAB.FORNAMN BEGINS multibrwsok
               NO-LOCK NO-ERROR. 
               FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.         
               IF AVAILABLE ANVANDARE THEN LEAVE.
               ELSE RETURN.
            END.
            IF NOT AVAILABLE PERSONALTAB THEN RETURN.
            FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.         
            IF AVAILABLE ANVANDARE THEN LEAVE.
         END.                      
      END.
      IF brwsortvar = 3 THEN DO:
         REPEAT:
            FIND NEXT PERSONALTAB WHERE PERSONALTAB.EFTERNAMN BEGINS multibrwsok
            NO-LOCK NO-ERROR. 
            IF NOT AVAILABLE PERSONALTAB THEN DO:
               FIND FIRST PERSONALTAB WHERE PERSONALTAB.EFTERNAMN BEGINS multibrwsok
               NO-LOCK NO-ERROR. 
               FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.         
               IF AVAILABLE ANVANDARE THEN LEAVE.
               ELSE RETURN.
            END.
            IF NOT AVAILABLE PERSONALTAB THEN RETURN.
            FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.         
            IF AVAILABLE ANVANDARE THEN LEAVE.
         END.           
      END.
      IF AVAILABLE ANVANDARE THEN DO:            
         RUN repo_UI (INPUT RECID(ANVANDARE)).
         status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.        
      END.
   END.       
   {musarrow.i}
END PROCEDURE.
