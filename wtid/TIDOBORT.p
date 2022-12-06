/*TIDOBORT.P*/
{APP.I}
&Scoped-define NEW NEW
{TIDALLT.I}
DEFINE INPUT PARAMETER ganv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER dbrwbdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER dbrwavdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR extraovertemp.
DEFINE OUTPUT PARAMETER TABLE FOR overtemp.
DEFINE NEW SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.  
DEFINE VARIABLE regdatumhj AS DATE NO-UNDO. 
DEFINE VARIABLE tidtabrecspar AS RECID NO-UNDO.
DEFINE VARIABLE nyber AS INTEGER NO-UNDO.
DEFINE VARIABLE ostart AS DECIMAL NO-UNDO. 
DEFINE VARIABLE krav5 AS INTEGER NO-UNDO. 
DEFINE VARIABLE krav6 AS INTEGER NO-UNDO.


FIND FIRST FORETAG NO-LOCK NO-ERROR.
ASSIGN
brwbdatum = dbrwbdatum 
brwavdatum = dbrwavdatum
Guru.Konstanter:globforetag = FORETAG.FORETAG.
FIND FIRST extraovertemp NO-ERROR.
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = extraovertemp.PERSONALKOD NO-LOCK NO-ERROR.
persrec = RECID(PERSONALTAB).
Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
{GDPRLOGGCLIENT.I}
FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = extraovertemp.RECTIDVIS NO-ERROR.
IF NOT AVAILABLE TIDREGITAB THEN RETURN.
tidtabrec = RECID(TIDREGITAB).

IF vadgora = 5 THEN DO:
   RUN bortotid_UI.
END.
PROCEDURE bortotid_UI :
   regdatum = TIDREGITAB.DATUM.
   RUN REGVEC.P.
   RUN SLUTARB.P.
   DO TRANSACTION:      
      FIND FIRST TIDREGITAB WHERE RECID(TIDREGITAB) = extraovertemp.RECTIDVIS EXCLUSIVE-LOCK NO-ERROR.
      IF TIDREGITAB.TIDLOG = FALSE THEN DO:
         DELETE TIDREGITAB.
      END.   
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.OVERAUTO = FALSE 
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.OVERAUTO = TRUE AND
         TIDREGITAB.TIDLOG = TRUE AND 
         (TIDREGITAB.START GE regslut OR TIDREGITAB.START < regstart) 
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.
         ELSE DO:
            tidtabrecspar = tidtabrec.
            tidtabrec = RECID(TIDREGITAB).
            bustart3 = TIDREGITAB.START.
            RUN OTOLKPR.P.
            IF TIDREGITAB.OANT1 > 0 THEN DO:
               CREATE overtemp.
               tidtabrec = RECID(overtemp).
               IF TIDREGITAB.OKOD1 = "" THEN tidtabrec = tidtabrecspar.
               ELSE DO:
                  tidtabrec2 = tidtabrec.
                  tidtabrec = 0.
               END.
               ASSIGN overtemp.AONR = TIDREGITAB.AONR 
               overtemp.DAG = TIDREGITAB.DAG 
               overtemp.DELNR = TIDREGITAB.DELNR 
               overtemp.OVERANTAL = TIDREGITAB.OANT1 
               overtemp.OVERAUTO = TIDREGITAB.OVERAUTO 
               overtemp.OVERTIDTILL = TIDREGITAB.OKOD1
               overtemp.DATUM = TIDREGITAB.DATUM
               overtemp.RECTIDVIS = RECID(TIDREGITAB).
               IF TIDREGITAB.OANT2 > 0 THEN DO:
                  CREATE overtemp.
                  ASSIGN overtemp.AONR = TIDREGITAB.AONR 
                  overtemp.DAG = TIDREGITAB.DAG 
                  overtemp.DELNR = TIDREGITAB.DELNR 
                  overtemp.OVERANTAL = TIDREGITAB.OANT2 
                  overtemp.OVERAUTO = TIDREGITAB.OVERAUTO 
                  overtemp.OVERTIDTILL = TIDREGITAB.OKOD2
                  overtemp.DATUM = TIDREGITAB.DATUM
                  overtemp.RECTIDVIS = RECID(TIDREGITAB).
               END.
               IF TIDREGITAB.OANT3 > 0 THEN DO:
                  CREATE overtemp.
                  ASSIGN overtemp.AONR = TIDREGITAB.AONR 
                  overtemp.DAG = TIDREGITAB.DAG 
                  overtemp.DELNR = TIDREGITAB.DELNR 
                  overtemp.OVERANTAL = TIDREGITAB.OANT3 
                  overtemp.OVERAUTO = TIDREGITAB.OVERAUTO 
                  overtemp.OVERTIDTILL = TIDREGITAB.OKOD3
                  overtemp.DATUM = TIDREGITAB.DATUM
                  overtemp.RECTIDVIS = RECID(TIDREGITAB).
               END.
            END. 
            IF TIDREGITAB.OKOD1 = "" THEN tidtabrec = tidtabrecspar.    
            REPEAT:
               FIND NEXT TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.OVERAUTO = TRUE AND
               TIDREGITAB.TIDLOG = TRUE AND 
               (TIDREGITAB.START GE regslut OR TIDREGITAB.START < regstart) 
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN LEAVE.
               ELSE DO:
                  tidtabrecspar = tidtabrec.
                  tidtabrec = RECID(TIDREGITAB).
                  bustart3 = TIDREGITAB.START.
                  RUN OTOLKPR.P.
                  IF TIDREGITAB.OANT1 > 0 THEN DO:
                     CREATE overtemp.
                     tidtabrec = RECID(overtemp).
                     IF TIDREGITAB.OKOD1 = "" THEN tidtabrec = tidtabrecspar.
                     ASSIGN overtemp.AONR = TIDREGITAB.AONR 
                     overtemp.DAG = TIDREGITAB.DAG 
                     overtemp.DELNR = TIDREGITAB.DELNR 
                     overtemp.OVERANTAL = TIDREGITAB.OANT1 
                     overtemp.OVERAUTO = TIDREGITAB.OVERAUTO 
                     overtemp.OVERTIDTILL = TIDREGITAB.OKOD1
                     overtemp.DATUM = TIDREGITAB.DATUM
                     overtemp.RECTIDVIS = RECID(TIDREGITAB).
                     IF TIDREGITAB.OANT2 > 0 THEN DO:
                        CREATE overtemp.
                        ASSIGN overtemp.AONR = TIDREGITAB.AONR 
                        overtemp.DAG = TIDREGITAB.DAG 
                        overtemp.DELNR = TIDREGITAB.DELNR 
                        overtemp.OVERANTAL = TIDREGITAB.OANT2 
                        overtemp.OVERAUTO = TIDREGITAB.OVERAUTO 
                        overtemp.OVERTIDTILL = TIDREGITAB.OKOD2
                        overtemp.DATUM = TIDREGITAB.DATUM
                        overtemp.RECTIDVIS = RECID(TIDREGITAB).
                     END.
                     IF TIDREGITAB.OANT3 > 0 THEN DO:
                        CREATE overtemp.
                        ASSIGN overtemp.AONR = TIDREGITAB.AONR 
                        overtemp.DAG = TIDREGITAB.DAG 
                        overtemp.DELNR = TIDREGITAB.DELNR 
                        overtemp.OVERANTAL = TIDREGITAB.OANT3 
                        overtemp.OVERAUTO = TIDREGITAB.OVERAUTO 
                        overtemp.OVERTIDTILL = TIDREGITAB.OKOD3
                        overtemp.DATUM = TIDREGITAB.DATUM
                        overtemp.RECTIDVIS = RECID(TIDREGITAB).
                     END.
                  END.     
                  IF TIDREGITAB.OKOD1 = "" THEN tidtabrec = tidtabrecspar.
               END.
            END.
         END.     
      END.          
      RETURN.
   END.
END PROCEDURE.
