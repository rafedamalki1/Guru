/*BERVALSEL.I*/
DEFINE VARIABLE extrarow AS ROWID NO-UNDO.
PROCEDURE pselectfoc_UI :
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   RUN vh_UI.
   RUN pselectrowid_UI IN brwproc[brwvald] (OUTPUT brwrowid).
   RUN ff_UI (INPUT brwvald,INPUT brwrowid).  
END PROCEDURE.
PROCEDURE nselectfoc_UI :
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   RUN vh_UI.
   RUN nselectrowid_UI IN brwproc[brwvald] (OUTPUT brwrowid).
   RUN ff_UI (INPUT brwvald,INPUT brwrowid).    
END PROCEDURE.
PROCEDURE fselectfoc_UI :
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   RUN vh_UI.
   RUN fetrowid_UI IN brwproc[brwvald] (INPUT antal_raknare,OUTPUT brwrowid).
   RUN ff_UI (INPUT brwvald,INPUT brwrowid).  
END PROCEDURE.
PROCEDURE selectfoc_UI :
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   RUN vh_UI.
   RUN selectrowid_UI IN brwproc[brwvald] (OUTPUT brwrowid).
   RUN selectrecid_UI IN brwproc[brwvald] (OUTPUT brwrecid).
   RUN ff_UI (INPUT brwvald,INPUT brwrowid).    
   extrarow = brwrowid.
END PROCEDURE.

PROCEDURE ff_UI :
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER brwrowid AS ROWID NO-UNDO.
   IF brwvald = 1 THEN FIND FIRST fastanmtemp WHERE ROWID(fastanmtemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 2 THEN FIND FIRST konstvaltemp WHERE ROWID(konstvaltemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 3 THEN FIND FIRST konstvaltemp WHERE ROWID(konstvaltemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 4 THEN FIND FIRST konstvaltemp WHERE ROWID(konstvaltemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 5 THEN FIND FIRST konstvaltemp WHERE ROWID(konstvaltemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 6 THEN FIND FIRST konstvaltemp WHERE ROWID(konstvaltemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 7 THEN FIND FIRST andrakon_val WHERE ROWID(andrakon_val) = brwrowid NO-LOCK NO-ERROR.  
   IF brwvald = 8 THEN FIND FIRST kalk_temp WHERE ROWID(kalk_temp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 9 THEN FIND FIRST konstvaltemp WHERE ROWID(konstvaltemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 10 THEN FIND FIRST konstvaltemp WHERE ROWID(konstvaltemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 11 THEN FIND FIRST konstvaltemp WHERE ROWID(konstvaltemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 12 THEN FIND FIRST konstvaltemp WHERE ROWID(konstvaltemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 13 THEN FIND FIRST konstvaltemp WHERE ROWID(konstvaltemp) = brwrowid NO-LOCK NO-ERROR.         

   IF brwvald = 14 THEN FIND FIRST konsttemp WHERE ROWID(konsttemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 15 THEN FIND FIRST list_mtrl WHERE ROWID(list_mtrl) = brwrowid NO-LOCK NO-ERROR.         
   
   IF brwvald = 18 THEN FIND FIRST berfortemp WHERE ROWID(berfortemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 19 THEN FIND FIRST berlinjetemp WHERE ROWID(berlinjetemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 20 THEN FIND FIRST bernattemp WHERE ROWID(bernattemp) = brwrowid NO-LOCK NO-ERROR.
   /*         
   IF brwvald = 21 THEN FIND FIRST kon_val WHERE ROWID(kon_val) = brwrowid NO-LOCK NO-ERROR.
   */         
   IF brwvald = 21 THEN DO:
      FIND FIRST kon_display WHERE ROWID(kon_display) = brwrowid NO-LOCK NO-ERROR.
      IF AVAILABLE kon_display THEN DO:
         FIND FIRST kon_val WHERE kon_val.NUM = kon_display.NUM AND kon_val.KSKAP = FALSE NO-LOCK NO-ERROR.
      END.   
   END.   
   IF brwvald = 22 THEN FIND FIRST mtrltemp WHERE ROWID(mtrltemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 23 THEN FIND FIRST satstemp WHERE ROWID(satstemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 24 THEN FIND FIRST berstolptemp WHERE ROWID(berstolptemp) = brwrowid NO-LOCK NO-ERROR.         
   IF brwvald = 25 THEN FIND FIRST bildbertemp WHERE ROWID(bildbertemp) = brwrowid NO-LOCK NO-ERROR.
   IF brwvald = 26 THEN FIND FIRST soktamtrlfavo WHERE ROWID(soktamtrlfavo) = brwrowid NO-LOCK NO-ERROR.
   IF brwvald = 27 THEN FIND FIRST konsttemp WHERE ROWID(konsttemp) = brwrowid NO-LOCK NO-ERROR. 
          
END PROCEDURE.
PROCEDURE vh_UI :
   IF NOT VALID-HANDLE(berfortemph) THEN berfortemph = TEMP-TABLE  berfortemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(berlinjetemph) THEN berlinjetemph = TEMP-TABLE  berlinjetemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(bernattemph) THEN bernattemph = TEMP-TABLE  bernattemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(berstolptemph) THEN berstolptemph = TEMP-TABLE  berstolptemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(bildbertemph ) THEN bildbertemph = TEMP-TABLE  bildbertemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(fastanmtemph ) THEN fastanmtemph = TEMP-TABLE  fastanmtemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(konstgrptemph) THEN konstgrptemph = TEMP-TABLE  konstgrptemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(konsttemph ) THEN konsttemph = TEMP-TABLE  konsttemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(konstvaltemph) THEN konstvaltemph = TEMP-TABLE  konstvaltemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(mtrltemph) THEN mtrltemph = TEMP-TABLE  mtrltemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(soktamtrlfavoh) THEN soktamtrlfavoh = TEMP-TABLE  soktamtrlfavo:DEFAULT-BUFFER-HANDLE.
   
   IF NOT VALID-HANDLE(satstemph ) THEN satstemph = TEMP-TABLE  satstemp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(kalk_temph) THEN kalk_temph = TEMP-TABLE  kalk_temp:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(list_mtrlh) THEN list_mtrlh = TEMP-TABLE  list_mtrl:DEFAULT-BUFFER-HANDLE.
   /*
   IF NOT VALID-HANDLE(kon_valh ) THEN kon_valh = TEMP-TABLE  kon_val:DEFAULT-BUFFER-HANDLE.
   */
   IF NOT VALID-HANDLE(kon_valh ) THEN kon_valh = TEMP-TABLE  kon_display:DEFAULT-BUFFER-HANDLE.
   IF NOT VALID-HANDLE(andrakon_valh ) THEN andrakon_valh = TEMP-TABLE  kskkon_val:DEFAULT-BUFFER-HANDLE.
END PROCEDURE.
PROCEDURE dselectfoc_UI :
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   RUN dselectfoc_UI IN brwproc[brwvald].
      
END PROCEDURE.
