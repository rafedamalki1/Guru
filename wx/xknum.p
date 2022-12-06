/*XKNUM.P*/
{KALKYLKAT.I}
{KALKTEMP2.I}

DEFINE VARIABLE LocalAppServerHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE nyKalkNrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE BUFFER KALKNUMBUF FOR KALKNUM.
CREATE kalk_temp.
ASSIGN
kalk_temp.ARBKOD = "110" 
kalk_temp.LOPNR = 12
kalk_temp.ANTAL = 1   
kalk_temp.NUM = 2.
RUN KALKBERAPPDS.p PERSISTENT SET LocalAppServerHandle (INPUT CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)).  
RUN Kalkspara_UI.       
PROCEDURE Kalkspara_UI:
   
   EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
   
   FOR EACH kalk_temp WHERE NO-LOCK:
      FIND FIRST KALKNUM WHERE KALKNUM.KALKNR = 101126 AND
      KALKNUM.OMRADE = "0910" AND KALKNUM.BERNUM = kalk_temp.NUM AND
      KALKNUM.ARBKOD = kalk_temp.ARBKOD AND KALKNUM.LOPNR = kalk_temp.LOPNR
      NO-LOCK NO-ERROR.
      
         
         CREATE KalkylimportTT.
         ASSIGN
         KalkylimportTT.TTRECID = RECID(KalkylimportTT) 
         KalkylimportTT.KALKNR = 101126
         KalkylimportTT.OMRADE = "0910"
         KalkylimportTT.MATRIS = 1
         KalkylimportTT.ARBKOD = kalk_temp.ARBKOD 
         KalkylimportTT.LOPNR = kalk_temp.LOPNR
         KalkylimportTT.ANTAL = kalk_temp.ANTAL
         KalkylimportTT.BERNUM = kalk_temp.NUM.
            
   END.
   RUN kalknumskap_UI.
   
   
 
  EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
  RELEASE KALKNUM NO-ERROR.   
END PROCEDURE.

PROCEDURE kalknumskap_UI :
   DEFINE VARIABLE ksid LIKE KALKNUM.KLOGSUBID NO-UNDO.
   DEFINE VARIABLE numnummer AS INTEGER NO-UNDO.
   DEFINE VARIABLE numsubnummer AS INTEGER NO-UNDO.
   DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
   FIND FIRST KalkylimportTT  WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KalkylimportTT THEN RETURN.
   IF AVAILABLE KalkylimportTT THEN DO:
      FIND FIRST kalkylloppostertt WHERE trim(kalkylloppostertt.ARBKOD) = TRIM(KalkylimportTT.ARBKOD)
      AND kalkylloppostertt.LOPNR = KalkylimportTT.LOPNR  NO-LOCK NO-ERROR.
      IF AVAILABLE kalkylloppostertt THEN.
      ELSE RUN kathmt_UI IN LocalAppServerHandle (INPUT KalkylimportTT.KALKNR,INPUT KalkylimportTT.OMRADE , 
      INPUT CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),OUTPUT felmed, OUTPUT TABLE kalkylarbkodertt,
      OUTPUT TABLE kalkylloppostertt,OUTPUT TABLE kalkylkatalogtt, OUTPUT TABLE markfiltertt,OUTPUT TABLE kalkyldelkatalogtt).
   END.
   FIND LAST KALKNUMbuf WHERE KALKNUMbuf.KALKNR = KalkylimportTT.KALKNR AND KALKNUMbuf.OMRADE = KalkylimportTT.OMRADE USE-INDEX NUM
   NO-LOCK NO-ERROR.
   IF AVAILABLE KALKNUMbuf THEN numnummer = KALKNUMbuf.NUM.
   ELSE numnummer = 0.
   
   FOR EACH KalkylimportTT WHERE NO-LOCK:
      FIND FIRST kalkylloppostertt WHERE trim(kalkylloppostertt.ARBKOD) = TRIM(KalkylimportTT.ARBKOD)
      AND kalkylloppostertt.LOPNR = KalkylimportTT.LOPNR  NO-LOCK NO-ERROR.
      IF AVAILABLE kalkylloppostertt THEN DO:
         DO TRANSACTION: 
            numnummer = numnummer + 1.
            CREATE KALKNUM. 
            
            BUFFER-COPY KalkylimportTT EXCEPT KalkylimportTT.NUM  TO KALKNUM.
            BUFFER-COPY kalkylloppostertt TO KALKNUM.
         
          
           ASSIGN
           KALKNUM.NUM        = numnummer
           KALKNUM.KALKNR     = KalkylimportTT.KALKNR
           KALKNUM.OMRADE     = KalkylimportTT.OMRADE
           KALKNUM.TYPKALK    = 2.
           /*
           KALKNUM.KLOGSUBID  = kalkylloppostertt.KLOGSUBID.
           KALKNUM.BENAMNING = kalkylloppostertt.BENAMNING.
           KALKNUM.ENHET     = kalkylloppostertt.ENHET.
           KALKNUM.KOMMENTAR = kalkylloppostertt.KOMMENTA .
           KALKNUM.MARKNING  = kalkylloppostertt.MARKNING.  
           KALKNUM.MARKSUB   = kalkylloppostertt.MARKSUB.
           */
        END.
        RELEASE KALKNUM NO-ERROR.
        EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.   
        RUN skapanumsub_UI IN LocalAppServerHandle (INPUT kalkylloppostertt.KLOGSUBID,INPUT KalkylimportTT.ARBKOD,INPUT KalkylimportTT.LOPNR,OUTPUT TABLE ekalknumsubtt).
        numsubnummer = 0.
        FIND LAST KALKNUMSUB WHERE 
        KALKNUMSUB.KALKNR = KALKNUM.KALKNR AND KALKNUMSUB.OMRADE = KALKNUM.OMRADE AND
        KALKNUMSUB.NUM = KALKNUM.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
        IF AVAILABLE KALKNUMSUB THEN numsubnummer =  KALKNUMSUB.NUMSUBID + 1.
        DO TRANSACTION:
           FOR EACH ekalknumsubtt WHERE NO-LOCK:
              ASSIGN 
              ekalknumsubtt.KALKNR = KalkylimportTT.KALKNR
              ekalknumsubtt.OMRADE = KalkylimportTT.OMRADE
              ekalknumsubtt.NUM = numnummer.
              numsubnummer = numsubnummer + 1.
              CREATE KALKNUMSUB.
              BUFFER-COPY ekalknumsubtt TO KALKNUMSUB.
              KALKNUMSUB.NUMSUBID = numsubnummer.
              DELETE ekalknumsubtt.                      
           END.
        END.   
        RELEASE KALKNUMSUB NO-ERROR.
        EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.
     END.   
  END.
  EMPTY TEMP-TABLE KalkylimportTT NO-ERROR.
  RELEASE KALKNUM NO-ERROR.
  RELEASE KALKNUMSUB NO-ERROR.
  RELEASE KALKNUMbuf NO-ERROR.
END PROCEDURE.

/*
DEFINE BUFFER KALKNUMbuf FOR KALKNUM.
RUN Kalkspara_UI (1).
PROCEDURE Kalkspara_UI:
   
   DEFINE INPUT  PARAMETER valnum AS INTEGER NO-UNDO.
   
   
         CREATE KalkylimportTT.
         ASSIGN
         KalkylimportTT.TTRECID = RECID(KalkylimportTT) 
         KalkylimportTT.KALKNR = 101126
         KalkylimportTT.OMRADE = "0910"
         KalkylimportTT.MATRIS = 1
         KalkylimportTT.ARBKOD = "110" 
         KalkylimportTT.LOPNR = 12
         KalkylimportTT.ANTAL = 1   
         KalkylimportTT.BERNUM = valnum.
    FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkylimportTT.KALKNR NO-LOCK NO-ERROR.   
   
   RUN kalknumskap_UI.
   
   
 
  EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
  RELEASE KALKNUM NO-ERROR.   
END PROCEDURE.

PROCEDURE kalknumskap_UI :
   DEFINE VARIABLE ksid LIKE KALKNUM.KLOGSUBID NO-UNDO.
   DEFINE VARIABLE numnummer AS INTEGER NO-UNDO.
   DEFINE VARIABLE numsubnummer AS INTEGER NO-UNDO.
   DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
   FIND FIRST KalkylimportTT  WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KalkylimportTT THEN RETURN.
   IF AVAILABLE KalkylimportTT THEN DO:
   FIND LAST KALKNUMbuf WHERE KALKNUMbuf.KALKNR = KALKHUV.KALKNR AND KALKNUMbuf.OMRADE = KALKHUV.OMRADE USE-INDEX NUM
   NO-LOCK NO-ERROR.
   IF AVAILABLE KALKNUMbuf THEN numnummer = KALKNUMbuf.NUM.
   ELSE numnummer = 0.
   
   FOR EACH KalkylimportTT WHERE NO-LOCK:
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKHUV.KLOGID NO-LOCK:
         FIND FIRST KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID and
         KALKYLLOPPOSTER.arbkod = KalkylimportTT.ARBKOD AND KALKYLLOPPOSTER.lopnr = KalkylimportTT.lopnr 
         NO-LOCK NO-ERROR.
         IF AVAILABLE KALKYLLOPPOSTER THEN LEAVE.
      END.   
         DO TRANSACTION: 
            numnummer = numnummer + 1.
            CREATE KALKNUM. 
            
            BUFFER-COPY KalkylimportTT EXCEPT KalkylimportTT.NUM  TO KALKNUM.
      
          
           ASSIGN
           KALKNUM.NUM        = numnummer
           KALKNUM.KALKNR     = KALKHUV.KALKNR
           KALKNUM.OMRADE     = KALKHUV.OMRADE
           KALKNUM.TYPKALK    = KALKHUV.TYPKALK.
           KALKNUM.KLOGSUBID  = KALKYLKATALOGSUB.KLOGSUBID.
           KALKNUM.BENAMNING  = KALKYLKATALOGSUB.BENAMNING.
          
        END.
        numsubnummer = 0.
        FIND LAST KALKNUMSUB WHERE 
        KALKNUMSUB.KALKNR = KALKNUM.KALKNR AND KALKNUMSUB.OMRADE = KALKNUM.OMRADE AND
        KALKNUMSUB.NUM = KALKNUM.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
        IF AVAILABLE KALKNUMSUB THEN numsubnummer =  KALKNUMSUB.NUMSUBID + 1.
        DO TRANSACTION:
            FOR EACH KALKYLLOPSUB WHERE KALKYLLOPSUB.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND 
            KALKYLLOPSUB.ARBKOD = KALKNUM.ARBKOD AND KALKYLLOPSUB.LOPNR = KALKNUM.LOPNR 
            NO-LOCK:
              ASSIGN 
              numsubnummer = numsubnummer + 1.
              CREATE KALKNUMSUB.
              BUFFER-COPY KALKYLLOPSUB TO KALKNUMSUB.
              ASSIGN 
              KALKNUMSUB.NUMSUBID = numsubnummer
              KALKNUMSUB.KALKNR = KALKHUV.KALKNR
              KALKNUMSUB.OMRADE = KALKHUV.OMRADE
              KALKNUMSUB.NUM = numnummer.
                                    
           END.
        END.   
        RELEASE KALKNUMSUB NO-ERROR.

     END.   
  END.
  EMPTY TEMP-TABLE KalkylimportTT NO-ERROR.
  RELEASE KALKNUM NO-ERROR.
  RELEASE KALKNUMSUB NO-ERROR.
  RELEASE KALKNUMbuf NO-ERROR.
END PROCEDURE.
*/
/*
FOR EACH KALKNUM  WHERE KALKNUM.KALKNR = 101126 NO-LOCK:
      DISPLAY KALKNUM.ARBKOD KALKNUM.LOPNR.
      FOR EACH KALKNUMSUB  WHERE KALKNUMSUB.KALKNR = KALKNUM.KALKNR AND KALKNUMSUB.KALKNR = KALKNUM.KALKNR AND 
      KALKNUMSUB.num  = KALKNUM.NUM   NO-LOCK BY KALKNUMSUB.num BY KALKNUMSUB.numsubid :
         DISPLAY KALKNUMSUB.num KALKNUMSUB.NUMSUBID KALKNUMSUB.BENAMNING. 
      END. 
   END.
   */
