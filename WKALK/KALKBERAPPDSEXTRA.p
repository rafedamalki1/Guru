
/*------------------------------------------------------------------------
    File        : KALKBERAPPDSEXTRA.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Thu Aug 30 08:30:08 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/
{SCADMIN.I}
{KONVALTEMP.I}
DEFINE TEMP-TABLE ber_temp  NO-UNDO
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER    
   FIELD ANTAL AS DECIMAL
   FIELD NUM AS INTEGER
   INDEX KOD ARBKOD LOPNR ASCENDING
   INDEX NUM NUM ARBKOD LOPNR ASCENDING.

{KALKYLKAT.I}
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
{GLOBVAR2DEL1.I}
 
{KBETEMP.I}
{EXTRADATA.I}
{EXTRATAB.I}
{LISTDEF.I}

DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE finnskkod AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER inglobanv AS CHARACTER NO-UNDO.

DEFINE BUFFER KALKNUMbuffsista FOR KALKNUM.
DEFINE BUFFER KALKNUMSUBbuffsista FOR KALKNUMSUB.
DEFINE VARIABLE aonrplan AS LOGICAL NO-UNDO. /* = true aonr = false planr*/


FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
DEFINE BUFFER KALKNUMBUF FOR KALKNUM.
DEFINE BUFFER KALKNUMSUBBUF FOR KALKNUMSUB.
DEFINE BUFFER berbuff FOR BEREDNING.

RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).

/*FRÅN AONRHUV berkalk till kalkyl*/

PROCEDURE reg_UI :
   DEFINE INPUT PARAMETER orgomrade LIKE AONRTAB.OMRADE NO-UNDO.
   DEFINE INPUT PARAMETER orgbernr AS INTEGER  NO-UNDO.
   DEFINE INPUT PARAMETER kopomrade LIKE AONRTAB.OMRADE NO-UNDO.
   DEFINE INPUT PARAMETER kopbernr AS INTEGER  NO-UNDO.
   
   DEFINE BUFFER berebuff FOR BEREDNING.
  
   FIND FIRST berebuff  WHERE berebuff.OMRADE = orgomrade AND berebuff.BERNR = orgbernr  NO-LOCK NO-ERROR.
   
   DO TRANSACTION:
      CREATE BEREDNING.
      BUFFER-COPY berebuff EXCEPT berebuff.BERNR TO BEREDNING.
      ASSIGN    
      BEREDNING.BERNR = kopbernr
      BEREDNING.BERAONR = STRING(kopbernr)
      BEREDNING.OMRADE = kopomrade
      BEREDNING.AONR = ?
      BEREDNING.DELNR = ?. 
     
   END.   
   RELEASE BEREDNING NO-ERROR.
         
END PROCEDURE.
 
 {EJVALAONR.I}
 
PROCEDURE sistafastspec_UI :
   DEFINE INPUT PARAMETER tempvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER leavevar AS LOGICAL NO-UNDO.
   FIND FIRST FASTSPEC WHERE FASTSPEC.KALKNR = tempvar NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FASTSPEC THEN DO:
      FIND FIRST KALKSPEC WHERE KALKSPEC.KALKNR = tempvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKSPEC THEN leavevar = TRUE. 
   END.
END PROCEDURE.
/*skapa kalkyl av berkalk*/
PROCEDURE kbufmtrlcreate_UI :
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE midvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE matrisvar AS INTEGER NO-UNDO.
   FOR EACH BERMTRL WHERE BERMTRL.AONR = BERVAL.AONR AND BERMTRL.OMRADE = BERVAL.OMRADE AND BERMTRL.NUM = BERVAL.NUM  NO-LOCK:
         IF BERMTRL.ANTAL >  0 THEN DO:
            IF BERVAL.UPPLAG = 0 OR BERVAL.UPPLAG = ? THEN matrisvar = 1.
            ELSE matrisvar = BERVAL.UPPLAG. 
            FIND FIRST kalktmtrlTT WHERE kalktmtrlTT.LEVKOD = BERMTRL.LEVKOD AND kalktmtrlTT.Enr = BERMTRL.ENR AND kalktmtrlTT.MATRIS = matrisvar NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kalktmtrlTT THEN DO:
               midvar = midvar + 1.
               CREATE kalktmtrlTT.
               ASSIGN
               kalktmtrlTT.OMRADE    = omrvar
               kalktmtrlTT.KALKNR    = KalkNrvar
               kalktmtrlTT.MATRIS    = matrisvar
               kalktmtrlTT.MID = midvar.
            END.   
            ASSIGN 
            kalktmtrlTT.ENR = BERMTRL.ENR
            kalktmtrlTT.BENAMNING = BERMTRL.BENAMNING
            kalktmtrlTT.ENHET = BERMTRL.ENHET
            kalktmtrlTT.BERKVANT = kalktmtrlTT.BERKVANT + BERMTRL.ANTAL
            kalktmtrlTT.NPRIS = BERMTRL.PRIS
            kalktmtrlTT.LEVKOD = BERMTRL.LEVKOD.
            kalktmtrlTT.SUMMA = kalktmtrlTT.NPRIS * kalktmtrlTT.BERKVANT.
         END.    
      END.
END PROCEDURE.
PROCEDURE kbufcreate_UI :
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER togsum AS LOGICAL NO-UNDO.
   DEFINE VARIABLE antlavar AS DECIMAL NO-UNDO.
   antlavar = 0.
   IF KALKNUM.ARBKOD = "egen" THEN DO TRANSACTION:
      CREATE KALKNUMBUF.
      
      ASSIGN
      KALKNUMBUF.KALKNR     = KALKNUM.KALKNR
      KALKNUMBUF.OMRADE     = KALKNUM.OMRADE
      KALKNUMBUF.KLOGSUBID  = KALKNUM.KLOGSUBID
      KALKNUMBUF.ARBKOD     = KALKNUM.ARBKOD
      KALKNUMBUF.LOPNR      = KALKNUM.LOPNR
      KALKNUMBUF.NUM        = KALKNUM.NUM
      KALKNUMBUF.MATRIS     = KALKNUM.MATRIS
      KALKNUMBUF.BENAMNING  = KALKNUM.BENAMNING
      KALKNUMBUF.ANTAL      = KALKNUM.ANTAL
      KALKNUMBUF.ENHET      = KALKNUM.ENHET
      KALKNUMBUF.KOMMENTAR  = KALKNUM.KOMMENTAR
      KALKNUMBUF.ANMARKNING = KALKNUM.ANMARKNING
      KALKNUMBUF.TOTKOST    = KALKNUM.TOTKOST
      KALKNUMBUF.TYPKALK    = KALKNUM.TYPKALK
      KALKNUMBUF.MARKNING   = KALKNUM.MARKNING
      KALKNUMBUF.MARKSUB    = KALKNUM.MARKSUB
      KALKNUMBUF.RISK       = KALKNUM.RISK
      KALKNUMBUF.VINST      = KALKNUM.VINST
      KALKNUMBUF.FRITOTKOST = KALKNUM.FRITOTKOST.
      FIND FIRST KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID NO-LOCK NO-ERROR.
      ASSIGN
      KALKNUMBUF.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID
      KALKNUMBUF.KALKNR  = KalkNrvar  
      KALKNUMBUF.OMRADE  = omrvar
      KALKNUMBUF.TYPKALK = 2.
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKNUM.KALKNR  AND KALKNUMSUB.OMRADE = KALKNUM.OMRADE AND KALKNUMSUB.NUM = KALKNUM.NUM NO-LOCK:
         CREATE KALKNUMSUBBUF.
         BUFFER-COPY KALKNUMSUB EXCEPT KALKNUMSUB.KALKNR TO KALKNUMSUBBUF.
         ASSIGN
         KALKNUMSUBBUF.KALKNR  = KALKNUMBUF.KALKNR  
         KALKNUMSUBBUF.OMRADE  = KALKNUMBUF.OMRADE.   
      END.
   END.
   ELSE DO:   
      IF togsum = TRUE THEN DO:
         FIND FIRST KalkylimportTT WHERE KalkylimportTT.ARBKOD = KALKNUM.ARBKOD AND KalkylimportTT.LOPNR = KALKNUM.LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KalkylimportTT THEN DO:
            CREATE KalkylimportTT.
         END. 
         antlavar =  KalkylimportTT.ANTAL.          
      END.
      ELSE CREATE KalkylimportTT.
      ASSIGN
      KalkylimportTT.TTRECID = RECID(KalkylimportTT) 
      KalkylimportTT.KALKNR = KalkNrvar
      KalkylimportTT.OMRADE = omrvar
      KalkylimportTT.MATRIS = 1
      KalkylimportTT.ARBKOD = KALKNUM.ARBKOD 
      KalkylimportTT.LOPNR = KALKNUM.LOPNR.
      KalkylimportTT.ANTAL = antlavar + KALKNUM.ANTAL.
   END.            
   RELEASE KALKNUMBUF NO-ERROR.
   RELEASE KALKNUMSUBBUF NO-ERROR.   
END PROCEDURE.
PROCEDURE BerKalkSkapa_UI :
   DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER togsum AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER togmtrl AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER togkonval AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER kbename AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR kon_val.
   DEFINE INPUT PARAMETER TABLE FOR skrivhdschakttemp.
   DEFINE OUTPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE OUTPUT PARAMETER TABLE FOR KalkylimportTT.
   DEFINE VARIABLE antlavar AS DECIMAL NO-UNDO.
   
   DEFINE VARIABLE qvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE LocalAppServerHandle AS HANDLE NO-UNDO.
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
   RUN KALKBERAPPDS.p PERSISTENT SET LocalAppServerHandle (INPUT Guru.Konstanter:globanv).
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = aonrvar AND AONRTAB.DELNR = delnrvar NO-LOCK NO-ERROR.
   omrvar = AONRTAB.OMRADE.
   
   RUN omradekoll_UI IN LocalAppServerHandle (INPUT omrvar, OUTPUT KalkNrvar).  
   IF KalkNrvar = ? THEN DO:
      CREATE tidut.
      tidut.UT =  "Det går inte att lägga upp kalkyler på detta " + LC(Guru.Konstanter:gomrk) + ". Nummerserie saknas eller är fylld.".      
      RETURN.
   END.
   FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   
   /*skapar huvud och koppling till aonr*/ 
   FIND FIRST BEREDNING WHERE BEREDNING.AONR = aonrvar AND BEREDNING.DELNR = delnrvar NO-LOCK NO-ERROR. 
   FIND FIRST BERKALKOPPLA WHERE BERKALKOPPLA.BERNR =  BEREDNING.BERNR  AND BERKALKOPPLA.OMRADE = BEREDNING.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = BERKALKOPPLA.KALKNR AND KALKHUV.OMRADE = BERKALKOPPLA.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   RUN bercopycat_UI IN LocalAppServerHandle (INPUT KalkNrvar, INPUT omrvar,INPUT KALKHUV.KLOGID,INPUT KALKHUV.TYPKALK,INPUT aonrvar,INPUT delnrvar, INPUT kbename).
   RUN copyrel_UI IN LocalAppServerHandle.
   FIND FIRST BEREDNING WHERE BEREDNING.AONR = aonrvar AND BEREDNING.DELNR = delnrvar NO-LOCK NO-ERROR. 
   FIND FIRST BERKALKOPPLA WHERE BERKALKOPPLA.BERNR =  BEREDNING.BERNR  AND BERKALKOPPLA.OMRADE = BEREDNING.OMRADE NO-LOCK NO-ERROR.
   IF togkonval = FALSE THEN DO:
      FOR EACH KALKNUM WHERE KALKNUM.KALKNR = BERKALKOPPLA.KALKNR AND KALKNUM.OMRADE = BERKALKOPPLA.OMRADE NO-LOCK:   
         RUN kbufcreate_UI (INPUT omrvar, INPUT KalkNrvar, INPUT togsum).
      END.  
   END.
   ELSE DO:
      FOR EACH kon_val: 
         FOR EACH KALKNUM WHERE KALKNUM.KALKNR = BERKALKOPPLA.KALKNR AND KALKNUM.OMRADE = BERKALKOPPLA.OMRADE AND  KALKNUM.BERNUM = kon_val.num NO-LOCK.
            RUN kbufcreate_UI (INPUT omrvar, INPUT KalkNrvar, INPUT togsum).
         END.
      END.
      FOR EACH skrivhdschakttemp WHERE NO-LOCK:
         FOR EACH KALKNUM WHERE KALKNUM.KALKNR = BERKALKOPPLA.KALKNR AND KALKNUM.OMRADE = BERKALKOPPLA.OMRADE AND  KALKNUM.SID = skrivhdschakttemp.SID NO-LOCK.
            RUN kbufcreate_UI (INPUT omrvar, INPUT KalkNrvar, INPUT togsum).
         END.
      END.   
   END.      
   EMPTY TEMP-TABLE kalktmtrlTT NO-ERROR. 
   IF togmtrl = TRUE THEN DO: 
      IF togkonval = FALSE THEN DO:
         FOR EACH BERVAL WHERE BERVAL.AONR = STRING(BEREDNING.BERNR) AND BERVAL.OMRADE = BEREDNING.OMRADE AND BERVAL.KSKAP = FALSE NO-LOCK: 
            RUN kbufmtrlcreate_UI (INPUT omrvar, INPUT KalkNrvar).
         END.   
      END.
      ELSE DO:
         FOR EACH kon_val: 
            FOR EACH BERVAL WHERE BERVAL.AONR = STRING(BEREDNING.BERNR) AND BERVAL.OMRADE = BEREDNING.OMRADE AND BERVAL.KSKAP = FALSE AND BERVAL.NUM = kon_val.num NO-LOCK: 
               RUN kbufmtrlcreate_UI (INPUT omrvar, INPUT KalkNrvar).
            END.
         END.
      END.   
      
      FOR EACH kalktmtrlTT WHERE kalktmtrlTT.BERKVANT > 0 NO-LOCK:
         DO TRANSACTION:
            CREATE KALKMTRL.
            BUFFER-COPY kalktmtrlTT TO KALKMTRL.
         END.   
      END. 
      RELEASE KALKMTRL NO-ERROR.  
   END.
   EMPTY TEMP-TABLE kalktmtrlTT NO-ERROR.
   
   IF VALID-HANDLE(LocalAppServerHandle) THEN DO:
      RUN avsluta_UI IN LocalAppServerHandle.
      DELETE PROCEDURE LocalAppServerHandle.  
   END.   
END PROCEDURE.

PROCEDURE BerKalkHDSkapa_UI:
   DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER togsum AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER togkonval AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE antlavar AS DECIMAL NO-UNDO.
   FIND FIRST BEREDNING WHERE BEREDNING.AONR = aonrvar AND BEREDNING.DELNR = delnrvar NO-LOCK NO-ERROR. 
   IF togkonval = FALSE THEN DO:
      FOR EACH HDKALK WHERE HDKALK.BERNR = BEREDNING.BERNR AND HDKALK.OMRADE = BEREDNING.OMRADE NO-LOCK:   
         antlavar = 0.
         IF HDKALK.ARBKOD = "egen" THEN DO:
            RUN egen_UI (INPUT KalkNrvar, INPUT omrvar, INPUT HDKALK.ARBKOD, INPUT HDKALK.LOPNR).
         END.
         ELSE DO:   
            IF togsum = TRUE THEN DO:
               FIND FIRST KalkylimportTT WHERE KalkylimportTT.ARBKOD = HDKALK.ARBKOD AND KalkylimportTT.LOPNR = HDKALK.LOPNR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE KalkylimportTT THEN DO:
                  CREATE KalkylimportTT.
               END. 
               antlavar =  KalkylimportTT.ANTAL.          
            END.
            ELSE CREATE KalkylimportTT.
            
            ASSIGN
            KalkylimportTT.TTRECID = RECID(KalkylimportTT) 
            KalkylimportTT.KALKNR = KalkNrvar
            KalkylimportTT.OMRADE = omrvar
            KalkylimportTT.MATRIS = 1
            KalkylimportTT.ARBKOD = HDKALK.ARBKOD 
            KalkylimportTT.LOPNR = HDKALK.LOPNR.
            KalkylimportTT.ANTAL = antlavar + HDKALK.ANTAL.
         END.            
      END.
      FOR EACH BERKALK WHERE BERKALK.AONR = STRING(BEREDNING.BERNR) AND BERKALK.OMRADE = BEREDNING.OMRADE USE-INDEX OMR NO-LOCK.
         antlavar = 0.
         IF BERKALK.ARBKOD = "egen" THEN DO:
            RUN egen_UI (INPUT KalkNrvar, INPUT omrvar, INPUT BERKALK.ARBKOD, INPUT BERKALK.LOPNR).
         END.
         ELSE DO: 
            IF togsum = TRUE THEN DO:
               FIND FIRST KalkylimportTT WHERE KalkylimportTT.ARBKOD = BERKALK.ARBKOD AND KalkylimportTT.LOPNR = BERKALK.LOPNR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE KalkylimportTT THEN DO:
                  CREATE KalkylimportTT.
               END. 
               antlavar =  KalkylimportTT.ANTAL.          
            END.
            ELSE CREATE KalkylimportTT.
            ASSIGN 
            KalkylimportTT.TTRECID = RECID(KalkylimportTT)
            KalkylimportTT.KALKNR = KalkNrvar
            KalkylimportTT.OMRADE = omrvar
            KalkylimportTT.MATRIS = 1
            KalkylimportTT.ARBKOD = BERKALK.ARBKOD 
            KalkylimportTT.LOPNR = BERKALK.LOPNR
            KalkylimportTT.ANTAL = antlavar + BERKALK.ANTAL.
         END.   
      END.
   END.
   ELSE DO:
      FOR EACH skrivhdschakttemp WHERE NO-LOCK:
         FOR EACH HDKALK WHERE HDKALK.BERNR = BEREDNING.BERNR AND HDKALK.OMRADE = BEREDNING.OMRADE AND  HDKALK.SID = skrivhdschakttemp.SID NO-LOCK:   
            antlavar = 0.
            IF HDKALK.ARBKOD = "egen" THEN DO:
               RUN egen_UI (INPUT KalkNrvar, INPUT omrvar, INPUT HDKALK.ARBKOD, INPUT HDKALK.LOPNR).
            END.
            ELSE DO:   
               IF togsum = TRUE THEN DO:
                  FIND FIRST KalkylimportTT WHERE KalkylimportTT.ARBKOD = HDKALK.ARBKOD AND KalkylimportTT.LOPNR = HDKALK.LOPNR NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE KalkylimportTT THEN DO:
                     CREATE KalkylimportTT.
                  END. 
                  antlavar =  KalkylimportTT.ANTAL.          
               END.
               ELSE CREATE KalkylimportTT.
               
               ASSIGN
               KalkylimportTT.TTRECID = RECID(KalkylimportTT) 
               KalkylimportTT.KALKNR = KalkNrvar
               KalkylimportTT.OMRADE = omrvar
               KalkylimportTT.MATRIS = 1
               KalkylimportTT.ARBKOD = HDKALK.ARBKOD 
               KalkylimportTT.LOPNR = HDKALK.LOPNR.
               KalkylimportTT.ANTAL = antlavar + HDKALK.ANTAL.
            END.            
         END.
      END.   
      FOR EACH kon_val: 
         FOR EACH BERKALK WHERE BERKALK.AONR = STRING(BEREDNING.BERNR) AND BERKALK.OMRADE = BEREDNING.OMRADE AND  BERKALK.NUM = kon_val.num USE-INDEX OMR NO-LOCK.
            antlavar = 0.
            IF BERKALK.ARBKOD = "egen" THEN DO:
               RUN egen_UI (INPUT KalkNrvar, INPUT omrvar, INPUT BERKALK.ARBKOD, INPUT BERKALK.LOPNR).
            END.
            ELSE DO: 
               IF togsum = TRUE THEN DO:
                  FIND FIRST KalkylimportTT WHERE KalkylimportTT.ARBKOD = BERKALK.ARBKOD AND KalkylimportTT.LOPNR = BERKALK.LOPNR NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE KalkylimportTT THEN DO:
                     CREATE KalkylimportTT.
                  END. 
                  antlavar =  KalkylimportTT.ANTAL.          
               END.
               ELSE CREATE KalkylimportTT.
               ASSIGN 
               KalkylimportTT.TTRECID = RECID(KalkylimportTT)
               KalkylimportTT.KALKNR = KalkNrvar
               KalkylimportTT.OMRADE = omrvar
               KalkylimportTT.MATRIS = 1
               KalkylimportTT.ARBKOD = BERKALK.ARBKOD 
               KalkylimportTT.LOPNR = BERKALK.LOPNR
               KalkylimportTT.ANTAL = antlavar + BERKALK.ANTAL.
            END.   
         END.
      END.   
   END.    
END PROCEDURE.

PROCEDURE egen_UI :
   
   DEFINE INPUT  PARAMETER KalkNrvar AS INTEGER  NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER egenkod AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER lopnrkod AS INTEGER  NO-UNDO.
   DEFINE VARIABLE lopnrkodorg AS INTEGER NO-UNDO.
   DEFINE VARIABLE benvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
   lopnrkodorg = lopnrkod.
   /*Anders Olsson Elpool i Umeå AB  14 jan 2016 11:16:10 
   bEHÖVS INTE DÅ SCHAKT OCH BERKALK EGEN INTE FÅR SAMMA LOPNR 
   */
   /*
   FIND FIRST KALKNUM WHERE KALKNUM.KALKNR = KalkNrvar AND KALKNUM.OMRADE = omrvar AND KALKNUM.ARBKOD = egenkod AND KALKNUM.LOPNR = lopnrkod NO-LOCK NO-ERROR.
   IF AVAILABLE KALKNUM THEN DO:
      FIND LAST KALKNUM WHERE KALKNUM.KALKNR = KalkNrvar AND KALKNUM.OMRADE = omrvar AND KALKNUM.ARBKOD = egenkod AND KALKNUM.LOPNR >= lopnrkod NO-LOCK NO-ERROR.
      IF AVAILABLE KALKNUM THEN lopnrkod =  KALKNUM.LOPNR + 1.
      ELSE lopnrkod = lopnrkod + 1.
   END.*/
   DO TRANSACTION:
      CREATE KALKNUM.
      IF AVAILABLE HDKALK THEN DO:
          BUFFER-COPY HDKALK TO KALKNUM. 
      END. 
      IF AVAILABLE BERKALK THEN DO: 
         BUFFER-COPY BERKALK EXCEPT NUM TO KALKNUM .  
      END. 
      benvar   = KALKNUM.BENAMNING.
      KALKNUM.BENAMNING = "".
     
      ASSIGN
      KALKNUM.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID
      KALKNUM.BENAMNING = SUBSTRING(benvar,1,40)
      KALKNUM.ANMARK = SUBSTRING(benvar,50)           
      KALKNUM.KALKNR = KalkNrvar
      KALKNUM.OMRADE = omrvar
      KALKNUM.MATRIS = 1  
      KALKNUM.TYPKALK = 2.
      
      RUN sistanum_UI (OUTPUT numvar).
      KALKNUM.NUM = numvar.
   END.
   RELEASE KALKNUM NO-ERROR.    
   RUN egensub_UI (INPUT 0,INPUT KalkNrvar, INPUT omrvar, INPUT egenkod, INPUT lopnrkod,INPUT numvar).
END PROCEDURE. 
PROCEDURE egensub_UI :  
   /*totkostnad för koden beräknas i kalkyldb 2376       METHOD PUBLIC VOID RaknaEgen()*/
   DEFINE INPUT  PARAMETER BernrVar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER KalkNrvar AS INTEGER  NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER egenkod AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER lopnrkod AS INTEGER  NO-UNDO.
   DEFINE INPUT  PARAMETER numvar AS INTEGER NO-UNDO.
   IF NOT AVAILABLE KALKYLKATALOG THEN DO:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkNrvar AND KALKHUV.OMRADE = omrvar NO-LOCK NO-ERROR.
      FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE KALKNUM THEN DO:
      FIND FIRST KALKNUM WHERE KALKNUM.KALKNR = KalkNrvar AND KALKNUM.OMRADE = omrvar AND KALKNUM.NUM = numvar NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE BEREDNING THEN DO:
      FIND FIRST BEREDNING WHERE BEREDNING.BERNR = BernrVar AND BEREDNING.OMRADE = OmrVar NO-LOCK NO-ERROR.
   END.
   FIND FIRST KALKBEFB WHERE KALKBEFB.BERNR = BEREDNING.BERNR AND KALKBEFB.OMRADE =  BEREDNING.OMRADE AND KALKBEFB.ARBKOD = "EGEN" AND 
   KALKBEFB.LOPNR = lopnrkod NO-LOCK NO-ERROR.
   IF AVAILABLE KALKBEFB THEN DO:
      FOR EACH KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID AND KALKYLPRISER.EGENKODUPP = TRUE NO-LOCK:
         CREATE KALKNUMSUB.               
         BUFFER-COPY KALKYLPRISER TO KALKNUMSUB.
         ASSIGN 
         KALKNUMSUB.KALKNR = KALKNUM.KALKNR
         KALKNUMSUB.OMRADE = KALKNUM.OMRADE
         KALKNUMSUB.NUM = KALKNUM.NUM.
         RUN sistanumsubid_UI (OUTPUT KALKNUMSUB.NUMSUBID).
         IF KALKYLPRISER.SOKBENAMNING = "BEREDARE" THEN DO:
            ASSIGN
            KALKNUMSUB.TIMMAR = KALKBEFB.F1
            KALKNUMSUB.PRIS = KALKBEFB.PRIS1.
         END. 
         ELSE  IF KALKYLPRISER.SOKBENAMNING = "BEREDARE REGION" THEN DO:
            ASSIGN
            KALKNUMSUB.TIMMAR = KALKBEFB.F1
            KALKNUMSUB.PRIS = KALKBEFB.PRIS1.
         END.   
         ELSE IF KALKYLPRISER.SOKBENAMNING = "MONTÖR" THEN DO:
            ASSIGN
            KALKNUMSUB.TIMMAR = KALKBEFB.F2
            KALKNUMSUB.PRIS = KALKBEFB.PRIS2.
         END.   
         ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN1" THEN DO:
            ASSIGN
            KALKNUMSUB.TIMMAR = KALKBEFB.F3
            KALKNUMSUB.PRIS = KALKBEFB.PRIS3.
         END.   
         ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN2" THEN DO:
            ASSIGN
            KALKNUMSUB.TIMMAR = KALKBEFB.F4
            KALKNUMSUB.PRIS = KALKBEFB.PRIS4.
         END.    
         ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN3" THEN DO:
            ASSIGN
            KALKNUMSUB.TIMMAR = KALKBEFB.F7
            KALKNUMSUB.PRIS = KALKBEFB.PRIS7.
         END.   
         ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN4" THEN DO:
            ASSIGN
            KALKNUMSUB.TIMMAR = KALKBEFB.F5
            KALKNUMSUB.PRIS = KALKBEFB.PRIS5.
         END.   
         ELSE IF KALKYLPRISER.SOKBENAMNING = "MASKIN5" THEN DO:
            ASSIGN
            KALKNUMSUB.TIMMAR = KALKBEFB.F6
            KALKNUMSUB.PRIS = KALKBEFB.PRIS6.
         END.   
         ELSE IF KALKYLPRISER.SOKBENAMNING = "MATERIEL" THEN DO: 
            ASSIGN
            KALKNUMSUB.KOSTNAD  = KALKBEFB.MATERIEL.                  
         END.   
         ELSE IF KALKYLPRISER.SOKBENAMNING = "ÖVRIGKOSTNAD" THEN DO:
            ASSIGN
            KALKNUMSUB.KOSTNAD = KALKBEFB.OVRIGT.             
         END. 
         ELSE IF KALKYLPRISER.SOKBENAMNING = "ENTREP" THEN DO:
            ASSIGN
            KALKNUMSUB.KOSTNAD = KALKBEFB.ENTRP.               
         END. 
         ELSE IF KALKYLPRISER.SOKBENAMNING = "UTRUSTNING" THEN DO:
             ASSIGN
             KALKNUMSUB.KOSTNAD = KALKBEFB.UTRUSTKOST.   
             /*KALKBEFB.UTRUST ???*/            
         END.
         IF KALKNUMSUB.KOSTNAD = 0 AND KALKNUMSUB.TIMMAR = 0 /*AND KALKNUMSUB.PRIS = 0*/  THEN DO:
            DELETE KALKNUMSUB.
         END.   
         ELSE DO:
            ASSIGN 
            KALKNUMSUB.FRIBENAMNING = KALKNUMSUB.BENAMNING         
            KALKNUMSUB.FRIKOSTNAD   = KALKNUMSUB.KOSTNAD
            KALKNUMSUB.FRIPRIS      = KALKNUMSUB.PRIS
            KALKNUMSUB.FRITIMMAR    = KALKNUMSUB.TIMMAR.
             
         END.  
      END.                  
   END.
   RELEASE KALKNUMSUB NO-ERROR.
   RELEASE KALKNUM NO-ERROR.    
END PROCEDURE.
PROCEDURE sistanum_UI :
   DEFINE OUTPUT PARAMETER hjraknare AS INTEGER NO-UNDO.

   FIND LAST KALKNUMbuffsista WHERE KALKNUMbuffsista.KALKNR = KALKNUM.KALKNR AND KALKNUMbuffsista.OMRADE = KALKNUM.OMRADE USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE KALKNUMbuffsista THEN 
   DO:
      hjraknare = KALKNUMbuffsista.NUM + 1.   
   END.  
   ELSE hjraknare = hjraknare + 1.
   RELEASE KALKNUMbuffsista NO-ERROR.
END PROCEDURE.
PROCEDURE sistanumsubid_UI :
   DEFINE OUTPUT PARAMETER hjraknare AS INTEGER NO-UNDO.
   FIND LAST KALKNUMSUBbuffsista WHERE KALKNUMSUBbuffsista.KALKNR = KALKNUM.KALKNR AND 
   KALKNUMSUBbuffsista.OMRADE = KALKNUM.OMRADE AND KALKNUMSUBbuffsista.NUM = KALKNUM.NUM 
   USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE KALKNUMSUBbuffsista THEN DO:
      hjraknare = KALKNUMSUBbuffsista.NUMSUBID + 1.   
   END.  
   ELSE hjraknare = hjraknare + 1.
   RELEASE KALKNUMSUBbuffsista NO-ERROR.
END PROCEDURE. 

/*============KONTROLL AV UPPLÄGGET FÖR BERKALKYL===========*/

PROCEDURE berkalkuppkoll_UI :   
   DEFINE OUTPUT PARAMETER TABLE FOR kbetemp.
   DEFINE VARIABLE delkatnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE kbetemp NO-ERROR. 
   DEBUGGER:SET-BREAK().
   FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   OPEN QUERY kq FOR EACH KALKBER /*WHERE KALKBER.KTYPKOD = KONSTRUKTION.KTYPKOD*/
   NO-LOCK BY KALKBER.KTYPKOD BY KALKBER.F5 BY KALKBER.F4 BY KALKBER.F3 BY KALKBER.F2 BY KALKBER.F1 BY KALKBER.ARBKOD BY KALKBER.LOPNR.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(KALKBER):
      finnskkod = FALSE.
      delkatnr = ?.
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID NO-LOCK:
         FIND FIRST KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLLOPPOSTER.ARBKOD = KALKBER.ARBKOD AND
         KALKYLLOPPOSTER.LOPNR = KALKBER.LOPNR NO-LOCK NO-ERROR.                  
         IF AVAILABLE KALKYLLOPPOSTER THEN DO:
             finnskkod = TRUE.
             delkatnr = KALKYLLOPPOSTER.KLOGSUBID.             
          END.            
      END.
      IF finnskkod = TRUE THEN DO:
         finnskkod = FALSE.
         
         FIND FIRST KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = delkatnr AND KALKYLLOPPOSTER.ARBKOD = KALKBER.ARBKOD AND
         KALKYLLOPPOSTER.LOPNR = KALKBER.LOPNR NO-LOCK NO-ERROR.
         IF AVAILABLE KALKYLLOPPOSTER  THEN DO:
            DO TRANSACTION:
               FIND CURRENT KALKBER EXCLUSIVE-LOCK NO-ERROR.  
               IF KALKBER.BENAMNING =  KALKYLLOPPOSTER.BENAMNING THEN.
               ELSE DO:
                  ASSIGN KALKBER.BENAMNING =  KALKYLLOPPOSTER.BENAMNING.
               END.
             END.           
         END.                           
      END.            
      ELSE DO:
         CREATE kbetemp.
         ASSIGN    
         kbetemp.KTYPKOD = KALKBER.KTYPKOD
         kbetemp.ARBKOD = KALKBER.ARBKOD
         kbetemp.LOPNR = KALKBER.LOPNR
         kbetemp.BENAMNING = SUBSTRING(KALKBER.BENAMNING,1,30)
         kbetemp.ENHET = SUBSTRING(KALKBER.ENHET,1,3)
         kbetemp.ANTAL = KALKBER.ANTAL
         kbetemp.F1 = KALKBER.F1
         kbetemp.F2 = KALKBER.F2
         kbetemp.F3 = KALKBER.F3
         kbetemp.F4 = KALKBER.F4
         kbetemp.F5 = KALKBER.F5           
         kbetemp.KATAR = KALKBER.KATAR.
         
      END.
   
      GET NEXT kq NO-LOCK.
   END.
   CLOSE QUERY kq.
   
   FOR EACH HDKKOPP  NO-LOCK:         
      IF HDKKOPP.TYP = "F" OR HDKKOPP.TYP = "Y" OR HDKKOPP.TYP = "FH" OR HDKKOPP.TYP = "PH" THEN DO:
         FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID NO-LOCK:
            FIND FIRST KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLLOPPOSTER.ARBKOD = HDKKOPP.ARBKOD AND
            KALKYLLOPPOSTER.LOPNR = HDKKOPP.LOPNR NO-LOCK NO-ERROR.                  
            IF AVAILABLE KALKYLLOPPOSTER THEN finnskkod = TRUE.         
         END.
         IF finnskkod = TRUE THEN finnskkod = FALSE.        
         ELSE DO:
            
            CREATE kbetemp.
            ASSIGN                   
            kbetemp.ARBKOD = HDKKOPP.ARBKOD
            kbetemp.LOPNR = HDKKOPP.LOPNR
            kbetemp.BENAMNING = HDKKOPP.TYP               
            kbetemp.ANTAL = HDKKOPP.ANTAL
            kbetemp.KATAR = HDKKOPP.KATAR.
            FIND LAST KALKYLLOPPOSTER WHERE  KALKYLLOPPOSTER.ARBKOD = HDKKOPP.ARBKOD AND
            KALKYLLOPPOSTER.LOPNR = HDKKOPP.LOPNR USE-INDEX LOPNR NO-LOCK NO-ERROR.
            IF AVAILABLE KALKYLLOPPOSTER THEN DO:
               kbetemp.BENAMNING = KALKYLLOPPOSTER.BENAMNING + " " + HDKKOPP.TYP.
            END.   
            IF HDKKOPP.TYP = "F" THEN DO:
               FIND FIRST FORLAGG WHERE FORLAGG.ID = HDKKOPP.ID NO-LOCK NO-ERROR.
               IF AVAILABLE FORLAGG THEN DO:
                  kbetemp.KTYPKOD = FORLAGG.BENAMNING. 
                  /*kbetemp.KTYPKOD = "FÖRLÄGGNING " + FORLAGG.BENAMNING. */                 
               END.
            END.
            ELSE IF HDKKOPP.TYP = "Y" THEN DO:
               FIND FIRST YTBELAGG WHERE YTBELAGG.ID = HDKKOPP.ID NO-LOCK NO-ERROR.
               IF AVAILABLE YTBELAGG THEN DO:
                  kbetemp.KTYPKOD =  YTBELAGG.YTBELAGG.
                  /*kbetemp.KTYPKOD = "YTBELÄGGNING " + YTBELAGG.YTBELAGG.*/                  
               END.
            END.
            ELSE IF HDKKOPP.TYP = "FH"  THEN DO:
               FIND FIRST HDHANDELSE WHERE HDHANDELSE.ID = HDKKOPP.ID NO-LOCK NO-ERROR.
               IF AVAILABLE HDHANDELSE THEN DO:
                  kbetemp.KTYPKOD =  HDHANDELSE.BENAMNING.
                  /*kbetemp.KTYPKOD = "FÖRLÄGGNINGSHÄNDELSE " + HDHANDELSE.BENAMNING.*/                  
               END.
            END.
            ELSE IF HDKKOPP.TYP = "PH" THEN DO:
               FIND FIRST HDHANDELSE WHERE HDHANDELSE.ID = HDKKOPP.ID NO-LOCK NO-ERROR.
               IF AVAILABLE HDHANDELSE THEN DO:
                  kbetemp.KTYPKOD =  HDHANDELSE.BENAMNING.
                  /*kbetemp.KTYPKOD = "PUNKTHÄNDELSE " + HDHANDELSE.BENAMNING.*/                  
               END.
            END.
            
            /*kbetemp.F1 = KALKBER.F1
            kbetemp.F2 = KALKBER.F2
            kbetemp.F3 = KALKBER.F3
            kbetemp.F4 = KALKBER.F4
            kbetemp.F5 = KALKBER.F5           
            */
         END.
      END.   
   END.
            
END PROCEDURE.
/*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
        */
/*FINNS KALKYL-BEREDNING SUNDSVALL*/
PROCEDURE BeredningMtrlfinns :
   DEFINE INPUT  PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER bloblog AS LOGICAL NO-UNDO.
   RUN procsetkop_UI.
   CREATE inextrakopptemp.          
   ASSIGN
   inextrakopptemp.PROGRAM = "KALKBER"                   
   inextrakopptemp.KOPPLACHAR1 = omrvar
   inextrakopptemp.KOPPLAINT1 = KalkNrvar
   inextrakopptemp.KOPPLACHAR2 = ?
   inextrakopptemp.KOPPLAINT2 =  ?.
   RUN finnsextra_UI IN fbestapph (INPUT TABLE inextrakopptemp, OUTPUT bloblog).
   RUN procresetkop_UI.  
   
END PROCEDURE.
/*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
        */
/*KOPPLING KALKYL-BEREDNING SUNDSVALL*/
PROCEDURE BeredningMtrl :
   DEFINE INPUT  PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER nyber AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER beromr AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER bernr AS INTEGER NO-UNDO.
   DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
   RUN BeredningMtrlfinns(INPUT KalkNrvar, INPUT omrvar, OUTPUT bloblog).
   RUN procsetkop_UI.
   IF bloblog = TRUE THEN DO:
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "KALKBER"                   
      inextrakopptemp.KOPPLACHAR1 = omrvar
      inextrakopptemp.KOPPLAINT1 = KalkNrvar
      inextrakopptemp.KOPPLACHAR2 = ?
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp, OUTPUT TABLE extrakopptemp).
      FIND FIRST extrakopptemp WHERE NO-LOCK NO-ERROR.
      ASSIGN 
      beromr = extrakopptemp.KOPPLACHAR2
      bernr = extrakopptemp.KOPPLAINT2
      nyber = FALSE. 
   END.   
   ELSE DO:
      ASSIGN 
      beromr = ?
      bernr = ?
      nyber = TRUE.  
   END.  
   RUN procresetkop_UI. 
   
END PROCEDURE.
/*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
        */

PROCEDURE BeredningMtrlHmt :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER totbermtrl AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR berkalkmtrltt.
   EMPTY TEMP-TABLE berkalkmtrltt NO-ERROR. 
   RUN procsetkop_UI.
   /*KALKYL-BEREDNING SUNDSVALL*/
   CREATE inextrakopptemp.          
   ASSIGN
   inextrakopptemp.PROGRAM = "KALKBER"                   
   inextrakopptemp.KOPPLACHAR1 = omrvar
   inextrakopptemp.KOPPLAINT1 = KalkNrvar
   inextrakopptemp.KOPPLACHAR2 = ?
   inextrakopptemp.KOPPLAINT2 =  ?.
   RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp, OUTPUT TABLE extrakopptemp).
   FIND FIRST extrakopptemp WHERE NO-LOCK NO-ERROR.
   FIND FIRST BEREDNING WHERE BEREDNING.OMRADE = extrakopptemp.KOPPLACHAR2 AND  BEREDNING.BERNR = extrakopptemp.KOPPLAINT2 NO-LOCK NO-ERROR.
   IF AVAILABLE BEREDNING THEN DO:
      RUN LISTPROG.P (INPUT BEREDNING.BERAONR, INPUT BEREDNING.OMRADE, OUTPUT TABLE mtrl_temp, 
                      OUTPUT TABLE lin_upp, OUTPUT TABLE lin_temp).   
   END.
   FIND FIRST mtrl_temp NO-LOCK NO-ERROR.
   IF AVAILABLE mtrl_temp THEN DO:           
      FOR EACH mtrl_temp BREAK BY mtrl_temp.ENR: 
         ACCUMULATE mtrl_temp.TOTPRIS (TOTAL BY mtrl_temp.ENR). 
         ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.ENR).       
         IF LAST-OF(mtrl_temp.ENR) THEN DO:
            CREATE berkalkmtrltt.
            ASSIGN 
            berkalkmtrltt.ENR = mtrl_temp.ENR
            berkalkmtrltt.BENAMNING = mtrl_temp.BENAMNING 
            berkalkmtrltt.ENHET = mtrl_temp.ENHET 
            berkalkmtrltt.PRIS = mtrl_temp.PRIS
            berkalkmtrltt.TOTPRIS = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.TOTPRIS)                       
            berkalkmtrltt.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL). 
            totbermtrl = totbermtrl + berkalkmtrltt.TOTPRIS.                                                   
         END.     
      END.    
      FOR EACH lin_upp:
         FIND FIRST berkalkmtrltt WHERE berkalkmtrltt.ENR = lin_upp.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE berkalkmtrltt THEN DO:                      
            ASSIGN
            berkalkmtrltt.ANTAL = berkalkmtrltt.ANTAL + lin_upp.TOTMETER
            berkalkmtrltt.TOTPRIS = berkalkmtrltt.TOTPRIS + lin_upp.TOTPRIS.
            totbermtrl = totbermtrl + lin_upp.TOTPRIS.
         END.
         ELSE DO:                    
            CREATE berkalkmtrltt.
            ASSIGN 
            berkalkmtrltt.ENR = lin_upp.ENR
            berkalkmtrltt.BENAMNING = lin_upp.BENAMNING 
            berkalkmtrltt.ENHET = lin_upp.ENHET 
            berkalkmtrltt.PRIS = lin_upp.PRIS
            berkalkmtrltt.TOTPRIS = lin_upp.TOTPRIS                       
            berkalkmtrltt.ANTAL = lin_upp.TOTMETER.
            totbermtrl = totbermtrl + lin_upp.TOTPRIS.
         END.
      END.   
   END.
   RUN procresetkop_UI.
END PROCEDURE.


PROCEDURE BeredningMtrlHmtUppf :
   DEFINE INPUT  PARAMETER kalkvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrade AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR kalktmtrlTT.
   FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = kalkvar AND KALKAONR.OMRADE = omrade NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKAONR THEN RETURN.
   IF KALKAONR.AONR = ? THEN RETURN.
   
   
   
   FIND FIRST BEREDNING WHERE BEREDNING.AONR = KALKAONR.AONR AND  BEREDNING.DELNR = KALKAONR.DELNR NO-LOCK NO-ERROR.
   IF AVAILABLE BEREDNING THEN DO:
      RUN LISTPROG.P (INPUT BEREDNING.BERAONR, INPUT BEREDNING.OMRADE, OUTPUT TABLE mtrl_temp, 
                      OUTPUT TABLE lin_upp, OUTPUT TABLE lin_temp).   
   END.
   FIND FIRST mtrl_temp NO-LOCK NO-ERROR.
   IF AVAILABLE mtrl_temp THEN DO:           
      FOR EACH mtrl_temp BREAK BY mtrl_temp.ENR: 
         ACCUMULATE mtrl_temp.TOTPRIS (TOTAL BY mtrl_temp.ENR). 
         ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.ENR).       
         IF LAST-OF(mtrl_temp.ENR) THEN DO:
            CREATE kalktmtrlTT.
            ASSIGN 
            kalktmtrlTT.KALKNR = kalkvar
            kalktmtrlTT.OMRADE = omrade
            kalktmtrlTT.ENR = mtrl_temp.ENR
            kalktmtrlTT.BENAMNING = mtrl_temp.BENAMNING 
            kalktmtrlTT.ENHET = mtrl_temp.ENHET 
            kalktmtrlTT.NPRIS = mtrl_temp.PRIS
            kalktmtrlTT.SUMMA = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.TOTPRIS)                       
            kalktmtrlTT.BERKVANT = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL).
            kalktmtrlTT.TTRECID = RECID(kalktmtrlTT). 
         END.     
      END.    
   END.   
END PROCEDURE.


/*============Visa BERKALKYL===========*/
/*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:54:51 
 visar rätt kalkyl från beredningen
*/

PROCEDURE BerKalkVisa_UI:
   DEFINE INPUT  PARAMETER bernr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER beromr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER benvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER mtrlsekvar6 AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidutorg.
   EMPTY TEMP-TABLE tidutorg NO-ERROR. 
   DEFINE VARIABLE slutsumrec AS RECID NO-UNDO.
   CREATE tidutorg.                  
   ASSIGN                                                               
   SUBSTRING(tidutorg.UT,1) = "KALKYL"        
   SUBSTRING(tidutorg.UT,55) = STRING(TODAY)
   SUBSTRING(tidutorg.UT,65) = STRING(TIME,"HH:MM:SS").                                                               
   CREATE tidutorg.              
   CREATE tidutorg.  
   ASSIGN                                                               
   SUBSTRING(tidutorg.UT,1) = CAPS(Guru.Konstanter:gaonamnk) + ": " + benvar.
   CREATE tidutorg.  
   ASSIGN                                                               
   SUBSTRING(tidutorg.UT,1) = "BEREDNING NR : " + bernr.
   CREATE tidutorg.  
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = beromr USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE OMRADETAB THEN DO:
      ASSIGN                                                               
      SUBSTRING(tidutorg.UT,1) = CAPS(Guru.Konstanter:gomrk) + "       : " + OMRADETAB.NAMN.                
   END.         
   /*                           
   CREATE tidutorg.
   ASSIGN
   SUBSTRING(tidutorg.UT,1) = "SUMMA KALKYL : "
   slutsumrec = RECID(tidutorg).
   */
   CREATE tidutorg.
   ASSIGN 
   SUBSTRING(tidutorg.UT,1) = "P2-KOD"
   SUBSTRING(tidutorg.UT,10) = "BENÄMNING"
   SUBSTRING(tidutorg.UT,68) = "ANTAL".
   SUBSTRING(tidutorg.UT,78) = "ENHET".
   CREATE tidutorg.
   ASSIGN                                                                     
   SUBSTRING(tidutorg.UT,1) = "======.==.=========================================================.=========.=====". 
   FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   FOR EACH BERKALK WHERE BERKALK.AONR = bernr AND BERKALK.OMRADE = beromr NO-LOCK BY BERKALK.ARBKOD BY BERKALK.LOPNR:
      /*
      IF BERKALK.ARBKOD = "EGEN" THEN DO:
         NEXT.
      END.
      ELSE DO:
         FIND FIRST KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID AND KALKYLLOPPOSTER.ARBKOD = BERKALK.ARBKOD AND
         KALKYLLOPPOSTER.LOPNR = BERKALK.LOPNR NO-LOCK NO-ERROR.
         CREATE tidutorg.
         ASSIGN 
         SUBSTRING(tidutorg.UT,1) = BERKALK.ARBKOD
         SUBSTRING(tidutorg.UT,7) = STRING(BERKALK.LOPNR).
         IF AVAILABLE KALKYLLOPPOSTER THEN DO:
            SUBSTRING(tidutorg.UT,10) = KALKYLLOPPOSTER.BENAMNING.
         END.   
         ELSE DO:                           
            SUBSTRING(tidutorg.UT,10) = "Koden saknas i den senaste katalogen!".
         END.   
         SUBSTRING(tidutorg.UT,68) = STRING(BERKALK.ANTAL).
         IF AVAILABLE KALKYLLOPPOSTER THEN DO:
            SUBSTRING(tidutorg.UT,78) = KALKYLLOPPOSTER.ENHET.
         END.   
         ELSE DO:
            SUBSTRING(tidutorg.UT,78) = BERKALK.ENHET.
         END.
               
      END.
      */
      IF BERKALK.ARBKOD = "EGEN" THEN DO:
         NEXT.
      END.
      ELSE DO:
         CREATE tidutorg.
         ASSIGN 
         SUBSTRING(tidutorg.UT,1) = BERKALK.ARBKOD
         SUBSTRING(tidutorg.UT,7) = STRING(BERKALK.LOPNR)
         SUBSTRING(tidutorg.UT,10) = BERKALK.BENAMNING
         SUBSTRING(tidutorg.UT,68) = STRING(BERKALK.ANTAL)
         SUBSTRING(tidutorg.UT,78) = BERKALK.ENHET.
                       
      END.
   END.   
   FOR EACH BERKALK WHERE BERKALK.AONR = bernr AND BERKALK.OMRADE = beromr AND BERKALK.ARBKOD = "EGEN" NO-LOCK BY BERKALK.ARBKOD BY BERKALK.LOPNR:
      CREATE tidutorg.
      ASSIGN 
      SUBSTRING(tidutorg.UT,1) = BERKALK.ARBKOD
      SUBSTRING(tidutorg.UT,7) = STRING(BERKALK.LOPNR)
      SUBSTRING(tidutorg.UT,10) = BERKALK.BENAMNING
      SUBSTRING(tidutorg.UT,68) = STRING(BERKALK.ANTAL)
      SUBSTRING(tidutorg.UT,78) = BERKALK.ENHET.        
   END.
   
   
END PROCEDURE.

DEFINE TEMP-TABLE ber_temp2  NO-UNDO
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER    
   FIELD ANTAL AS DECIMAL
   FIELD NUM AS INTEGER
   INDEX KOD ARBKOD LOPNR ASCENDING
   INDEX NUM NUM ARBKOD LOPNR ASCENDING. 
/*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:54:51 
 visar rätt kalkyl från beredningen KÖRS INTE
*/
PROCEDURE ByggBerKalkVisa_UI:
   DEFINE INPUT  PARAMETER bernr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER beromr AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR ber_temp2.
   EMPTY TEMP-TABLE tidutorg NO-ERROR. 
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = beromr USE-INDEX OMR NO-LOCK NO-ERROR.
   FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   FOR EACH BERKALK WHERE BERKALK.AONR = bernr AND BERKALK.OMRADE = beromr NO-LOCK BY BERKALK.ARBKOD BY BERKALK.LOPNR:
      IF BERKALK.ARBKOD = "EGEN" THEN DO:
         NEXT.
      END.
      ELSE DO:
         FIND FIRST KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID AND KALKYLLOPPOSTER.ARBKOD = BERKALK.ARBKOD AND
         KALKYLLOPPOSTER.LOPNR = BERKALK.LOPNR NO-LOCK NO-ERROR.
         CREATE ber_temp2.
         ASSIGN 
         ber_temp2.ARBKOD = BERKALK.ARBKOD
         ber_temp2.LOPNR = BERKALK.LOPNR.
         IF AVAILABLE KALKYLLOPPOSTER THEN DO:
            ber_temp2.BENAMNING = KALKYLLOPPOSTER.BENAMNING.
         END.   
         ELSE DO:                           
            ber_temp2.BENAMNING = "Koden saknas i den senaste katalogen!".
         END.   
         SUBSTRING(tidutorg.UT,68) = STRING(BERKALK.ANTAL).
         IF AVAILABLE KALKYLLOPPOSTER THEN DO:
            ber_temp2.ENHET = KALKYLLOPPOSTER.ENHET.
         END.   
         ELSE DO:
            ber_temp2.ENHET = BERKALK.ENHET.
         END.
         ber_temp2.ANTAL = BERKALK.ANTAL.
         ber_temp2.NUM = BERKALK.NUM.       
      END.
   END.   
   FOR EACH BERKALK WHERE BERKALK.AONR = bernr AND BERKALK.OMRADE = beromr AND BERKALK.ARBKOD = "EGEN" NO-LOCK BY BERKALK.ARBKOD BY BERKALK.LOPNR:
      CREATE  ber_temp2.
      ASSIGN 
      ber_temp2.ARBKOD = BERKALK.ARBKOD
      ber_temp2.LOPNR = BERKALK.LOPNR
      ber_temp2.BENAMNING = BERKALK.BENAMNING
      ber_temp2.ANTAL = BERKALK.ANTAL
      ber_temp2.ENHET = BERKALK.ENHET
      ber_temp2.NUM = BERKALK.NUM.        
   END.
   
   
END PROCEDURE.


PROCEDURE procset_UI:
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.  
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.   
   IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.            
END PROCEDURE .
PROCEDURE procreset_UI :
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.  
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE procsetkop_UI:  
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR.  
   IF NOT VALID-HANDLE(fbestapph) THEN RUN EXTRATABHMT.P PERSISTENT SET fbestapph.            
END PROCEDURE. 
PROCEDURE procresetkop_UI :
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph. 
   fbestapph = ?. 
END PROCEDURE.
PROCEDURE avsluta_UI  :
END PROCEDURE.