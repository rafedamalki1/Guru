/*KOPIBER.p KOPIERA EN BEREDNING från bla bermeny . 
Dock kopieras inte tabellen BEREDNING.

*/
{STARTFORAPP.I}
{HDKALKTEMP.I} 
{MARKGRUPP.I}
DEFINE  TEMP-TABLE list_mtrl2 
   {LISTMTRLTT.I}
CREATE WIDGET-POOL "DynTableBH" NO-ERROR.
DEFINE VARIABLE datvar LIKE BERMTRL.DATUM NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.

DEFINE INPUT PARAMETER valaonr2 LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade2 LIKE AONRTAB.OMRADE NO-UNDO.



DEFINE VARIABLE berkopptabbuffh AS HANDLE NO-UNDO.
DEFINE BUFFER konbuff FOR BERVAL.
DEFINE BUFFER ordbuff FOR BERORD.
DEFINE BUFFER mtrlbuff FOR BERMTRL.
DEFINE BUFFER idbuff FOR BERID.
DEFINE BUFFER kabbuff FOR BERLINKAB.
DEFINE BUFFER uppbuff FOR BERUPP.
DEFINE BUFFER kalkbuff FOR BERKALK.
DEFINE BUFFER fribuff FOR FRIKORT.
DEFINE BUFFER skyddbuff FOR KSKYDD.      
DEFINE VARIABLE grundkloid AS INTEGER NO-UNDO.     
{EXTRATAB.I}
FIND FIRST BETFRIA WHERE BETFRIA.BETNR = INTEGER(valaonr2) AND
BETFRIA.FAKTTEXT = valomrade2 
NO-LOCK NO-ERROR.
IF AVAILABLE BETFRIA THEN DO:
   Guru.SharedVariable:ValdmtrlLeverantor = BETFRIA.TYP. 
END.

DEFINE VARIABLE ekoppdataapph AS HANDLE NO-UNDO.
OPEN QUERY kq FOR EACH BERVAL WHERE BERVAL.AONR = valaonr AND 
BERVAL.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
GET FIRST kq NO-LOCK.
IF AVAILABLE BERVAL THEN DO TRANSACTION:
   CREATE konbuff.
   BUFFER-COPY BERVAL TO konbuff.
   ASSIGN
   konbuff.AONR = valaonr2
   konbuff.OMRADE = valomrade2
   konbuff.DELNR = 0
   konbuff.ORT = "". /*Nollställa om konstruktionen är delinköpt*/     
END.   
REPEAT:
   GET NEXT kq NO-LOCK.
   IF AVAILABLE BERVAL THEN DO TRANSACTION:
      CREATE konbuff.
      BUFFER-COPY BERVAL TO konbuff.
      ASSIGN
      konbuff.AONR = valaonr2
      konbuff.OMRADE = valomrade2
      konbuff.DELNR = 0
      konbuff.ORT = "".      
   END.
   ELSE LEAVE.
END.
CLOSE QUERY kq.

OPEN QUERY kq2 FOR EACH BERORD WHERE BERORD.AONR = valaonr AND 
BERORD.OMRADE = valomrade USE-INDEX ORD NO-LOCK.
GET FIRST kq2 NO-LOCK.
IF AVAILABLE BERORD THEN DO TRANSACTION:
   CREATE ordbuff.
   BUFFER-COPY BERORD TO ordbuff.
   ASSIGN
   ordbuff.OMRADE = valomrade2
   ordbuff.AONR = valaonr2.      
END.   
REPEAT:
   GET NEXT kq2 NO-LOCK.
   IF AVAILABLE BERORD THEN DO TRANSACTION:
      CREATE ordbuff.
      BUFFER-COPY BERORD TO ordbuff.
      ASSIGN
      ordbuff.OMRADE = valomrade2
      ordbuff.AONR = valaonr2.      
   END.
   ELSE LEAVE.
END.
CLOSE QUERY kq2.       

FIND LAST BERMTRL WHERE BERMTRL.AONR = valaonr AND 
BERMTRL.OMRADE = valomrade AND BERMTRL.INKOP = FALSE 
USE-INDEX DATUM NO-LOCK NO-ERROR.
IF AVAILABLE BERMTRL THEN DO:      
   ASSIGN
   datvar = BERMTRL.DATUM.                                       
END.   
ELSE DO:
   ASSIGN
   datvar = TODAY.                                       
END.
RUN ReMtrluppl_UI.  
OPEN QUERY berqlin FOR EACH BERLINKAB WHERE BERLINKAB.AONR = valaonr AND
BERLINKAB.OMRADE = valomrade AND BERLINKAB.DATUM = datvar USE-INDEX DATUM NO-LOCK.
GET FIRST berqlin NO-LOCK.
DO WHILE AVAILABLE(BERLINKAB):   
   DO TRANSACTION:      
      CREATE kabbuff.
      BUFFER-COPY BERLINKAB TO kabbuff.
      ASSIGN
      kabbuff.AONR = valaonr2
      kabbuff.OMRADE = valomrade2
      kabbuff.DELNR = 0
      kabbuff.DATUM = TODAY.
      /*UPPDATERA PRIS TILL DAGENS*/
      FIND FIRST MTRL WHERE MTRL.LEVKOD = BERLINKAB.LEVKOD AND MTRL.ENR = BERLINKAB.ENR AND 
      MTRL.KALKNR = 0 NO-LOCK NO-ERROR.
      IF AVAILABLE MTRL THEN DO:
         kabbuff.PRIS = MTRL.NPRIS.
      END.
   END.
   GET NEXT berqlin NO-LOCK.
END.
CLOSE QUERY berqlin.      

OPEN QUERY kq4 FOR EACH BERID WHERE BERID.AONR = valaonr AND 
BERID.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
GET FIRST kq4 NO-LOCK.
IF AVAILABLE BERID THEN DO TRANSACTION:
   CREATE idbuff.
   BUFFER-COPY BERID TO idbuff.
   ASSIGN   
   idbuff.AONR = valaonr2
   idbuff.OMRADE = valomrade2.      
END.   
REPEAT:
   GET NEXT kq4 NO-LOCK.
   IF AVAILABLE BERID THEN DO TRANSACTION:
      CREATE idbuff.
      BUFFER-COPY BERID TO idbuff.
      ASSIGN   
      idbuff.AONR = valaonr2
      idbuff.OMRADE = valomrade2.      
   END.
   ELSE LEAVE.
END.
CLOSE QUERY kq4.     
 
OPEN QUERY kq6 FOR EACH BERUPP WHERE BERUPP.AONR = valaonr AND 
BERUPP.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
GET FIRST kq6 NO-LOCK.
IF AVAILABLE BERUPP THEN DO TRANSACTION:
   CREATE uppbuff.
   BUFFER-COPY BERUPP TO uppbuff.
   ASSIGN
   uppbuff.AONR = valaonr2
   uppbuff.OMRADE = valomrade2
   uppbuff.DELNR = 0
   uppbuff.ANTALRADER= 0.
END.   
REPEAT:
   GET NEXT kq6 NO-LOCK.
   IF AVAILABLE BERUPP THEN DO TRANSACTION:
      CREATE uppbuff.
      BUFFER-COPY BERUPP TO uppbuff.
      ASSIGN
      uppbuff.AONR = valaonr2
      uppbuff.OMRADE = valomrade2
      uppbuff.DELNR = 0
      uppbuff.ANTALRADER= 0.
   END.
   ELSE LEAVE.
END.
CLOSE QUERY kq6.

RUN BKID_UI (INPUT valaonr,INPUT valomrade,INPUT valaonr2,INPUT valomrade2).
RUN DelPool_UI.

OPEN QUERY kq8 FOR EACH FRIKORT WHERE FRIKORT.AONR = valaonr AND 
FRIKORT.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
GET FIRST kq8 NO-LOCK.
IF AVAILABLE FRIKORT THEN DO TRANSACTION:
   CREATE fribuff.
   BUFFER-COPY FRIKORT TO fribuff.
   ASSIGN
   fribuff.AONR = valaonr2  
   fribuff.OMRADE = valomrade2.      
END.   
REPEAT:
   GET NEXT kq8 NO-LOCK.
   IF AVAILABLE FRIKORT THEN DO TRANSACTION:
      CREATE fribuff.
      BUFFER-COPY FRIKORT TO fribuff.
      ASSIGN
      fribuff.AONR = valaonr2  
      fribuff.OMRADE = valomrade2.      
   END.
   ELSE LEAVE.
END.
CLOSE QUERY kq8.

OPEN QUERY kq12 FOR EACH KSKYDD WHERE KSKYDD.AONR = valaonr AND 
KSKYDD.OMRADE = valomrade AND KSKYDD.BERED = TRUE USE-INDEX OMR NO-LOCK.
GET FIRST kq12 NO-LOCK.
IF AVAILABLE KSKYDD THEN DO TRANSACTION:
   CREATE skyddbuff.
   BUFFER-COPY KSKYDD TO skyddbuff.
   ASSIGN
   skyddbuff.AONR = valaonr2
   skyddbuff.OMRADE = valomrade2
   skyddbuff.DATUM = TODAY.      
END.   
REPEAT:
   GET NEXT kq12 NO-LOCK.
   IF AVAILABLE KSKYDD THEN DO TRANSACTION:
      CREATE skyddbuff.
      BUFFER-COPY KSKYDD TO skyddbuff.
      ASSIGN
      skyddbuff.AONR = valaonr2
      skyddbuff.OMRADE = valomrade2
      skyddbuff.DATUM = TODAY.      
   END.
   ELSE LEAVE.
END.
CLOSE QUERY kq12.           
RUN EXTRATABHMT.P PERSISTENT SET ekoppdataapph.      
EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
CREATE inextrakopptemp.          
ASSIGN
inextrakopptemp.PROGRAM = "MARKSTN"                   
inextrakopptemp.KOPPLACHAR1 = valaonr               
inextrakopptemp.KOPPLAINT1 = ?
inextrakopptemp.KOPPLACHAR2 = valomrade            
inextrakopptemp.KOPPLAINT2 =  ?.
RUN etabhamt_UI IN ekoppdataapph (INPUT TABLE inextrakopptemp, OUTPUT TABLE extrakopptemp). 
EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
FOR EACH extrakopptemp:
   CREATE inextrakopptemp.
   BUFFER-COPY extrakopptemp TO inextrakopptemp.
   ASSIGN
   inextrakopptemp.KOPPLACHAR1 = valaonr2     
   inextrakopptemp.KOPPLACHAR2 = valomrade2.
END.
RUN extraspar_UI IN ekoppdataapph (INPUT TABLE inextrakopptemp). 
EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
IF VALID-HANDLE(ekoppdataapph) THEN DELETE PROCEDURE ekoppdataapph.
RUN MarkstationKopi_UI (INPUT valaonr, INPUT valomrade,INPUT valaonr2, INPUT valomrade2).
/*HD HÄR*/

RUN FINNSTABELL.P (INPUT "HDKALK", OUTPUT bloblog).
IF bloblog = TRUE THEN DO:
   RUN HDKOP.P (INPUT INTEGER(valaonr),INPUT valomrade,INTEGER(valaonr2),INPUT valomrade2).
END.
{BERKALKBKID.I}
{Markstationhmt.i}

PROCEDURE ReMtrluppl_UI :
   
   DEFINE VARIABLE Revers AS CHARACTER NO-UNDO.
   DEFINE VARIABLE BerValdNr AS INTEGER NO-UNDO.
   DEFINE VARIABLE BerValdaOmr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ValdBerLev AS CHARACTER NO-UNDO.
   DEFINE VARIABLE BerValdNrKopia AS INTEGER NO-UNDO.
   DEFINE VARIABLE BerValdaOmrKopia AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ValdBerLevKopia AS CHARACTER NO-UNDO.
   DEFINE VARIABLE listnrkopia AS INTEGER NO-UNDO.
   
   BerValdNr = INTEGER(valaonr).
   BerValdaOmr = valomrade.
   
   FIND FIRST BETFRIA WHERE BETFRIA.BETNR = BerValdNr AND
   BETFRIA.FAKTTEXT = BerValdaOmr 
   NO-LOCK NO-ERROR.
   IF AVAILABLE BETFRIA THEN DO:
      ValdBerLev = BETFRIA.TYP. 
      Guru.SharedVariable:KopiFranBerLeverantor = ValdBerLev.
   END.
   BerValdNrKopia = INTEGER(valaonr2).
   BerValdaOmrKopia = valomrade2.
   FIND FIRST BETFRIA WHERE BETFRIA.BETNR = INTEGER(valaonr2) AND
   BETFRIA.FAKTTEXT = valomrade2 
   NO-LOCK NO-ERROR.
   IF AVAILABLE BETFRIA THEN DO:
      ValdBerLevKopia = BETFRIA.TYP. 
      Guru.SharedVariable:KopiTillBerLeverantor = ValdBerLevKopia.
   END.   

   OPEN QUERY berqmtrl FOR EACH BERMTRL WHERE BERMTRL.AONR = STRING(BerValdNr) AND
   BERMTRL.OMRADE = BerValdaOmr AND BERMTRL.INKOP = FALSE AND
   BERMTRL.DATUM = datvar USE-INDEX DATUM NO-LOCK.
   GET FIRST berqmtrl NO-LOCK.
   DO WHILE AVAILABLE(BERMTRL):   
      DO TRANSACTION:    
         CREATE list_mtrl2.
         BUFFER-COPY BERMTRL TO list_mtrl2.
         list_mtrl2.LISTAKUNDLEV = FALSE.
      END.      
      GET NEXT berqmtrl NO-LOCK.
   END.
   CLOSE QUERY berqmtrl.
   {UTBBEREDNINGUID.I}
   Guru.SharedVariable:KopiFranListnr = listnr.
   beredningh:FIND-FIRST("WHERE BERNR = " + STRING(BerValdNrKopia) + " AND OMRADE = " + QUOTER(BerValdaOmrKopia) ,NO-LOCK) NO-ERROR.
   IF beredningh:AVAILABLE THEN DO:
      listnrKopia = beredningh:BUFFER-FIELD("UID"):BUFFER-VALUE.       
      Guru.SharedVariable:KopiTillListnr = listnrKopia.
   END.
   
   IF ValdBerLevKopia = ValdBerLev AND listnr = 0 AND listnrKopia = 0 THEN DO: 
      FOR EACH list_mtrl2 WHERE NO-LOCK:
         DO TRANSACTION: 
            CREATE mtrlbuff.
            BUFFER-COPY list_mtrl2 TO mtrlbuff.
            ASSIGN 
            mtrlbuff.AONR = STRING(BerValdNrKopia)
            mtrlbuff.OMRADE = BerValdaOmrKopia
            mtrlbuff.DELNR = 0
            mtrlbuff.DATUM = TODAY.
         END.
      END.
      RELEASE mtrlbuff NO-ERROR.
      RETURN.
   END.
   FOR EACH list_mtrl2:
      list_mtrl2.LISTAKUNDLEV = FALSE.
   END. 
   Revers = "ÅTERUPP".
   /*ÅTERSTLLER TILL UPPLÄGGET*/
   FOR EACH list_mtrl2 WHERE list_mtrl2.LISTAKUNDLEV = FALSE:
      RUN UTBYTESLISTA.P (INPUT Revers, INPUT listnr,INPUT list_mtrl2.LEVKOD,INPUT list_mtrl2.ENR,INPUT ValdBerLevKopia, OUTPUT mrID). 
      FIND FIRST MTRL WHERE ROWID(MTRL) = mrID NO-LOCK NO-ERROR.
      IF AVAILABLE MTRL THEN DO:
         ASSIGN
         list_mtrl2.ENR       = MTRL.ENR
         list_mtrl2.BENAMNING = MTRL.BENAMNING
         list_mtrl2.ENHET     = MTRL.ENHET
         list_mtrl2.PRIS      = MTRL.NPRIS
         list_mtrl2.LEVKOD    = MTRL.LEVKOD.
      END.   
      list_mtrl2.LISTAKUNDLEV = TRUE.   
   END.
    
   Revers = "UPPERSATT". 
   BerValdNr = INTEGER(valaonr2).
   BerValdaOmr = valomrade2.
   FIND FIRST BETFRIA WHERE BETFRIA.BETNR = BerValdNr AND
   BETFRIA.FAKTTEXT = BerValdaOmr 
   NO-LOCK NO-ERROR.
   IF AVAILABLE BETFRIA THEN DO:
      ValdBerLev = BETFRIA.TYP. 
   END.
   beredningh:FIND-FIRST("WHERE BERNR = " + STRING(BerValdNr) + " AND OMRADE = " + QUOTER(BerValdaOmr) ,NO-LOCK) NO-ERROR.
   IF beredningh:AVAILABLE THEN DO:
      listnr = beredningh:BUFFER-FIELD("UID"):BUFFER-VALUE. 
   END.
   FOR EACH list_mtrl2 WHERE NO-LOCK:
      list_mtrl2.LISTAKUNDLEV = FALSE.
   END.  
   FOR EACH list_mtrl2 WHERE list_mtrl2.LISTAKUNDLEV = FALSE:
      RUN UTBYTESLISTA.P (INPUT Revers, INPUT listnr,INPUT list_mtrl2.LEVKOD,INPUT list_mtrl2.ENR,INPUT ValdBerLev, OUTPUT mrID). 
      FIND FIRST MTRL WHERE ROWID(MTRL) = mrID NO-LOCK NO-ERROR.
      IF AVAILABLE MTRL THEN DO:
         ASSIGN
         list_mtrl2.ENR       = MTRL.ENR
         list_mtrl2.BENAMNING = MTRL.BENAMNING
         list_mtrl2.ENHET     = MTRL.ENHET
         list_mtrl2.PRIS      = MTRL.NPRIS
         list_mtrl2.LEVKOD    = MTRL.LEVKOD.
      END.   
      list_mtrl2.LISTAKUNDLEV = TRUE.   
   END.
    
   FOR EACH list_mtrl2 WHERE NO-LOCK:
      DO TRANSACTION: 
         CREATE mtrlbuff.
         BUFFER-COPY list_mtrl2 TO mtrlbuff.
         ASSIGN 
         mtrlbuff.AONR = STRING(BerValdNr)
         mtrlbuff.OMRADE = BerValdaOmr
         mtrlbuff.DELNR = 0
         mtrlbuff.DATUM = TODAY.
      END.
   END.
   RELEASE mtrlbuff NO-ERROR.
   Guru.SharedVariable:KopiFranBerLeverantor = "".
   Guru.SharedVariable:KopitILLBerLeverantor = "".
   Guru.SharedVariable:KopiFranListnr = 0.
   Guru.SharedVariable:KopiFranListnr = 0.   
END PROCEDURE.
