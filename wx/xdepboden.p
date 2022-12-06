
   
   /*INLÄSNING AV PRISFIL AHLSELL*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.
/*{PROVAG.I} */
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE hjenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_depa  AS INTEGER NO-UNDO.


DEFINE BUFFER mtrlbuff FOR MTRL.



DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER    
   /*FIELD ENHET              AS CHARACTER*/
   FIELD BENAMNING          AS CHARACTER   
   FIELD NPRIS              AS DECIMAL   
   FIELD ANTAL              AS INTEGER   
   FIELD BESTPUNKT              AS INTEGER
   FIELD BESTKV              AS INTEGER 
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{EXTRADATA.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
{muswait.i} 
ASSIGN
   vald_depa = 7
   leverant = "12"
   
   filnamn = "D:\delad\PRO10S\DEPÅ\inlasPassivt lager felavhjälpning ServaNet.SKV".
   /*filnamn = "c:\inlasPassivt lager felavhjälpning ServaNet.SKV".*/
   /*filnamn = "c:\inlasPassivt lager felavhjälpning ServaNet.SKV".*/ 
   
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Sevanet felavhjälpningslager\inlasPassivt lager felavhjälpning ServaNet.SKV".*/
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Sevanet felavhjälpningslager\extrainlas.SKV".*/
   /*filnamn = "\\SERVER05\d\elpool\elplo\Boden\förråd\DEP1.skv"*/
   /*filnamn = "\\server04\d\elpool\elpnj\lapp\ahlsell\ah20903.skv"*/
   /*filnamn = "\\server04\d\elpool\elpnj\lapp\ahlsell\ah10903.skv" */
   /*filnamn = "\\server04\d\elpool\elpnj\Luleå\Priser20081104\Priser2008-10.skv" */
  /* filnamn = "\\server04\d\elpool\elpnj\tectel\materiel\prisertot.skv" */
/*filnamn = "\\server04\d\elpool\elpnj\Luleå\Priser20080331\nettol2.skv" */
/* filnamn = "/u01/guru/prisfil.skv" */

FIND FIRST FORETAG NO-LOCK NO-ERROR.
RUN in_UI.

PROCEDURE in_UI: 
   EMPTY TEMP-TABLE intid NO-ERROR.
   EMPTY TEMP-TABLE tidinah NO-ERROR.
   
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinah.
         ASSIGN.
         IMPORT DELIMITER ";" tidinah   NO-ERROR.
      END.               
   END.

   FOR EACH tidinah WHERE tidinah.ENR = "":
      DELETE tidinah.
   END.     
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep7.txt.

   RUN skapaenr_UI.           

   FOR EACH MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.IBDATUM = ? AND MTRLDEP.SALDO > 0 NO-LOCK:
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "FIFO"                                  
      inextradatatemp.HUVUDINT =  MTRLDEP.DEPNR
      inextradatatemp.HUVUDCH =  MTRLDEP.ENR
      inextradatatemp.SOKCHAR[2] =  MTRLDEP.BENAMNING
      inextradatatemp.SOKCHAR[3] =  MTRLDEP.ENHET
      inextradatatemp.SOKDEC[1] =  MTRLDEP.NPRIS
      inextradatatemp.SOKINT[1] =  MTRLDEP.SALDO
      inextradatatemp.SOKDATE[1] =  TODAY
      inextradatatemp.SOKINT[2] =  TIME.         
   END.
   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.            

   OS-DELETE VALUE(kommando).
END PROCEDURE.

PROCEDURE skapaenr_UI:   
/*    OUTPUT TO C:\kaka.txt. */
   /*OBS ! Luleå ska ha brutto = netto*/
   FOR EACH tidinah NO-LOCK:                                   
      DO TRANSACTION: 
         hjenr = tidinah.ENR.
         
         
         IF FORETAG.FORETAG = "SNAT" THEN DO: 
            IF SUBSTRING(hjenr,1,1) NE  "E" THEN hjenr = "E" + hjenr.
         END.      
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  hjenr AND
         mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
         EXCLUSIVE-LOCK NO-ERROR.
         /*IF NOT AVAILABLE mtrlbuff THEN DO:
            hjenr = "0" + tidinah.ENR.
            FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  hjenr AND
            mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
            EXCLUSIVE-LOCK NO-ERROR.            
            IF NOT AVAILABLE mtrlbuff THEN DO:
               hjenr = "00" + tidinah.ENR.
               FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  hjenr AND
               mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
               EXCLUSIVE-LOCK NO-ERROR.            
            END.
         END.*/
         IF AVAILABLE mtrlbuff THEN DO:
            FIND FIRST MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.ENR = hjenr NO-LOCK NO-ERROR.
            IF NOT AVAILABLE MTRLDEP THEN DO:            
               CREATE MTRLDEP.
               ASSIGN 
               MTRLDEP.DEPNR = vald_depa
               MTRLDEP.ENR = hjenr
               MTRLDEP.LEVKOD = leverant
               MTRLDEP.BENAMNING = mtrlbuff.BENAMNING
               MTRLDEP.ENHET = mtrlbuff.ENHET
               MTRLDEP.BESTPUNKT = tidinah.BESTPUNKT
               MTRLDEP.BESTKV = tidinah.BESTKV
               MTRLDEP.FACKID = ""
               MTRLDEP.OMSATT = 0
               MTRLDEP.NPRIS = tidinah.NPRIS / 100
               MTRLDEP.BPRIS = tidinah.NPRIS / 100 
               MTRLDEP.LAGER = TRUE
               MTRLDEP.IBDATUM = ?         
               MTRLDEP.INVDATUM = TODAY
               MTRLDEP.SALDO = tidinah.ANTAL.
            END.
            ELSE DO:
               PUT UNFORMATTED "FANNS"  tidinah.ENR SKIP.
            END.
         END.
         ELSE DO:                               
         
            PUT UNFORMATTED tidinah.ENR "  "  tidinah.benamning SKIP.
            /*
            CREATE MTRL.
            ASSIGN      
            MTRL.ENR = tidinah.ENR
            MTRL.LEVKOD = leverant 
            MTRL.KALKNR = 0        
            MTRL.BENAMNING = tidinah.BENAMNING             
            MTRL.NPRIS = tidinah.NPRIS / 100
            MTRL.ENHET = tidinah.ENHET.
            MTRL.BPRIS = tidinah.BPRIS / 100.*/

         END.   
      END.            
   END.   
/*    OUTPUT CLOSE. */
END PROCEDURE.   

                

   
   /*CREATE spec_mtrl.
   ASSIGN 
   spec_mtrl.DEPNR = vald_depa
   spec_mtrl.ENR = mtrltemp.ENR
   spec_mtrl.BENAMNING = mtrltemp.BENAMNING
   spec_mtrl.ENHET = mtrltemp.ENHET
   spec_mtrl.BESTPUNKT = FILL-IN-BESTP 
   spec_mtrl.BESTKV = FILL-IN-BESTKV
   spec_mtrl.FACKID = fackid
   spec_mtrl.OMSATT = FILL-IN-ARSOM
   spec_mtrl.NPRIS = mtrltemp.NPRIS
   spec_mtrl.BPRIS = mtrltemp.BPRIS  
   spec_mtrl.LAGER = TRUE
   MTRLDEP.IBDATUM = ?         
   MTRLDEP.INVDATUM = invdat.*/
=======
   
   /*INLÄSNING AV PRISFIL AHLSELL*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE gurubilder AS CHARACTER NO-UNDO.
/*{PROVAG.I} */
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE hjenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_depa  AS INTEGER NO-UNDO.


DEFINE BUFFER mtrlbuff FOR MTRL.



DEFINE TEMP-TABLE tidinah
   FIELD ENR                AS CHARACTER    
   /*FIELD ENHET              AS CHARACTER*/
   FIELD BENAMNING          AS CHARACTER   
   FIELD NPRIS              AS DECIMAL   
   FIELD ANTAL              AS INTEGER   
   FIELD BESTPUNKT              AS INTEGER
   FIELD BESTKV              AS INTEGER 
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{EXTRADATA.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
{muswait.i} 
ASSIGN
   vald_depa = 7
   leverant = "12"
   
   filnamn = "D:\delad\PRO10S\DEPÅ\inlasPassivt lager felavhjälpning ServaNet.SKV".
   /*filnamn = "c:\inlasPassivt lager felavhjälpning ServaNet.SKV".*/
   /*filnamn = "c:\inlasPassivt lager felavhjälpning ServaNet.SKV".*/ 
   
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Sevanet felavhjälpningslager\inlasPassivt lager felavhjälpning ServaNet.SKV".*/
   /*filnamn = "\\SERVER05\d\elpool\elplo\Sundsvall Elnät\Depå\Sevanet felavhjälpningslager\extrainlas.SKV".*/
   /*filnamn = "\\SERVER05\d\elpool\elplo\Boden\förråd\DEP1.skv"*/
   /*filnamn = "\\server04\d\elpool\elpnj\lapp\ahlsell\ah20903.skv"*/
   /*filnamn = "\\server04\d\elpool\elpnj\lapp\ahlsell\ah10903.skv" */
   /*filnamn = "\\server04\d\elpool\elpnj\Luleå\Priser20081104\Priser2008-10.skv" */
  /* filnamn = "\\server04\d\elpool\elpnj\tectel\materiel\prisertot.skv" */
/*filnamn = "\\server04\d\elpool\elpnj\Luleå\Priser20080331\nettol2.skv" */
/* filnamn = "/u01/guru/prisfil.skv" */

FIND FIRST FORETAG NO-LOCK NO-ERROR.
RUN in_UI.

PROCEDURE in_UI: 
   EMPTY TEMP-TABLE intid NO-ERROR.
   EMPTY TEMP-TABLE tidinah NO-ERROR.
   
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidinah.
         ASSIGN.
         IMPORT DELIMITER ";" tidinah   NO-ERROR.
      END.               
   END.

   FOR EACH tidinah WHERE tidinah.ENR = "":
      DELETE tidinah.
   END.     
   OUTPUT TO D:\delad\PRO10S\DEPÅ\feldep7.txt.

   RUN skapaenr_UI.           

   FOR EACH MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.IBDATUM = ? AND MTRLDEP.SALDO > 0 NO-LOCK:
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "FIFO"                                  
      inextradatatemp.HUVUDINT =  MTRLDEP.DEPNR
      inextradatatemp.HUVUDCH =  MTRLDEP.ENR
      inextradatatemp.SOKCHAR[2] =  MTRLDEP.BENAMNING
      inextradatatemp.SOKCHAR[3] =  MTRLDEP.ENHET
      inextradatatemp.SOKDEC[1] =  MTRLDEP.NPRIS
      inextradatatemp.SOKINT[1] =  MTRLDEP.SALDO
      inextradatatemp.SOKDATE[1] =  TODAY
      inextradatatemp.SOKINT[2] =  TIME.         
   END.
   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.            

   OS-DELETE VALUE(kommando).
END PROCEDURE.

PROCEDURE skapaenr_UI:   
/*    OUTPUT TO C:\kaka.txt. */
   /*OBS ! Luleå ska ha brutto = netto*/
   FOR EACH tidinah NO-LOCK:                                   
      DO TRANSACTION: 
         hjenr = tidinah.ENR.
         
         
         IF FORETAG.FORETAG = "SNAT" THEN DO: 
            IF SUBSTRING(hjenr,1,1) NE  "E" THEN hjenr = "E" + hjenr.
         END.      
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  hjenr AND
         mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
         EXCLUSIVE-LOCK NO-ERROR.
         /*IF NOT AVAILABLE mtrlbuff THEN DO:
            hjenr = "0" + tidinah.ENR.
            FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  hjenr AND
            mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
            EXCLUSIVE-LOCK NO-ERROR.            
            IF NOT AVAILABLE mtrlbuff THEN DO:
               hjenr = "00" + tidinah.ENR.
               FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  hjenr AND
               mtrlbuff.LEVKOD = leverant AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
               EXCLUSIVE-LOCK NO-ERROR.            
            END.
         END.*/
         IF AVAILABLE mtrlbuff THEN DO:
            FIND FIRST MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.ENR = hjenr NO-LOCK NO-ERROR.
            IF NOT AVAILABLE MTRLDEP THEN DO:            
               CREATE MTRLDEP.
               ASSIGN 
               MTRLDEP.DEPNR = vald_depa
               MTRLDEP.ENR = hjenr
               MTRLDEP.LEVKOD = leverant
               MTRLDEP.BENAMNING = mtrlbuff.BENAMNING
               MTRLDEP.ENHET = mtrlbuff.ENHET
               MTRLDEP.BESTPUNKT = tidinah.BESTPUNKT
               MTRLDEP.BESTKV = tidinah.BESTKV
               MTRLDEP.FACKID = ""
               MTRLDEP.OMSATT = 0
               MTRLDEP.NPRIS = tidinah.NPRIS / 100
               MTRLDEP.BPRIS = tidinah.NPRIS / 100 
               MTRLDEP.LAGER = TRUE
               MTRLDEP.IBDATUM = ?         
               MTRLDEP.INVDATUM = TODAY
               MTRLDEP.SALDO = tidinah.ANTAL.
            END.
            ELSE DO:
               PUT UNFORMATTED "FANNS"  tidinah.ENR SKIP.
            END.
         END.
         ELSE DO:                               
         
            PUT UNFORMATTED tidinah.ENR "  "  tidinah.benamning SKIP.
            /*
            CREATE MTRL.
            ASSIGN      
            MTRL.ENR = tidinah.ENR
            MTRL.LEVKOD = leverant 
            MTRL.KALKNR = 0        
            MTRL.BENAMNING = tidinah.BENAMNING             
            MTRL.NPRIS = tidinah.NPRIS / 100
            MTRL.ENHET = tidinah.ENHET.
            MTRL.BPRIS = tidinah.BPRIS / 100.*/

         END.   
      END.            
   END.   
/*    OUTPUT CLOSE. */
END PROCEDURE.   

                

   
   /*CREATE spec_mtrl.
   ASSIGN 
   spec_mtrl.DEPNR = vald_depa
   spec_mtrl.ENR = mtrltemp.ENR
   spec_mtrl.BENAMNING = mtrltemp.BENAMNING
   spec_mtrl.ENHET = mtrltemp.ENHET
   spec_mtrl.BESTPUNKT = FILL-IN-BESTP 
   spec_mtrl.BESTKV = FILL-IN-BESTKV
   spec_mtrl.FACKID = fackid
   spec_mtrl.OMSATT = FILL-IN-ARSOM
   spec_mtrl.NPRIS = mtrltemp.NPRIS
   spec_mtrl.BPRIS = mtrltemp.BPRIS  
   spec_mtrl.LAGER = TRUE
   MTRLDEP.IBDATUM = ?         
   MTRLDEP.INVDATUM = invdat.*/
>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
