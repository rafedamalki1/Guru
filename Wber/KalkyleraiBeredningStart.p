
/*------------------------------------------------------------------------
    File        : KalkyleraiBeredningStart.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Sep 05 14:01:50 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
{GLOBVAR2DEL1.I}
  
DEFINE INPUT  PARAMETER inglobanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE aonrplan AS LOGICAL NO-UNDO. /* = true aonr = false planr*/


FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
DEFINE VARIABLE berkopptabbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE berdningtabbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE berkalktabbuffh AS HANDLE NO-UNDO.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
CREATE WIDGET-POOL "DynTableiKST" NO-ERROR.
DEFINE VARIABLE AppServerExtraHandle AS HANDLE NO-UNDO.

PROCEDURE KalkyleraiBeredning_UI :
   DEFINE INPUT  PARAMETER klogidvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER BernrVar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER OmrVar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER felmedd AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER ejstart AS LOGICAL NO-UNDO.
   DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
   CREATE BUFFER berkopptabbuffh FOR TABLE "BERKALKOPPLA" IN WIDGET-POOL "DynTableiKST".
   CREATE BUFFER berdningtabbuffh FOR TABLE "BEREDNING" IN WIDGET-POOL "DynTableiKST".
   berkopptabbuffh:FIND-FIRST("WHERE BERNR = " + STRING(BernrVar) + " AND OMRADE = '" + OmrVar + "'", NO-LOCK) NO-ERROR.
   IF berkopptabbuffh:AVAILABLE THEN.
   ELSE DO:
      RUN omradekoll_UI  (INPUT OmrVar, OUTPUT KalkNrvar).  
      IF KalkNrvar = ? THEN DO:
         felmedd = "Det går inte att lägga upp kalkyler på detta " + LC(Guru.Konstanter:gomrk) + ". Nummerserie saknas eller är fylld.".
         ejstart = TRUE.
         RETURN.
      END.         
      FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
      berdningtabbuffh:FIND-FIRST("WHERE BERNR = " + STRING(BernrVar) + " AND OMRADE = '" + OmrVar + "'",NO-LOCK) NO-ERROR.
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = berdningtabbuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE ANVANDARE THEN DO:
         FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
           
      END.
      IF NOT AVAILABLE ANVANDARE THEN DO:
         FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD NE "" NO-LOCK NO-ERROR.
      END.   
      IF ANVANDARE.PERSONALKOD = "" THEN DO:
         FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD NE "" NO-LOCK NO-ERROR.
      END.   
      DO TRANSACTION:
         CREATE KALKHUV.
         ASSIGN
         KALKHUV.AKTIV = TRUE
         KALKHUV.BENAMNING = berdningtabbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE 
         KALKHUV.KALKANV = ANVANDARE.PERSONALKOD
         KALKHUV.ANVANDARE = berdningtabbuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE
         KALKHUV.KALKNR = KalkNrvar
         KALKHUV.OMRADE = OmrVar   
         KALKHUV.BESTID = OmrVar  
         KALKHUV.TYPKALK = Guru.Konstanter:varforetypval[53].
         IF klogidvar = 0 THEN KALKHUV.KLOGID = KALKYLKATALOG.KLOGID.
         ELSE KALKHUV.KLOGID = klogidvar.
         berkopptabbuffh:BUFFER-CREATE().
         berkopptabbuffh:BUFFER-FIELD("AONR"):BUFFER-VALUE = STRING(BernrVar).
         berkopptabbuffh:BUFFER-FIELD("BERNR"):BUFFER-VALUE = BernrVar.
         berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = OmrVar.
         berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = KALKHUV.KALKNR.
         berkopptabbuffh:BUFFER-FIELD("OMRADEKALK"):BUFFER-VALUE = KALKHUV.OMRADE.
         
      END.   
   END.
   RELEASE KALKHUV NO-ERROR.
   KalkNrvar = berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE .
   berkalktabbuffh:BUFFER-RELEASE () NO-ERROR.
   berdningtabbuffh:BUFFER-RELEASE () NO-ERROR.
   DELETE OBJECT berkopptabbuffh  NO-ERROR.
   berkopptabbuffh = ?.
   DELETE OBJECT berdningtabbuffh NO-ERROR.
   berdningtabbuffh = ?.
   
END PROCEDURE.


/*tar fram rätt nummer*/
PROCEDURE omradekoll_UI:
   DEFINE INPUT PARAMETER omradevar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER tempvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
   DO TRANSACTION:
      IF Guru.Konstanter:varforetypchar[3] NE "" THEN DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = Guru.Konstanter:varforetypchar[3] EXCLUSIVE-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = omradevar EXCLUSIVE-LOCK NO-ERROR.
      END.
      IF NOT AVAILABLE OMRADETAB THEN FIND FIRST OMRADETAB USE-INDEX OMR EXCLUSIVE-LOCK NO-ERROR.
      IF OMRADETAB.KALKYLINT2 < OMRADETAB.KALKYLSIST OR OMRADETAB.KALKYLINT1 = OMRADETAB.KALKYLINT2 THEN DO:
         tempvar = ?.     
         RETURN.
      END.
      ELSE tempvar = OMRADETAB.KALKYLSIST.
     
      RUN kalksista_UI (INPUT-OUTPUT tempvar).
   END.  
END PROCEDURE.
PROCEDURE kalksista_UI :
   DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.  
   DEFINE INPUT-OUTPUT PARAMETER tempvar AS INTEGER NO-UNDO.
   DEFINE BUFFER KALKHUVSISTA FOR KALKHUV.
   DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
   DEFINE VARIABLE leavevar AS LOGICAL NO-UNDO.
   
   RUN KALKBERAPPDSEXTRA.p PERSISTENT SET AppServerExtraHandle (INPUT Guru.Konstanter:globanv).
   RUN FINNSTABELL.P (INPUT "FASTSPEC", OUTPUT bloblog).
   IF tempvar < OMRADETAB.KALKYLINT1 THEN tempvar = OMRADETAB.KALKYLINT1.
   REPEAT:
      FIND FIRST KALKHUVSISTA WHERE KALKHUVSISTA.KALKNR = tempvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKHUVSISTA THEN DO:
         IF bloblog = TRUE THEN DO:
            /*koll om fastkalknr finns*/
            RUN sistafastspec_UI IN AppServerExtraHandle (INPUT tempvar, OUTPUT leavevar).
            IF leavevar = TRUE THEN LEAVE.
         END.
      END.   
      tempvar = tempvar + 1.
      IF tempvar > OMRADETAB.KALKYLINT2 THEN DO:
         felkoll = TRUE.
         LEAVE.
      END.    
   END.              
   OMRADETAB.KALKYLSIST = tempvar.
   IF felkoll = TRUE THEN tempvar = ?.  
   FIND CURRENT OMRADETAB NO-LOCK. 
   IF VALID-HANDLE(AppServerExtraHandle) THEN DELETE PROCEDURE AppServerExtraHandle NO-ERROR.
   AppServerExtraHandle = ?.
END PROCEDURE.



PROCEDURE avsluta_UI :
   RELEASE KALKNUMSUB NO-ERROR.
   RELEASE KALKNUM NO-ERROR.
   DELETE WIDGET-POOL "DynTableiKST" NO-ERROR.
   
END PROCEDURE.