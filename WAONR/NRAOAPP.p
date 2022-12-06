/*NRAOAPP.P*/
DEFINE INPUT PARAMETER Cglobomr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER RAD_FAST AS LOGICAL NO-UNDO.     
DEFINE INPUT-OUTPUT PARAMETER valnr AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE fastegetnr AS LOGICAL NO-UNDO.
DEFINE VARIABLE aoint AS INTEGER NO-UNDO.
DEFINE VARIABLE aonrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE aonrlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE prevalnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
{EXTRADATA.I}
RUN EXTRADATAHMT.P PERSISTENT SET edataapph.

FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT FORETAG.FORETAG).
fastegetnr = FALSE.
IF valnr BEGINS "F" THEN DO:
   valnr = SUBSTRING(valnr,2).   
   fastegetnr = TRUE.
END.
aonrlangd = Guru.Konstanter:varforetypval[8]. 
IF FORETAG.FORETAG = "CSUND" OR FORETAG.FORETAG = "Celpa" THEN aonrlangd = 5. 
IF FORETAG.FORETAG = "SUND" OR FORETAG.FORETAG = "SNAT" OR FORETAG.FORETAG = "celpa" THEN DO: 
   IF valnr BEGINS "S" THEN aonrlangd = 6. 
   ELSE aonrlangd = 5. 
END.
aonrvar = 1.
IF fastegetnr = TRUE THEN DO:
   IF LENGTH(valnr) > aonrlangd THEN DO:
      valnr = "XXXXXX".
      RETURN.
   END.
END.
ELSE IF LENGTH(valnr) >= aonrlangd THEN DO:
   valnr = "XXXXXX".
   RETURN.
END.
DO TRANSACTION:
  
   IF fastegetnr = TRUE THEN DO:
      FIND LAST AONRTAB WHERE AONRTAB.AONR = valnr USE-INDEX aonr NO-LOCK NO-ERROR.   
      IF AVAILABLE AONRTAB THEN DO: 
         valnr = "XXXXXX".
         RETURN.
      END.
   END.
   ELSE DO:
        
      IF FORETAG.FORETAG = "SNAT" THEN DO:
         prevalnr = valnr.
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "SISTNRPRE"                   
         inextradatatemp.HUVUDCH = prevalnr.   
         inextradatatemp.HUVUDINT = ?.         
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
         IF AVAILABLE extradatatemp THEN DO:     
            valnr = extradatatemp.SOKCHAR[1].               
            RUN aopresista_UI(INPUT-OUTPUT valnr, INPUT prevalnr).
            IF valnr BEGINS prevalnr THEN.
            ELSE DO:
               valnr = "XXXXXX".      
               RETURN.
            END.               
         END.
         ELSE DO:
            FIND FIRST AONRTAB WHERE AONRTAB.AONR BEGINS valnr AND LENGTH(AONRTAB.AONR) = aonrlangd USE-INDEX aonr NO-LOCK NO-ERROR.   
            IF NOT AVAILABLE AONRTAB THEN DO: 
               IF aonrlangd - LENGTH(valnr) = 5 THEN valnr = valnr + "00001".
               IF aonrlangd - LENGTH(valnr) = 4 THEN valnr = valnr + "0001".
               IF aonrlangd - LENGTH(valnr) = 3 THEN valnr = valnr + "001".
               IF aonrlangd - LENGTH(valnr) = 2 THEN valnr = valnr + "01".
               IF aonrlangd - LENGTH(valnr) = 1 THEN valnr = valnr + "1".   
               RUN aopresista_UI(INPUT-OUTPUT valnr, INPUT prevalnr).   
            END.
            ELSE DO:               
               valnr = AONRTAB.AONR.           
               RUN aopresista_UI(INPUT-OUTPUT valnr, INPUT prevalnr).
               IF valnr BEGINS prevalnr THEN.
               ELSE DO:
                  valnr = "XXXXXX".      
                  RETURN.
               END.               
            END.
         END.       
      END.
      ELSE DO:   
         FIND LAST AONRTAB WHERE AONRTAB.AONR BEGINS valnr AND 
         LENGTH(AONRTAB.AONR) = aonrlangd
         USE-INDEX aonr NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE AONRTAB THEN DO: 
            IF aonrlangd - LENGTH(valnr) = 5 THEN valnr = valnr + "00001".
            IF aonrlangd - LENGTH(valnr) = 4 THEN valnr = valnr + "0001".
            IF aonrlangd - LENGTH(valnr) = 3 THEN valnr = valnr + "001".
            IF aonrlangd - LENGTH(valnr) = 2 THEN valnr = valnr + "01".
            IF aonrlangd - LENGTH(valnr) = 1 THEN valnr = valnr + "1".      
         END.
         ELSE DO:
            RUN nrkoll_UI (INPUT LENGTH(valnr),INPUT aonrlangd).
            IF valnr BEGINS "X"  THEN RETURN.
            RUN nr_UI (INPUT LENGTH(valnr),INPUT aonrlangd).
            IF aonrlangd - LENGTH(valnr) = 5 THEN valnr = valnr + STRING(aoint,"99999").
            IF aonrlangd - LENGTH(valnr) = 4 THEN valnr = valnr + STRING(aoint,"9999").
            IF aonrlangd - LENGTH(valnr) = 3 THEN valnr = valnr + STRING(aoint,"999").
            IF aonrlangd - LENGTH(valnr) = 2 THEN valnr = valnr + STRING(aoint,"99").
            IF aonrlangd - LENGTH(valnr) = 1 THEN valnr = valnr + STRING(aoint,"9").
         END.
      END.        
   END.
   CREATE AONRTAB.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = Cglobomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OMRADETAB THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.ELVOMRKOD = 0 USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   valnr = TRIM(valnr).
   ASSIGN
   AONRTAB.AUTOREG = TRUE
   AONRTAB.BESTID = OMRADETAB.OMRADE
   AONRTAB.OMRADE = OMRADETAB.OMRADE
   AONRTAB.FASTAAONR = RAD_FAST
   AONRTAB.AONR = valnr.
   aonrrec = RECID(AONRTAB).
  
END.
IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.      
edataapph = ?.
   
PROCEDURE nr_UI:
   DEFINE INPUT PARAMETER startv AS INTEGER.
   DEFINE INPUT PARAMETER slutv AS INTEGER.
   aoint = INTEGER(SUBSTRING(STRING(AONRTAB.AONR,"XXXXXX"),startv + 1,slutv - startv)) + 1.     
END PROCEDURE.
PROCEDURE nrkoll_UI:
   DEFINE INPUT PARAMETER startv AS INTEGER.
   DEFINE INPUT PARAMETER slutv AS INTEGER.
   DEFINE VARIABLE kollvar AS CHARACTER.
   IF slutv - startv = 5 THEN kollvar = "99999".
   IF slutv - startv = 4 THEN kollvar = "9999".
   IF slutv - startv = 3 THEN kollvar = "999".
   IF slutv - startv = 2 THEN kollvar = "99".
   IF slutv - startv = 1 THEN kollvar = "9".   
   IF SUBSTRING(STRING(AONRTAB.AONR,"XXXXXX"),startv + 1,slutv - startv) = kollvar  
   THEN valnr = "XXXXXX".      
END PROCEDURE.


PROCEDURE aopresista_UI :  
   DEFINE INPUT-OUTPUT PARAMETER tempvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER prevalnr AS CHARACTER NO-UNDO.
   DEFINE BUFFER aobuff FOR AONRTAB.   
   DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
   felkoll = FALSE.
   REPEAT:      
      FIND FIRST aobuff WHERE aobuff.AONR = tempvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE aobuff THEN DO:
         LEAVE.
      END.
      ELSE DO:
         tempvar = STRING(INTEGER(tempvar) + 1,"99999").         
         IF tempvar BEGINS prevalnr THEN.
         ELSE DO:
            felkoll = TRUE.
            LEAVE.
         END.
      END.                    
   END.              
   IF felkoll = FALSE THEN DO:   
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.
      ASSIGN           
      inextradatatemp.PROGRAM = "SISTNRPRE"                   
      inextradatatemp.HUVUDCH = prevalnr.   
      inextradatatemp.HUVUDINT = ?.              
      inextradatatemp.SOKCHAR[1] = tempvar.    
      /*inextradatatemp.SOKCHAR[1] = STRING(INTEGER(tempvar) + 1,"99999").     */ 
      RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
        
   END.   
END PROCEDURE.
