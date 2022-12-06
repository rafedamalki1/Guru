/*BYGGLEX.P*/
&Scoped-define NEW 
DEFINE SHARED VARIABLE valkonst  AS LOGICAL NO-UNDO.


 

{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}
{BYGGTEMP.I}
DEFINE INPUT PARAMETER TABLE FOR byggtemp.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE VARIABLE berlistapph AS HANDLE NO-UNDO.
DEFINE VARIABLE pnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE dnr AS INTEGER NO-UNDO.
DEFINE VARIABLE beredanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE blag AS CHARACTER NO-UNDO.
DEFINE VARIABLE extrarad AS INTEGER NO-UNDO.
DEFINE VARIABLE linhj1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE linhj2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE linuppdat1 AS LOGICAL NO-UNDO.
DEFINE VARIABLE linuppdat2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjcol AS CHARACTER NO-UNDO.
DEFINE VARIABLE bladraknare  AS INTEGER NO-UNDO.
DEFINE VARIABLE delraknare   AS INTEGER NO-UNDO.
DEFINE VARIABLE sidbrytvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE bernamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE ejmed AS LOGICAL NO-UNDO.
DEFINE VARIABLE baldut AS INTEGER NO-UNDO.
DEFINE VARIABLE delut AS INTEGER NO-UNDO.
DEFINE VARIABLE platsut AS INTEGER NO-UNDO.
DEFINE VARIABLE platsuthj AS INTEGER NO-UNDO.
DEFINE VARIABLE bladtotal AS INTEGER NO-UNDO.
baldut = baldut + 1.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE fnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE radraknare AS INTEGER NO-UNDO.
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE chCell                 AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COMPONENT-HANDLE.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
DEFINE VARIABLE sokpkod AS CHARACTER NO-UNDO.
fnamn = "BEREDPROTOKOLL.xlsX".
IF Guru.Konstanter:appcon THEN RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT bloblog).
ELSE RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT bloblog).
IF bloblog = TRUE THEN DO:
   {FINNSDYNBLOB.I}
   DEFINE VARIABLE resid AS INTEGER NO-UNDO.   
   RUN blobfil_UI IN blobproch (INPUT fnamn, OUTPUT resid).      
   IF resid = ? THEN kommando = SEARCH(fnamn).
   ELSE DO:      
      FIND FIRST blobinfotemp WHERE blobinfotemp.ID = resid NO-LOCK NO-ERROR.      
      RUN blobopen_UI IN blobproch (INPUT blobinfotemp.FILNAMN, OUTPUT kommando).      
   END.
   RUN deleteproc_UI IN blobproch.
END.
ELSE kommando = SEARCH(fnamn).   
IF kommando = ? THEN DO:
   MESSAGE "Hittade inte " fnamn VIEW-AS ALERT-BOX.
   RETURN.       
END. 
kommando2 = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
{SESSIONTEMPDIR.I}
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando2 = webclienttempdir.
OS-CREATE-DIR VALUE(kommando2) NO-ERROR.
IF Guru.GlobalaVariabler:plusaonr = "" OR Guru.GlobalaVariabler:plusaonr = ? THEN DO:
   fnamn = fnamn.
END.
ELSE DO:
   fnamn = TRIM (Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + fnamn.
END.
kommando2 = kommando2 + fnamn.
OS-COPY VALUE(kommando) VALUE(kommando2).
kommando = kommando2.
CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = TRUE.   
{OPENEXCEL.I}
IF fnamn NE "" THEN DO:
   chWorkbook = chExcelApplication:Workbooks:OPEN(kommando).
   chWorkSheet = chExcelApplication:Sheets:Item(1).   
   IF NOT  VALID-HANDLE(berlistapph) THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN BERLISTAPP.P PERSISTENT SET berlistapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN BERLISTAPP.P PERSISTENT SET berlistapph.
      END.
   END.   
   RUN start_UI.
   chWorkSheet = chExcelApplication:Sheets:Item(1).
   chWorkSheet:SELECT NO-ERROR.   
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
   RELEASE OBJECT chCell NO-ERROR.
   RELEASE OBJECT chWorkSheetRange NO-ERROR.   
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
   IF VALID-HANDLE(berlistapph) THEN DELETE PROCEDURE berlistapph NO-ERROR.
END.

PROCEDURE start_UI :   
   RUN hmtuppg_UI IN berlistapph (INPUT valaonr,INPUT valomrade, OUTPUT pnr , OUTPUT dnr , OUTPUT bernamn, OUTPUT blag).
   linhj1 = "".
   linhj2 = "".
   linuppdat1 = FALSE.
   linuppdat2 = FALSE.
   ASSIGN
   iColumn = 1
   cColumn = STRING(iColumn).
   cRange = "F" + cColumn.
   chWorkSheet:Range(cRange):Value = blag.
   cRange = "J" + cColumn.
   chWorkSheet:Range(cRange):Value = bernamn.
   ASSIGN
   iColumn = 2
   cColumn = STRING(iColumn).
   cRange = "B" + cColumn.
   chWorkSheet:Range(cRange):Value = valort.
   cRange = "I" + cColumn.
   chWorkSheet:Range(cRange):Value = Guru.Konstanter:gaok + ":".
   IF pnr NE "" THEN DO:
      cRange = "L" + cColumn.
      chWorkSheet:Range(cRange):Value = pnr + STRING(dnr).
   END.
   FIND LAST byggtempbuff USE-INDEX ORD NO-ERROR.
   bladraknare  = byggtempbuff.BLADNR.            
   REPEAT:
      ASSIGN 
      delut = delut + 1
      platsut = platsut + 1
      platsuthj = 20.
      extrarad = 13.
      IF baldut > bladraknare THEN LEAVE.      
      FOR EACH byggtemp WHERE byggtemp.BLADNR = baldut AND byggtemp.DELBLAD = delut:
         IF ( byggtemp.RADNR + extrarad ) > 53 AND platsut = 1 THEN DO:
            hjcol = "".                                           
            chWorkSheet:Rows(byggtemp.RADNR + extrarad):EntireRow:Insert NO-ERROR .            
            hjcol = "M" + STRING(byggtemp.RADNR + extrarad - 1).
            hjcol = hjcol + ":" + "M" + STRING(byggtemp.RADNR + extrarad - 1).
            chWorkSheet:Range(hjcol):Select.
            chExcelApplication:SELECTION():COPY().
            hjcol = "M" + STRING(byggtemp.RADNR + extrarad) + ":" + "M" + STRING(byggtemp.RADNR + extrarad).
            chWorkSheet:Range(hjcol):Select.                      
            chWorkSheet:PASTE().                       
         END.                            
         IF byggtemp.DELBLAD = 2 THEN DO:                         
            ASSIGN
            iColumn = byggtemp.RADNR + extrarad
            cColumn = STRING(iColumn).
            cRange = "A" + cColumn.
            chWorkSheet:Range(cRange):Value = CAPS(SUBSTRING(TRIM(byggtemp.BENAMNING),1,1)) + LC(SUBSTRING(TRIM(byggtemp.BENAMNING),2,30)) NO-ERROR.
            cRange = "L" + cColumn.
            chWorkSheet:Range(cRange):Value = "E" + byggtemp.ENR NO-ERROR.                      
         END.
         REPEAT:        
            ejmed = FALSE.    
            IF byggtemp.BYGG[platsut] = "" THEN. /*  byggtemp.BYGG[platsut] = "&nbsp;".*/
            IF byggtemp.DELBLAD = 1 THEN DO:
               IF byggtemp.RADNR = 1 THEN iColumn = 3.
               ELSE IF byggtemp.RADNR = 2 THEN iColumn = 4.
               ELSE IF byggtemp.RADNR = 5 THEN iColumn = 10.
               ELSE IF byggtemp.RADNR = 11 THEN iColumn = 9.
               ELSE DO:
                   ejmed = TRUE.
               END.                   
               cColumn = STRING(iColumn).
            END.
            ELSE IF byggtemp.DELBLAD = 2 THEN.               
            IF byggtemp.DELBLAD = 1 OR byggtemp.DELBLAD = 2 THEN DO:                
               IF ejmed = FALSE THEN DO:
                  IF byggtemp.DELBLAD = 1 THEN DO:
                     IF linuppdat1 = FALSE AND linhj1 NE "" THEN DO:                                                
                        cRange = "A" + STRING(12).
                        chWorkSheet:Range(cRange):Value = byggtemp.F2NAMN[platsut] NO-ERROR.
                        linuppdat1 = TRUE.
                     END.
                     IF linuppdat2 = FALSE AND linhj2 NE "" THEN DO:
                        IF TRIM(byggtemp.F5[platsut]) = "31" OR TRIM(byggtemp.F5[platsut]) = "62" OR TRIM(byggtemp.F5[platsut]) = "99" OR TRIM(byggtemp.F5[platsut]) = "Jordlina" 
                        OR TRIM(byggtemp.F5[platsut]) = "J-Lin 1" OR TRIM(byggtemp.F5[platsut]) = "J-Lin 2" OR TRIM(byggtemp.F5[platsut]) = "J-Lin 3" THEN DO:
                           cRange = "A" + STRING(13).
                           chWorkSheet:Range(cRange):Value = byggtemp.F5NAMN[platsut] NO-ERROR.
                           linuppdat2 = TRUE.
                        END.   
                     END.
                  END.                                           
                  IF platsut = 1 THEN DO:
                     cRange = "B" + cColumn.
                     chWorkSheet:Range(cRange):Value = byggtemp.BYGG[platsut] NO-ERROR.
                     IF byggtemp.DELBLAD = 1 AND byggtemp.RADNR = 1 THEN RUN lincheck_UI (INPUT "B").                      
                  END.   
                  ELSE IF platsut = 2 THEN DO:
                     cRange = "C" + cColumn.
                     chWorkSheet:Range(cRange):Value = byggtemp.BYGG[platsut] NO-ERROR.
                     IF byggtemp.DELBLAD = 1 AND byggtemp.RADNR = 1 THEN RUN lincheck_UI (INPUT "C").
                  END.   
                  ELSE IF platsut = 3 THEN DO:
                     cRange = "D" + cColumn.
                     chWorkSheet:Range(cRange):Value = byggtemp.BYGG[platsut] NO-ERROR.
                     IF byggtemp.DELBLAD = 1 AND byggtemp.RADNR = 1 THEN RUN lincheck_UI (INPUT "D").
                  END.   
                  ELSE IF platsut = 4 THEN DO:
                     cRange = "E" + cColumn.
                     chWorkSheet:Range(cRange):Value = byggtemp.BYGG[platsut] NO-ERROR.
                     IF byggtemp.DELBLAD = 1 AND byggtemp.RADNR = 1 THEN RUN lincheck_UI (INPUT "E").
                  END.   
                  ELSE IF platsut = 5 THEN DO:
                     cRange = "F" + cColumn.
                     chWorkSheet:Range(cRange):Value = byggtemp.BYGG[platsut] NO-ERROR.
                     IF byggtemp.DELBLAD = 1 AND byggtemp.RADNR = 1 THEN RUN lincheck_UI (INPUT "F").
                  END.   
                  ELSE IF platsut = 6 THEN DO:
                     cRange = "G" + cColumn.
                     chWorkSheet:Range(cRange):Value = byggtemp.BYGG[platsut] NO-ERROR.
                     IF byggtemp.DELBLAD = 1 AND byggtemp.RADNR = 1 THEN RUN lincheck_UI (INPUT "G").
                  END.   
                  ELSE IF platsut = 7 THEN DO:
                     cRange = "H" + cColumn.
                     chWorkSheet:Range(cRange):Value = byggtemp.BYGG[platsut] NO-ERROR.
                     IF byggtemp.DELBLAD = 1 AND  byggtemp.RADNR = 1 THEN RUN lincheck_UI (INPUT "H").
                  END.   
                  ELSE IF platsut = 8 THEN DO:
                     cRange = "I" + cColumn.
                     chWorkSheet:Range(cRange):Value = byggtemp.BYGG[platsut] NO-ERROR.
                     IF byggtemp.DELBLAD = 1 AND byggtemp.RADNR = 1 THEN RUN lincheck_UI (INPUT "I").
                  END.   
                  ELSE IF platsut = 9 THEN DO:
                     cRange = "J" + cColumn.
                     chWorkSheet:Range(cRange):Value = byggtemp.BYGG[platsut] NO-ERROR.
                     IF byggtemp.DELBLAD = 1 AND byggtemp.RADNR = 1 THEN RUN lincheck_UI (INPUT "J").
                  END.   
                  ELSE IF platsut = 10 THEN DO:
                     cRange = "K" + cColumn.
                     chWorkSheet:Range(cRange):Value = byggtemp.BYGG[platsut] NO-ERROR.
                     IF byggtemp.DELBLAD = 1 AND byggtemp.RADNR = 1 THEN RUN lincheck_UI (INPUT "K").
                  END.               
               END.
            END.               
            ASSIGN
            platsuthj = platsuthj + 6
            platsut = platsut + 1.
            IF platsut > 10 THEN DO:
               platsuthj = 20.
               platsut = 1.               
               LEAVE.
            END.
         END.         
      END.  
      platsut = 0.
      IF delut = 2 THEN DO:
         ASSIGN
         delut = 0
         platsut = 0
         baldut = baldut + 1.
         chWorkSheet = chExcelApplication:Sheets:Item(baldut).
         chWorkSheet:SELECT NO-ERROR.
         IF sidbrytvar = TRUE THEN DO:
            sidbrytvar = FALSE.                     
         END.
         
      END.
   END.
   
END PROCEDURE.

PROCEDURE lincheck_UI:
   DEFINE INPUT PARAMETER ccol AS CHARACTER NO-UNDO.
   
   IF byggtemp.GRUPP[platsut] = 19 OR byggtemp.GRUPP[platsut] = 20 OR byggtemp.GRUPP[platsut] = 21 OR byggtemp.GRUPP[platsut] = 22 THEN DO:
      DEBUGGER:SET-BREAK().
      IF TRIM(byggtemp.F2[platsut]) = "31" OR TRIM(byggtemp.F2[platsut]) = "62" OR TRIM(byggtemp.F2[platsut]) = "99" OR TRIM(byggtemp.F2[platsut]) = "157" OR TRIM(byggtemp.F2[platsut]) = "241" 
      OR TRIM(byggtemp.F2[platsut]) = "BLL62" OR TRIM(byggtemp.F2[platsut]) = "BLL99" OR TRIM(byggtemp.F2[platsut]) = "BLL157" OR TRIM(byggtemp.F2[platsut]) = "BLL241" 
      OR TRIM(byggtemp.F2[platsut]) = "BLX62" OR TRIM(byggtemp.F2[platsut]) = "BLX99" OR TRIM(byggtemp.F2[platsut]) = "BLX157" OR TRIM(byggtemp.F2[platsut]) = "BLX241" THEN DO:
         cRange = ccol + STRING(12).         
         chWorkSheet:Range(cRange):Value = "XXXX" NO-ERROR.
         linhj1 = TRIM(byggtemp.F2[platsut]).
      END.
      IF TRIM(byggtemp.F5[platsut]) = "31" OR TRIM(byggtemp.F5[platsut]) = "62" OR TRIM(byggtemp.F5[platsut]) = "99" OR TRIM(byggtemp.F5[platsut]) = "Jordlina" 
      OR TRIM(byggtemp.F5[platsut]) = "J-Lin 1" OR TRIM(byggtemp.F5[platsut]) = "J-Lin 2" OR TRIM(byggtemp.F5[platsut]) = "J-Lin 3" THEN DO:
         cRange = ccol + STRING(13).         
         chWorkSheet:Range(cRange):Value = "XXXX" NO-ERROR.
         linhj2 = TRIM(byggtemp.F5[platsut]).
      END.   
            
   END.   
END PROCEDURE.  


