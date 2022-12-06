&Scoped-define NEW NEW
&Scoped-define SHARED SHARED


{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE VARIABLE fltid AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE flextot LIKE TIDREGITAB.TOTALT NO-UNDO.
DEFINE VARIABLE kolldatum AS DATE NO-UNDO.
DEFINE VARIABLE lunorm AS INTEGER NO-UNDO.
DEFINE VARIABLE lufl AS INTEGER NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE varansf LIKE ANSTFORMTAB.KOD NO-UNDO. 
DEFINE VARIABLE plflex AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE  VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.



DEFINE VARIABLE fdatum AS DATE NO-UNDO.
/*DEFINE  TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".*/
DEFINE TEMP-TABLE ftemp   
   FIELD DATUM AS DATE
   FIELD DAG AS CHARACTER
   FIELD OSTART AS DECIMAL 
   FIELD OSLUT AS DECIMAL
   FIELD FSTART AS DECIMAL 
   FIELD FSLUT AS DECIMAL 
   FIELD MKFLEX AS INTEGER
   FIELD MKTFLEX AS DECIMAL
   FIELD TFLEX AS INTEGER
   FIELD TOTFLEX AS DECIMAL
   FIELD LFLEX AS DECIMAL
   FIELD MFLEX AS DECIMAL
   FIELD KFLEX AS DECIMAL.

DEFINE TEMP-TABLE flut   
   FIELD PERSONALKOD AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD OMRADETAB AS CHARACTER
   FIELD DATUM AS DATE
   FIELD DAG AS CHARACTER
   FIELD LFLEX AS DECIMAL 
   FIELD MKTFLEX AS DECIMAL
   FIELD TOTFLEX AS DECIMAL. 
    
/*OUTPUT TO C:\LULFLEX2014.TXT.*/
CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = FALSE NO-ERROR.
chWorkbook = chExcelApplication:Workbooks:Add() NO-ERROR.
chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.
 
chWorkSheet:Columns("A"):ColumnWidth = 11 NO-ERROR.
chWorkSheet:Columns("B"):ColumnWidth = 30 NO-ERROR.
chWorkSheet:Columns("C"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("D"):ColumnWidth = 10 NO-ERROR.
chWorkSheet:Columns("E"):ColumnWidth = 5 NO-ERROR.
chWorkSheet:Columns("F"):ColumnWidth = 15 NO-ERROR.
chWorkSheet:Columns("G"):ColumnWidth = 15 NO-ERROR.
chWorkSheet:Columns("H"):ColumnWidth = 15 NO-ERROR.

chWorkSheet:Range("A1:H1"):Font:Bold = TRUE NO-ERROR.

chWorkSheet:Range("A1"):Value = "Enhet" NO-ERROR.
chWorkSheet:Range("B1"):Value = "Namn" NO-ERROR.
chWorkSheet:Range("C1"):Value = "Omrade" NO-ERROR.
chWorkSheet:Range("D1"):Value = "Datum" NO-ERROR.
chWorkSheet:Range("E1"):Value = "Dag" NO-ERROR.
chWorkSheet:Range("F1"):Value = "Lunchflex" NO-ERROR.
chWorkSheet:Range("G1"):Value = "Morgon/kväll" NO-ERROR.
chWorkSheet:Range("H1"):Value = "Totflex" NO-ERROR.
chWorkSheet:Range("G:G"):NumberFormat = "@" NO-ERROR.
iColumn = 2.   
FIND FIRST FORETAG  NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.

FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE USE-INDEX PERSONALKOD NO-LOCK:
   FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD AND FLEXAVT.FLEXTID = TRUE NO-LOCK NO-ERROR.
   IF AVAILABLE  FLEXAVT THEN DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
      TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      YEAR(TIDREGITAB.DATUM) = 2014   USE-INDEX PSTART NO-LOCK.
   
      IF   globforetag = "LULE" THEN DO:
         persrec = RECID(PERSONALTAB).
         
         FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING       USE-INDEX ANSTF NO-LOCK NO-ERROR.
         ASSIGN 
         varansf = ANSTFORMTAB.KOD.
        
         IF globforetag = "LULE"  THEN DO:
            EMPTY TEMP-TABLE ftemp NO-ERROR. 
            fltid = 0.
            fdatum = ?.
            GET FIRST tidq NO-LOCK.
            DO WHILE AVAILABLE (TIDREGITAB):               
               IF TIDREGITAB.TIDLOG = FALSE THEN.
               ELSE IF TIDREGITAB.OVERTIDUTTAG NE "F" THEN .
               ELSE IF TIDREGITAB.PRISTYP = "RESTID..." THEN.
               ELSE IF TIDREGITAB.DATUM NE fdatum THEN DO:                  
                  regdatum = TIDREGITAB.DATUM.
                  regvnr = TIDREGITAB.VECKONUMMER.
                  RUN SLUTARB.P.            
                  CREATE ftemp.
                  ASSIGN
                  ftemp.DAG = TIDREGITAB.DAG
                  ftemp.DATUM = TIDREGITAB.DATUM
                  ftemp.OSTART = regstart
                  ftemp.OSLUT = regslut
                  ftemp.FSTART = TIDREGITAB.START
                  ftemp.FSLUT = TIDREGITAB.SLUT
                  fdatum = TIDREGITAB.DATUM.
               END.
               ELSE DO:
                  FIND FIRST ftemp WHERE FTEMP.DATUM = TIDREGITAB.DATUM NO-ERROR.                  
                  ASSIGN
                  ftemp.FSLUT = TIDREGITAB.SLUT.
               END.
               GET NEXT tidq NO-LOCK.
            END.
         END.
         
         IF globforetag = "LULE"  THEN DO:               
            
            ASSIGN
            fltid = 0
            kolldatum = ?
            lufl = 0.
            GET FIRST tidq NO-LOCK.
            DO WHILE AVAILABLE (TIDREGITAB):                  
               IF TIDREGITAB.TIDLOG = FALSE THEN .                  
               ELSE DO:                  
                  IF globforetag = "LULE"  THEN DO:                                                                                             
                     IF kolldatum NE TIDREGITAB.DATUM THEN DO:
                        FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                        IF AVAILABLE ftemp THEN DO:                              
                           IF ftemp.FSTART > ftemp.OSTART AND ftemp.OSTART NE ftemp.OSLUT THEN DO:
                              nytid = ftemp.FSTART.
                              RUN TIMSEK.P.
                              seku = sekunder.
                              nytid = ftemp.OSTART.
                              RUN TIMSEK.P.
                              sekunder = sekunder - seku.
                              RUN FSEKTIM.P.                               
                              fltid = fltid + sekunder.
                              ftemp.MKFLEX = ftemp.MKFLEX + sekunder.
                              ftemp.TFLEX = ftemp.TFLEX + sekunder.                                 
                           END.
                        END.
                     END.
                  END.
                  IF TIDREGITAB.AONR = "155" THEN DO:                          
                     regdatum = TIDREGITAB.DATUM.
                     regvnr = TIDREGITAB.VECKONUMMER.
                     RUN SLUTARB.P.                                          
                     IF TIDREGITAB.START GE regslut THEN.
                     ELSE DO:                                                
                        nytid = TIDREGITAB.TOTALT.
                        RUN TIMSEK.P.
                        fltid = fltid - sekunder.
                        FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                        IF AVAILABLE ftemp THEN DO:
                           ftemp.MKFLEX = ftemp.MKFLEX - sekunder.
                           ftemp.TFLEX = ftemp.TFLEX - sekunder.                              
                        END.
                     END.   
                  END.                   
                  ELSE IF TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.LONTILLAGG = "" THEN DO:
                     regdatum = TIDREGITAB.DATUM.
                     regvnr = TIDREGITAB.VECKONUMMER.
                     RUN SLUTARB.P.                                       
                     FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = TIDREGITAB.DATUM AND 
                     OVERAVTAB.KOD = varansf USE-INDEX ODATUM  NO-LOCK NO-ERROR.                  
                     IF WEEKDAY(TIDREGITAB.DATUM) = 1 OR WEEKDAY(TIDREGITAB.DATUM) = 7
                     THEN DO:                           
                        nytid = TIDREGITAB.TOTALT.
                        RUN TIMSEK.P.
                        fltid = fltid + sekunder. 
                        FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                        IF AVAILABLE ftemp THEN DO:
                           ftemp.MKFLEX = ftemp.MKFLEX + sekunder.
                           ftemp.TFLEX = ftemp.TFLEX + sekunder.                              
                        END.
                     END.      
                     ELSE IF AVAILABLE OVERAVTAB AND ( OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7) THEN DO:                           
                        nytid = TIDREGITAB.TOTALT.
                        RUN TIMSEK.P.
                        fltid = fltid + sekunder.                     
                        FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                        IF AVAILABLE ftemp THEN DO:
                           ftemp.MKFLEX = ftemp.MKFLEX + sekunder.
                           ftemp.TFLEX = ftemp.TFLEX + sekunder.                              
                        END.
                     END.     
                     ELSE DO: 
                        IF regstart = regslut THEN DO:
                           nytid = TIDREGITAB.TOTALT.
                           RUN TIMSEK.P.
                           fltid = fltid + sekunder.                     
                           FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                           IF AVAILABLE ftemp THEN DO:
                              ftemp.MKFLEX = ftemp.MKFLEX + sekunder.
                              ftemp.TFLEX = ftemp.TFLEX + sekunder.                              
                           END.
                        END.
                        ELSE DO:                           
                           IF TIDREGITAB.START < regstart THEN DO:                                                                  
                              IF TIDREGITAB.SLUT > regstart THEN DO:                                 
                                 nytid = TIDREGITAB.START.
                                 RUN TIMSEK.P.
                                 seku = sekunder.
                                 nytid = regstart.
                                 RUN TIMSEK.P.
                                 sekunder = sekunder - seku.
                                 RUN SEKTIM.P.                                 
                                 plflex = nytid.
                              END.
                              ELSE plflex = TIDREGITAB.TOTALT.                                                            
                              nytid = plflex.
                              RUN TIMSEK.P.
                              fltid = fltid + sekunder.
                              FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                              IF AVAILABLE ftemp THEN DO:
                                 ftemp.MKFLEX = ftemp.MKFLEX + sekunder.
                                 ftemp.TFLEX = ftemp.TFLEX + sekunder.                                 
                              END.
                           END.
                           IF TIDREGITAB.SLUT > regslut THEN DO:                              
                              IF TIDREGITAB.START < regslut THEN DO:                               
                                 nytid = TIDREGITAB.SLUT.
                                 RUN TIMSEK.P.
                                 seku = sekunder.
                                 nytid = regslut.
                                 RUN TIMSEK.P.
                                 sekunder = seku - sekunder.
                                 RUN SEKTIM.P.                               
                                 plflex = nytid.
                              END.
                              ELSE plflex = TIDREGITAB.TOTALT.                              
                              flextot = plflex.                                                                                
                              nytid = flextot.
                              RUN TIMSEK.P.
                              fltid = fltid + sekunder.
                              FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                              IF AVAILABLE ftemp THEN DO:
                                 ftemp.MKFLEX = ftemp.MKFLEX + sekunder.
                                 ftemp.TFLEX = ftemp.TFLEX + sekunder.                                 
                              END.
                           END.
                        END.
                     END.
                  END.                                    
                  IF globforetag = "LULE"  THEN DO:                           
                     musz = FALSE.                        
                     IF kolldatum NE TIDREGITAB.DATUM THEN DO:
                         ASSIGN
                         lufl = 0
                         musz = TRUE.
                     END.
                     ELSE IF kolldatum = TIDREGITAB.DATUM AND lufl = 0 THEN musz = TRUE.                        
                     IF musz = TRUE THEN DO:
                        musz = FALSE.
                        regdatum = TIDREGITAB.DATUM.
                        regvnr = TIDREGITAB.VECKONUMMER.
                        RUN SLFLARB.P.
                        IF TIDREGITAB.START < regslut AND TIDREGITAB.SLUT > regstart THEN DO:                           
                            nytid = lunchslutet.
                            RUN TIMSEK.P.
                            seku = sekunder.
                            nytid = lunchstarten.
                            RUN TIMSEK.P.                                                     
                            lunorm = (seku - sekunder) / 60.
                            IF TIDREGITAB.LAGANTAL > 0 AND TIDREGITAB.LAGANTAL < lunorm THEN DO:                              
                               sekunder = (lunorm - TIDREGITAB.LAGANTAL) * 60.
                               RUN SEKTIM.P.                                                                                           
                               fltid = fltid + sekunder.
                               lufl = 1.
                               FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                               IF AVAILABLE ftemp THEN DO:
                                  ftemp.LFLEX = nytid.
                                  ftemp.TFLEX = ftemp.TFLEX + sekunder.                                     
                               END.
                            END.
                            IF TIDREGITAB.LAGANTAL > lunorm THEN DO:                              
                               sekunder = (TIDREGITAB.LAGANTAL - lunorm ) * 60.
                               RUN SEKTIM.P.                                                                                      
                               fltid = fltid - sekunder.
                               lufl = 1.
                               FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                               IF AVAILABLE ftemp THEN DO:
                                  ftemp.LFLEX = 0 - nytid.
                                  ftemp.TFLEX = ftemp.TFLEX - sekunder.                                     
                               END.
                            END.                                           
                        END.
                     END.
                     IF globforetag = "LULE"  THEN DO:                                                                     
                        IF kolldatum NE TIDREGITAB.DATUM THEN DO:
                           FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                           IF AVAILABLE ftemp THEN DO:                                 
                              IF ftemp.FSLUT < ftemp.OSLUT THEN DO:
                                 nytid = ftemp.FSLUT.
                                 RUN TIMSEK.P.
                                 seku = sekunder.
                                 nytid = ftemp.OSLUT.
                                 RUN TIMSEK.P.
                                 sekunder = seku - sekunder.
                                 RUN FSEKTIM.P.                                    
                                 fltid = fltid + sekunder.
                                 FIND FIRST ftemp WHERE ftemp.DATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
                                 IF AVAILABLE ftemp THEN DO:
                                    ftemp.MKFLEX = ftemp.MKFLEX + sekunder.
                                    ftemp.TFLEX = ftemp.TFLEX + sekunder.                                       
                                 END.
                              END.
                           END.
                        END.
                     END.
                     kolldatum = TIDREGITAB.DATUM.                        
                  END.                
               END.
               GET NEXT tidq.
            END.  
            FOR EACH ftemp: 
               sekunder = ftemp.MKFLEX.
               RUN FSEKTIM.P.
               ASSIGN ftemp.MKTFLEX = fnytid.                  
               sekunder = ftemp.TFLEX.
               RUN FSEKTIM.P.
               ASSIGN ftemp.TOTFLEX = fnytid.
            END.            
            FOR EACH ftemp: 
               IF ftemp.TOTFLEX = 0 THEN nytid = nytid.
               ELSE DO:       
                  IF ftemp.LFLEX =  0 AND ftemp.MKTFLEX = 0 AND ftemp.TOTFLEX = 0 THEN.
                  ELSE DO:
                     CREATE flut.
                     ASSIGN   
                     flut.PERSONALKOD = PERSONALTAB.PERSONALKOD
                     flut.NAMN = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN
                     flut.OMRADE = PERSONALTAB.OMRADE
                     flut.DATUM = ftemp.datum
                     flut.DAG = ftemp.DAG
                     flut.LFLEX = ftemp.LFLEX 
                     flut.MKTFLEX = ftemp.MKTFLEX
                     flut.TOTFLEX = ftemp.TOTFLEX.
                     iColumn = iColumn + 1.
                     cColumn = STRING(iColumn) NO-ERROR.
                     cRange = "A" + cColumn.                     
                     chWorkSheet:Range(cRange):Value = FLUT.PERSONALKOD NO-ERROR.
                     cRange = "B" + cColumn.
                     chWorkSheet:Range(cRange):Value = FLUT.NAMN NO-ERROR.
                     cRange = "C" + cColumn.
                     chWorkSheet:Range(cRange):Value = FLUT.OMRADE NO-ERROR.
                     cRange = "D" + cColumn.
                     chWorkSheet:Range(cRange):Value = FLUT.DATUM NO-ERROR.
                     cRange = "E" + cColumn.
                     chWorkSheet:Range(cRange):Value = FLUT.DAG NO-ERROR.
                     cRange = "F" + cColumn.
                     chWorkSheet:Range(cRange):Value = FLUT.LFLEX NO-ERROR.
                     cRange = "G" + cColumn.
                     chWorkSheet:Range(cRange):Value = FLUT.MKTFLEX NO-ERROR.                     
                     cRange = "H" + cColumn.
                     chWorkSheet:Range(cRange):Value = FLUT.TOTFLEX NO-ERROR.
                     
                  END.              
                 
               END.
            END.
         END.
         
       END.
   END.

  
END.
chExcelApplication:displayalerts = FALSE.      
chWorkbook:SaveAs("C:\FLEXSAMLULE2014.XLSX",,,,,,,,,).        
chExcelApplication:displayalerts = TRUE.

RELEASE OBJECT chWorksheetRange NO-ERROR.             
/*RELEASE OBJECT cActiveCell NO-ERROR.*/      
RELEASE OBJECT chWorksheet NO-ERROR.                  
NO-RETURN-VALUE chWorkbook:CLOSE().
NO-RETURN-VALUE chExcelApplication:QUIT().
RELEASE OBJECT chWorkbook NO-ERROR.    
RELEASE OBJECT chExcelApplication NO-ERROR.           
/*RELEASE OBJECT chWindow NO-ERROR.*/


/*OUTPUT TO C:\LULFLEX2014.TXT.
FOR EACH flut:
   EXPORT FLUT.
END.*/
OUTPUT CLOSE.   