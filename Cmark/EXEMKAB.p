/*EXEMKAB.P*/
                              
                              

DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
DEFINE VARIABLE markag AS INTEGER NO-UNDO.
DEFINE VARIABLE extrarader AS INTEGER NO-UNDO.
DEFINE VARIABLE extrarader2 AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE extraraderinnan AS INTEGER NO-UNDO.
DEFINE VARIABLE extram AS INTEGER NO-UNDO.
DEFINE VARIABLE emg AS INTEGER NO-UNDO.
DEFINE VARIABLE uColumn  AS INTEGER INITIAL 0.
DEFINE VARIABLE fnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE link AS CHARACTER NO-UNDO.
DEFINE VARIABLE linkumea AS CHARACTER NO-UNDO.
DEFINE VARIABLE bytcol AS LOGICAL NO-UNDO.
DEFINE VARIABLE bytacol AS CHARACTER NO-UNDO.
DEFINE VARIABLE bytbcol AS CHARACTER NO-UNDO.
DEFINE VARIABLE bytccol AS CHARACTER NO-UNDO.
DEFINE VARIABLE bytgcol AS CHARACTER NO-UNDO.
DEFINE VARIABLE bythcol AS CHARACTER NO-UNDO.

DEFINE VARIABLE valvardnr AS INTEGER NO-UNDO.
DEFINE VARIABLE omravd AS INTEGER NO-UNDO.
DEFINE VARIABLE aovar AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE ortvar AS CHARACTER NO-UNDO.   
DEFINE VARIABLE fkommun AS CHARACTER NO-UNDO.   
DEFINE VARIABLE fvaker AS CHARACTER NO-UNDO.   
DEFINE VARIABLE arendator AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
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
DEFINE VARIABLE radnrS                  AS CHARACTER.
DEFINE VARIABLE pristext                AS CHARACTER.
DEFINE VARIABLE skprotapph AS HANDLE NO-UNDO.
DEFINE VARIABLE radins AS INTEGER NO-UNDO.
DEFINE VARIABLE radstreck AS INTEGER NO-UNDO.
DEFINE VARIABLE radnamn AS INTEGER NO-UNDO.
DEFINE VARIABLE sisnamn AS INTEGER NO-UNDO.

DEFINE VARIABLE ledagare AS CHARACTER NO-UNDO.
DEFINE VARIABLE agfast AS CHARACTER NO-UNDO.
DEFINE VARIABLE agkommun AS CHARACTER NO-UNDO.
DEFINE VARIABLE aglan AS CHARACTER NO-UNDO.
DEFINE VARIABLE agorg AS CHARACTER NO-UNDO.
DEFINE VARIABLE gforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE lelitt AS CHARACTER NO-UNDO.
DEFINE VARIABLE pnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE pled AS CHARACTER NO-UNDO.
DEFINE VARIABLE konc AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE domsaga AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE nstn AS CHARACTER NO-UNDO.
DEFINE VARIABLE par1a AS INTEGER NO-UNDO.
DEFINE VARIABLE par1b AS INTEGER NO-UNDO.
DEFINE VARIABLE par2c AS INTEGER NO-UNDO.
DEFINE VARIABLE kabelmsvut1  AS INTEGER NO-UNDO.
DEFINE VARIABLE kabelmsvut2  AS INTEGER NO-UNDO.


DEFINE INPUT PARAMETER ponr AS CHARACTER FORMAT "X(12)" NO-UNDO.
DEFINE INPUT PARAMETER vdok AS CHARACTER  NO-UNDO.


{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED  
{MARKVAL.I}                         
DEFINE BUFFER markvalbuff FOR markval.
{MARAG.I}

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}


gforetag = Guru.Konstanter:globforetag.
IF vdok = "KAB" OR vdok = "KABUE" THEN DO:
   IF gforetag = "UMEA"  THEN fnamn = "KABELMVSUT.XLS".
   ELSE IF gforetag = "UMBR" THEN fnamn = "KABELMVSUTB.XLS".   
   ELSE fnamn = "KABELMVS.XLS".
END.
/*ELSE IF vdok = "KABBR" THEN DO:
   /*enbart test UME� ENERGI SOM ANV�NDSER BREDBANDS MALL*/
   fnamn = "KABELMVSUTB.XLS".      
END.*/
ELSE IF vdok = "KABUBOD" OR vdok = "KABUBODUE" THEN DO:
   IF gforetag = "UMBR" THEN fnamn = "KABELMVSUTBU.XLS".   
END.
ELSE IF vdok = "KABLUFT" OR vdok = "KABLUFTUE" THEN DO:
   IF gforetag = "UMEA" THEN fnamn = "MUAKabelLuft.XLS".
   IF gforetag = "UMBR" THEN fnamn = "MUAKabelLuftB.XLS".   
END.
ELSE IF vdok = "KABLSP"  THEN DO:
   fnamn = "KABELMVSUTlsp.XLS".   
END.
ASSIGN    
kabelmsvut1 = 137
kabelmsvut2 = 142.
IF fnamn = "MUAKabelLuft.XLS" THEN DO:
   ASSIGN 
   kabelmsvut1 = 140
   kabelmsvut2 = 145.
END.
IF fnamn = "MUAKabelLuftB.XLS" THEN DO:
   ASSIGN 
   kabelmsvut1 = 141
   kabelmsvut2 = 146.
END.    

 

IF Guru.Konstanter:appcon THEN RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT bloblog).
ELSE RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT bloblog).
IF bloblog = TRUE THEN DO:
   {FINNSDYNBLOB.I} 
   DEFINE VARIABLE resid AS INTEGER NO-UNDO.
   RUN blobfil_UI IN blobproch (INPUT fnamn, OUTPUT resid).
   IF resid = ? THEN DO:
      kommando = SEARCH(fnamn).      
   END.
   ELSE DO:
      FIND FIRST blobinfotemp WHERE blobinfotemp.ID = resid NO-LOCK NO-ERROR.      
      RUN blobopen_UI IN blobproch (INPUT blobinfotemp.FILNAMN, OUTPUT kommando).      
   END.
   RUN deleteproc_UI IN blobproch.
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
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
   kommando2 = kommando2 + fnamn.
END.
ELSE DO:
   kommando2 = kommando2 + TRIM (Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + fnamn.
END.

OS-COPY VALUE(kommando) VALUE(kommando2).
kommando = kommando2.
IF gforetag = "UMEA" OR gforetag = "UMBR" THEN bytcol = TRUE.
ELSE  bytcol = FALSE.
IF bytcol = TRUE THEN DO:
   ASSIGN
   bytacol = "B"
   bytbcol = "C"
   bytccol = "D"
   bytgcol = "H"
   bythcol = "I".
END.
ELSE DO:
   ASSIGN
   bytacol = "A"
   bytbcol = "B"
   bytccol = "C"
   bytgcol = "G"
   bythcol = "H".
END.

/* IF INDEX(kommando,"GURU") = 0 THEN DO:  */

/*    kommando = wtidvar + "kabelmvs.xls". */
/* END.                                    */
/*    kommando = SEARCH("kabelmvs.xls").                        */
/*    IF kommando = ? THEN DO:                                  */
/*       MESSAGE "Hittade inte kabelmvs.xls" VIEW-AS ALERT-BOX. */
/*       RETURN.                                                */
/*    END.                                                      */
   /*IF gforetag = "CELPA" THEN kommando = "\\PC112\delad\pro9\guru\wmark\kabelmvs.xls".*/ 
   markag = 0.   
   FOR EACH markval NO-LOCK.
      markag = markag + 1.
      IF markag = 1 THEN DO:
         IF markval.PNR2 BEGINS "0000" THEN extram = 0.
         ELSE extram = 1.
      END.      
   END.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKAPPROTOU7.P PERSISTENT SET skprotapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SKAPPROTOU7.P PERSISTENT SET skprotapph.
   END.
   FIND FIRST markval USE-INDEX MARKNR NO-LOCK NO-ERROR.  
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN EXKABAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT markval.VARDNR,INPUT markval.BETECKNING,OUTPUT omravd,OUTPUT aovar,OUTPUT delnrvar,OUTPUT ortvar,OUTPUT fkommun,OUTPUT fvaker).
   END.
   ELSE DO:
      RUN EXKABAPP.P 
      (INPUT markval.VARDNR,INPUT markval.BETECKNING,OUTPUT omravd,OUTPUT aovar,OUTPUT delnrvar,OUTPUT ortvar,OUTPUT fkommun,OUTPUT fvaker).
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
     RUN EXTRADATAHMT.P PERSISTENT SET edataapph.  
   END.
   IF ponr NE ? AND ponr NE ""  THEN DO:
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "NATPROJ"                   
      inextradatatemp.HUVUDCH = ponr.                    
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:  
         IF Guru.Konstanter:varforetypchar[7] = "1" THEN DO:
            ASSIGN       
            ledagare = extradatatemp.SOKCHAR[6] 
            agfast = extradatatemp.SOKCHAR[7] 
            agkommun = extradatatemp.SOKCHAR[8] 
            aglan = extradatatemp.SOKCHAR[9]
            agorg = extradatatemp.SOKCHAR[10].
         END.
         ELSE DO:
            ASSIGN               
            lelitt = extradatatemp.SOKCHAR[1] 
            pnamn = extradatatemp.SOKCHAR[2] 
            pled = extradatatemp.SOKCHAR[3] 
            konc = extradatatemp.SOKCHAR[4] 
           /* domsaga = extradatatemp.SOKCHAR[5]*/
            nstn = extradatatemp.SOKCHAR[9].
         END.
                
      END.
   END.      


   FIND FIRST markval USE-INDEX MARKNR NO-LOCK NO-ERROR.  
   RUN arrendat_UI IN skprotapph (INPUT markval.BETECKNING,OUTPUT arendator).
   RUN maka_UI IN skprotapph (INPUT TABLE markval,OUTPUT TABLE marag).
   FIND FIRST markval USE-INDEX MARKNR NO-LOCK NO-ERROR.  
   ASSIGN markval.AONR = aovar
   markval.DELNR = delnrvar.
   
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   /*
   chWorkbook = chExcelApplication:Workbooks:OPEN(kommando).
    */                    
   {OPENEXCEL.I}
   chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.
   chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.
   link = ?.
   /*FOREBILDER*/
   {LOGGOR.I}      
   IF gforetag = "vast" OR gforetag = "vELD" OR gforetag = "elpa" THEN.
   ELSE IF gforetag = "pica" THEN.
   ELSE IF gforetag = "UMEA" OR gforetag = "UMBR"  THEN DO:
      IF ponr = "umea" THEN DO:      
         linkumea = link. 
         IF Guru.Konstanter:appcon THEN DO:
            RUN LOGGORIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT gforetag, INPUT Guru.Konstanter:gurubilder,OUTPUT link).
         END.
         ELSE DO:
            RUN LOGGORIN.P (INPUT gforetag, INPUT Guru.Konstanter:gurubilder,OUTPUT link).
         END.
        
         IF link NE ? THEN RUN imageexcel_UI.
         link = linkumea.
      END.

   END.   
   ELSE IF link NE ? THEN RUN imageexcel_UI.
   {EXCELFEL.I}
   
   IF gforetag = "UMEA" OR gforetag = "UMBR" OR gforetag = "cELPA" THEN DO:   
      /*IF ortvar NE "" THEN DO:      
         iColumn = 4.
         cColumn = STRING(iColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Value =  ortvar NO-ERROR.  
      END.*/
      iColumn = 5.
      cColumn = STRING(iColumn).
      cRange = "E" + cColumn.
      
      chWorkSheet:Range(cRange):Value =  markval.VARDNR NO-ERROR.
      
      iColumn = 4.
      cColumn = STRING(iColumn).  
      IF ponr NE "" THEN DO:               
         cRange = "B" + cColumn.
         chWorkSheet:Range(cRange):Value =  ponr NO-ERROR.  
      END.
      IF pnamn NE "" THEN DO:               
         cRange = "D" + cColumn.
         chWorkSheet:Range(cRange):Value =  pnamn NO-ERROR.  
      END.
      IF pled NE "" THEN DO:               
         cRange = "G" + cColumn.
         chWorkSheet:Range(cRange):Value =  pled NO-ERROR.  
      END.
      IF lelitt NE "" THEN DO:               
         cRange = "J" + cColumn.
         chWorkSheet:Range(cRange):Value =  lelitt NO-ERROR.  
      END.
      
      
   END.
   ELSE DO:   
      IF ponr NE ? THEN DO:
         iColumn = 1.
         cColumn = STRING(iColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Value = chWorkSheet:Range(cRange):Value + "     " + ponr NO-ERROR.  
      END.      
      IF ortvar NE "" THEN DO:      
         iColumn = 2.
         cColumn = STRING(iColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Value =  ortvar NO-ERROR.  
      END.
      iColumn = 3.
      cColumn = STRING(iColumn).
      cRange = bytccol + cColumn.
      
      chWorkSheet:Range(cRange):Value =  markval.VARDNR NO-ERROR.  
   END.
   {EXCELFEL.I}
   iColumn = 12.
   cColumn = STRING(iColumn).
   cRange = bytacol + cColumn.
   chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
   chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   FIND FIRST marag WHERE marag.PERSONNUMMER =  markval.PERSONNUMMER NO-LOCK NO-ERROR.
   chWorkSheet:Range(cRange):Value = STRING(markval.PERSONNUMMER,"999999-9999") + " " +  marag.MARKAGARE NO-ERROR.  
   
   IF gforetag = "VAST"  OR gforetag = "VELD" OR gforetag = "celpa" THEN DO:       
         iColumn = 12.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):Value = agorg NO-ERROR.
      
         iColumn = 13.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):Value = ledagare NO-ERROR.   
   END.
   IF gforetag = "UMEA"  THEN DO:
             
      iColumn = 12.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
      chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "UME� ENERGI ELN�T AB" NO-ERROR.               
      iColumn = 15.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
      chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      /*�ndrat 20200212 Stefan L Lena
      chWorkSheet:Range(cRange):Value = "LAXEN 31" NO-ERROR.*/
      chWorkSheet:Range(cRange):Value = "Norrfors 3:18" NO-ERROR.     
                
      iColumn = 17.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
      chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "UME�" NO-ERROR.               
      iColumn = 19.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
      chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "V�STERBOTTEN" NO-ERROR.
                      
   END.
   IF gforetag = "UMBR" THEN DO:      
      IF vdok = "KABUE" OR vdok = "KABUBODUE" OR vdok = "KABLUFTUE" THEN DO:
         iColumn = 12.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):Value = "UME� ENERGI AB" NO-ERROR.
         iColumn = 13.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):Value = "556097-8602" NO-ERROR.
         iColumn = 15.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):Value = "  " NO-ERROR.
         /*INLAGT 202000302 Stefan L Lena     
         fastighet borttagen 20201019 Stefan L Lena */
         /*chWorkSheet:Range(cRange):Value = "Stj�rnbilden 13 " NO-ERROR.*/   
         iColumn = 20.
         cColumn = STRING(iColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 8 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
         chWorkSheet:Range(cRange):Value = "Ume� Energi AB �r personuppgiftsansvarig f�r personuppgifter som l�mnas av respektive mark�gare vid avtalstecknandet. Ume� Energi-koncernen behandlar personuppgifter enligt g�llande dataskyddslagstiftning. Mer information finns  p� Ume� Energis webb: www.umeaenergi.se/integritetspolicy " NO-ERROR.
         
      END.
      ELSE DO:    
         iColumn = 12.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):Value = "UME� ENERGI UMENET AB" NO-ERROR.
         iColumn = 13.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):Value = "556619-3057" NO-ERROR.
         iColumn = 15.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         /*INLAGT 202000302 Stefan L Lena      */
         chWorkSheet:Range(cRange):Value = "Stj�rnbilden 13 " NO-ERROR.
         iColumn = 20.
         cColumn = STRING(iColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 8 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
         chWorkSheet:Range(cRange):Value = "Ume� Energi UmeNet AB �r personuppgiftsansvarig f�r personuppgifter som l�mnas av respektive mark�gare vid avtalstecknandet. Ume� Energi-koncernen behandlar personuppgifter enligt g�llande dataskyddslagstiftning. Mer information finns  p� Ume� Energis webb: www.umeaenergi.se/integritetspolicy " NO-ERROR.               
      END.               
      iColumn = 17.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
      chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "UME�" NO-ERROR.               
      iColumn = 19.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
      chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "V�STERBOTTEN" NO-ERROR.               
   END.
   {EXCELFEL.I}
   IF gforetag = "LECA" THEN DO:       
      iColumn = 12.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
      chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "PSB och Teknik" NO-ERROR.                     
   END.
   IF markval.PNR2 BEGINS "0000" THEN gforetag = gforetag.
   ELSE DO:   
      iColumn = 13.
      cColumn = STRING(iColumn).
      cRange = bytacol + cColumn.
      chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
      chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      FIND FIRST marag WHERE marag.PERSONNUMMER =  markval.PNR2 NO-LOCK NO-ERROR.
      chWorkSheet:Range(cRange):Value = STRING(markval.PNR2,"999999-9999")  + " " +  marag.MARKAGARE NO-ERROR. 
               
   END.

   extrarader = 0.
   extrarader2 = 0.
   DEBUGGER:SET-BREAK().
   IF gforetag = "UMEA" OR gforetag = "UMBR" THEN extrarader2 = 11.
   ELSE extrarader2 = -4.
   iColumn = kabelmsvut1  + extrarader2.
   /*iColumn = 137  + extrarader2.*/
   
   cColumn = STRING(iColumn).
   cRange = bytacol + cColumn.
   chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
   chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.      
   FIND FIRST marag WHERE marag.PERSONNUMMER =  markval.PERSONNUMMER NO-LOCK NO-ERROR.
   IF INTEGER(SUBSTRING(marag.PERSONNUMMER,3,2)) GE 20 THEN DO:
       chWorkSheet:Range(cRange):Value = "Ledamot f�r " + marag.MARKAGARE NO-ERROR.
   END.
   ELSE  chWorkSheet:Range(cRange):Value = marag.MARKAGARE NO-ERROR.  
   sisnamn = iColumn.
   {EXCELFEL.I}
   IF markval.PNR2 BEGINS "0000" THEN gforetag = gforetag.
   ELSE DO:   
      
      iColumn = kabelmsvut2  + extrarader2.
      /*iColumn = 142  + extrarader2.*/
      cColumn = STRING(iColumn).
      cRange = bytacol + cColumn.
      chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
      chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.      
      FIND FIRST marag WHERE marag.PERSONNUMMER =  markval.PNR2 NO-LOCK NO-ERROR.
      chWorkSheet:Range(cRange):Value = marag.MARKAGARE NO-ERROR.
      sisnamn = iColumn.
      radins = iColumn + 1 - extrarader.
      radstreck = radins + 3.
      radnamn = radins + 4.
   END.
   IF markag > 1  THEN DO:
      FIND FIRST markvalbuff USE-INDEX MARKNR NO-LOCK.
      FIND NEXT markvalbuff USE-INDEX MARKNR NO-LOCK NO-ERROR.
      IF extram = 0 THEN DO:
         iColumn = 13.
         cColumn = STRING(iColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.         
         FIND FIRST marag WHERE marag.PERSONNUMMER =  markvalbuff.PERSONNUMMER NO-LOCK NO-ERROR.
         chWorkSheet:Range(cRange):Value = STRING(markvalbuff.PERSONNUMMER,"999999-9999") + " " +  marag.MARKAGARE NO-ERROR.  
         
         uColumn = kabelmsvut2 + extrarader + extrarader2.
         /*uColumn = 142 + extrarader + extrarader2.*/
         cColumn = STRING(uColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Value = marag.MARKAGARE NO-ERROR.
         sisnamn = uColumn.
         radins = uColumn + 1 - extrarader.
         radstreck = radins + 3.
         radnamn = radins + 4.
      END.
      ELSE DO:
         extrarader = extrarader + 1.
         iColumn = 12 + extrarader.
         cColumn = STRING(iColumn).
         chWorkSheet:Rows(iColumn):SELECT NO-ERROR.
         chWorkSheet:Rows(iColumn):ENTIREROW NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.         
         FIND FIRST marag WHERE marag.PERSONNUMMER =  markvalbuff.PERSONNUMMER NO-LOCK NO-ERROR.
         chWorkSheet:Range(cRange):Value = STRING(markvalbuff.PERSONNUMMER,"999999-9999") + " " +  marag.MARKAGARE NO-ERROR.           
   
         iColumn = radins + extrarader.
         cColumn = STRING(iColumn).
         chWorkSheet:Rows(iColumn):SELECT NO-ERROR.
         chWorkSheet:Rows(iColumn):ENTIREROW NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.         
         
         uColumn = radstreck + extrarader.
         cColumn = STRING(uColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Value = ".................................................................................." NO-ERROR.
         
         uColumn = radnamn + extrarader.
         cColumn = STRING(uColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Value = marag.MARKAGARE NO-ERROR.
         sisnamn = uColumn.
         radins = uColumn + 1 - extrarader.
         radstreck = radins + 3.
         radnamn = radins + 4.

      END.
      IF markvalbuff.PNR2 BEGINS "0000" THEN gforetag = gforetag.
      ELSE DO:
         extrarader = extrarader + 1.         
         iColumn = 12 + extrarader.
         cColumn = STRING(iColumn).
         chWorkSheet:Rows(iColumn):SELECT NO-ERROR.
         chWorkSheet:Rows(iColumn):ENTIREROW NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         FIND FIRST marag WHERE marag.PERSONNUMMER =  markvalbuff.PNR2 NO-LOCK NO-ERROR.
         chWorkSheet:Range(cRange):Value = STRING(markvalbuff.PNR2,"999999-9999")  + " " +  marag.MARKAGARE NO-ERROR.           
         /*PNR2*/
         
         iColumn = radins + extrarader.
         cColumn = STRING(iColumn).
         chWorkSheet:Rows(iColumn):SELECT NO-ERROR.
         chWorkSheet:Rows(iColumn):ENTIREROW NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.         
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.         
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.                  
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         
         uColumn = radstreck + extrarader.
         cColumn = STRING(uColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Value = ".................................................................................." NO-ERROR.
         
         uColumn = radnamn + extrarader.
         cColumn = STRING(uColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Value = marag.MARKAGARE NO-ERROR.
         sisnamn = uColumn.
         radins = uColumn + 1 - extrarader.
         radstreck = radins + 3.
         radnamn = radins + 4.
      END.
      REPEAT:
         {EXCELFEL.I}
         FIND NEXT markvalbuff NO-LOCK.
         IF NOT AVAILABLE markvalbuff THEN LEAVE.
         extrarader = extrarader + 1.
         iColumn = 12 + extrarader.
         cColumn = STRING(iColumn).
         chWorkSheet:Rows(iColumn):SELECT NO-ERROR.
         chWorkSheet:Rows(iColumn):ENTIREROW NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Font:Bold = TRUE.         
         FIND FIRST marag WHERE marag.PERSONNUMMER =  markvalbuff.PERSONNUMMER NO-LOCK NO-ERROR.
         chWorkSheet:Range(cRange):Value = STRING(markvalbuff.PERSONNUMMER,"999999-9999") + " " +  marag.MARKAGARE NO-ERROR.           
         
         iColumn = radins + extrarader.
         cColumn = STRING(iColumn).
         chWorkSheet:Rows(iColumn):SELECT NO-ERROR.
         chWorkSheet:Rows(iColumn):ENTIREROW NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.         
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.         
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.                  
         chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
         
         uColumn = radstreck + extrarader.
         cColumn = STRING(uColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Value = ".................................................................................." NO-ERROR.
         uColumn = radnamn + extrarader.
         
         cColumn = STRING(uColumn).
         cRange = bytacol + cColumn.
         chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
         chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
         chWorkSheet:Range(cRange):Value = marag.MARKAGARE NO-ERROR.
         sisnamn = uColumn.
         radins = uColumn + 1 - extrarader.
         radstreck = radins + 3.
         radnamn = radins + 4.
         IF markvalbuff.PNR2 BEGINS "0000" THEN gforetag = gforetag.
         ELSE DO:
            extrarader = extrarader + 1.            
            iColumn = 12 + extrarader.
            cColumn = STRING(iColumn).
            chWorkSheet:Rows(iColumn):SELECT NO-ERROR.
            chWorkSheet:Rows(iColumn):ENTIREROW NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            cRange = bytacol + cColumn.
            chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
            chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
            chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.            
            FIND FIRST marag WHERE marag.PERSONNUMMER =  markvalbuff.PNR2 NO-LOCK NO-ERROR.
            chWorkSheet:Range(cRange):Value = STRING(markvalbuff.PNR2,"999999-9999")  + " " +  marag.MARKAGARE NO-ERROR.  
            /*PNR2*/
            iColumn = radins + extrarader.
            
            cColumn = STRING(iColumn).
            chWorkSheet:Rows(iColumn):SELECT NO-ERROR.
            chWorkSheet:Rows(iColumn):ENTIREROW NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.         
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.         
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.                  
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            uColumn = radstreck + extrarader.
            
            cColumn = STRING(uColumn).
            cRange = bytacol + cColumn.
            chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
            chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
            chWorkSheet:Range(cRange):Value = ".................................................................................." NO-ERROR.
            uColumn = radnamn + extrarader.
            
            cColumn = STRING(uColumn).
            cRange = bytacol + cColumn.
            chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
            chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
            chWorkSheet:Range(cRange):Value = marag.MARKAGARE NO-ERROR.
            sisnamn = uColumn.
            radins = uColumn + 1 - extrarader.
            radstreck = radins + 3.
            radnamn = radins + 4.
         END.
      END.
      {EXCELFEL.I} 
   END.
   ELSE DO:
      /*bara en mark�gare -kolla om det �r ett orgnr. Om s� �r fallet l�gg till f�r en extra underskrift
      Om andra och tredje siffran i personnumer �r GE 20 �r det ett orgnr*/
           
      IF INTEGER(SUBSTRING(marag.PERSONNUMMER,3,2)) GE 20 THEN DO:
            
         DEBUGGER:SET-BREAK().   
         IF extram = 0 THEN DO:   
            /*m�ste s�ttas tillbaka efter�t Lena 20190701*/   
            extraraderinnan = extrarader.         
            uColumn = kabelmsvut2 + extrarader + extrarader2.
            /*uColumn = 142 + extrarader + extrarader2.*/
            cColumn = STRING(uColumn).
            cRange = bytacol + cColumn.
            chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
            chWorkSheet:Range(cRange):Font:SIZE = 8 NO-ERROR.
            chWorkSheet:Range(cRange):Value = "Namnf�rtydligande" NO-ERROR.
            sisnamn = uColumn.
            radins = uColumn + 1 - extrarader.
            radstreck = radins + 3.
            radnamn = radins + 4.
         
            extrarader = extrarader + 1.
            iColumn = radins + extrarader.
            cColumn = STRING(iColumn).
            chWorkSheet:Rows(iColumn):SELECT NO-ERROR.
            chWorkSheet:Rows(iColumn):ENTIREROW NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.         
            
            uColumn = radstreck + extrarader.
            cColumn = STRING(uColumn).
            cRange = bytacol + cColumn.
            chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
            chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
            chWorkSheet:Range(cRange):Value = ".................................................................................." NO-ERROR.
            
            uColumn = radnamn + extrarader.
            cColumn = STRING(uColumn).
            cRange = bytacol + cColumn.
            chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
            chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
            chWorkSheet:Range(cRange):Value = "Ledamot f�r " + marag.MARKAGARE NO-ERROR.
            sisnamn = uColumn.
            radins = uColumn + 1 - extrarader.
            radstreck = radins + 3.
            radnamn = radins + 4.            
            
            iColumn = radins + extrarader.
            cColumn = STRING(iColumn).
            chWorkSheet:Rows(iColumn):SELECT NO-ERROR.
            chWorkSheet:Rows(iColumn):ENTIREROW NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.
            chWorkSheet:Rows(iColumn):INSERT NO-ERROR.         
            
            uColumn = radstreck + extrarader.
            cColumn = STRING(uColumn).
            cRange = bytacol + cColumn.
            chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
            chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
            chWorkSheet:Range(cRange):Value = ".................................................................................." NO-ERROR.
            
            uColumn = radnamn + extrarader.
            cColumn = STRING(uColumn).
            cRange = bytacol + cColumn.
            chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
            chWorkSheet:Range(cRange):Font:SIZE = 8 NO-ERROR.
            chWorkSheet:Range(cRange):Value = "Namnf�rtydligande" NO-ERROR.
            sisnamn = uColumn.
            radins = uColumn + 1 - extrarader.
            radstreck = radins + 3.
            radnamn = radins + 4.
            
            extrarader = extraraderinnan.      
         END.       
      END.               
   END.   
   {EXCELFEL.I}
   
   iColumn = 15 + extrarader.
   cColumn = STRING(iColumn).
   cRange = bytacol + cColumn.
   chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
   chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = markval.BETECKNING NO-ERROR.

   iColumn = 17 + extrarader.
   cColumn = STRING(iColumn).
   cRange = bytacol + cColumn.
   /*FIND FIRST FASTIGHET WHERE FASTIGHET.BETECKNING = markval.BETECKNING NO-LOCK NO-ERROR.*/
   chWorkSheet:Range(cRange):Value = fkommun NO-ERROR.
   iColumn = 19 + extrarader.
   cColumn = STRING(iColumn).
   cRange = bytacol + cColumn.
   
   chWorkSheet:Range(cRange):Value = fvaker NO-ERROR.

   iColumn = 25 + extrarader .
   cColumn = STRING(iColumn).
   cRange = bythcol + cColumn.
   IF gforetag = "UMEA" OR gforetag = "UMBR" THEN DO:   
      IF vdok = "KABUBOD" THEN DO:
         iColumn = 25 + extrarader .
         cColumn = STRING(iColumn).
         cRange = "G" + cColumn.
       END.       
       ELSE IF vdok = "KABLUFT" THEN DO:
         /*ska inte ut!*/
       END.
       ELSE DO:      
          iColumn = 26 + extrarader .
          cColumn = STRING(iColumn).
          cRange = "F" + cColumn.
       END.          
   END.
   
   {EXCELFEL.I}
   IF vdok = "KABLUFT" THEN.
   ELSE DO:
      chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
      chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = markval.BETECKNING + "," NO-ERROR.
   END.   
   IF gforetag = "VAST"  OR gforetag = "VELD" OR  gforetag = "celpa" THEN DO: 
      iColumn = 15 + extrarader.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.   
      chWorkSheet:Range(cRange):Value = agfast NO-ERROR.
      iColumn = 17 + extrarader.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.      
      chWorkSheet:Range(cRange):Value = agkommun NO-ERROR.
      iColumn = 19 + extrarader.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Value = aglan NO-ERROR.
   END.      
   {EXCELFEL.I}
   
   IF arendator NE "" THEN DO:                  
      /*IF sisnamn > 137 + extrarader + extrarader2 THEN iColumn = sisnamn + 9.*/
      IF sisnamn > kabelmsvut1 + extrarader + extrarader2 THEN iColumn = sisnamn + 9.
      ELSE iColumn = sisnamn + 14.
      
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Value = arendator NO-ERROR.   
   END.
   IF gforetag = "VAST" OR gforetag = "VELD" OR gforetag = "celpa" THEN DO: 
      iColumn = 133 + extrarader + extrarader2.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Value = ledagare NO-ERROR.
   END.   
   IF gforetag = "UMEA"  THEN DO:
      iColumn = 133 + extrarader + extrarader2.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Value = "Ume� Energi Eln�t AB" NO-ERROR.
      

      /*iColumn = 137 + extrarader + extrarader2.*/
      iColumn = kabelmsvut1 + extrarader + extrarader2.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Value = "Stefan Lindgren" NO-ERROR.   
   END.
   IF gforetag = "UMBR" THEN DO:
      IF vdok = "KABLUFT" THEN.
      ELSE DO:
         iColumn = 131 + extrarader + extrarader2.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Value = "Ume�" NO-ERROR.
      END.   
      IF vdok = "KABUE" OR vdok = "KABUBODUE" THEN DO:
         iColumn = 133 + extrarader + extrarader2.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Value = "Ume� Energi AB" NO-ERROR.
         
         /*iColumn = 137 + extrarader + extrarader2.*/
         iColumn = kabelmsvut1 + extrarader + extrarader2.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Value = "Stefan Lindgren via Fullmakt" NO-ERROR.
      END.      
      ELSE IF vdok = "KABLUFT" THEN DO:         
         iColumn = 134 + extrarader + extrarader2.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Value = "Ume� Energi Umenet AB" NO-ERROR.
         
         iColumn = kabelmsvut1 + extrarader + extrarader2.
         /*iColumn = 137 + extrarader + extrarader2.*/
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         /*chWorkSheet:Range(cRange):Value = "Mats Berggren" NO-ERROR.*/
         /*Nathalie Forsberg har fullmakt att skriva under 20201215*/
         chWorkSheet:Range(cRange):Value = "Nathalie Forsberg via Fullmakt" NO-ERROR.
         
      END.
      ELSE IF vdok = "KABLUFTUE" THEN DO:         
         iColumn = 134 + extrarader + extrarader2.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Value = "Ume� Energi AB       " NO-ERROR.
         
         iColumn = kabelmsvut1 + extrarader + extrarader2.
         /*iColumn = 137 + extrarader + extrarader2.*/
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Value = "Stefan Lindgren via Fullmakt" NO-ERROR.
         
      END.      
      ELSE DO:   
         iColumn = 133 + extrarader + extrarader2.
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         chWorkSheet:Range(cRange):Value = "Ume� Energi Umenet AB" NO-ERROR.
         
         iColumn = kabelmsvut1 + extrarader + extrarader2.
         /*iColumn = 137 + extrarader + extrarader2.*/
         cColumn = STRING(iColumn).
         cRange = bytgcol + cColumn.
         /*chWorkSheet:Range(cRange):Value = "Mats Berggren" NO-ERROR.*/
         /*Nathalie Forsberg har fullmakt att skriva under 20201215*/
         chWorkSheet:Range(cRange):Value = "Nathalie Forsberg via Fullmakt" NO-ERROR.
      END.      
   END.   
   {EXCELFEL.I}
   IF gforetag = "LECA" THEN DO:
      iColumn = 133 + extrarader + extrarader2.
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Value = "PSB och Teknik" NO-ERROR.
      
      iColumn = kabelmsvut1 + extrarader + extrarader2.
      /*iColumn = 137 + extrarader + extrarader2.*/
      cColumn = STRING(iColumn).
      cRange = bytgcol + cColumn.
      chWorkSheet:Range(cRange):Value = "Lennart Carlsson" NO-ERROR.   
   END.
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
   RELEASE OBJECT chWorksheetRange NO-ERROR.   


PROCEDURE imageexcel_UI.   
   ASSIGN iColumn = iColumn + 1.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   chWorkSheet:Range(cRange):SELECT NO-ERROR.  
   IF link NE  ? THEN DO:
      chWorksheetRange = chWorkSheet:Pictures:INSERT(link) NO-ERROR.
      chWorksheetRange:TOP = 1.
      chWorksheetRange:LEFT = 1.
   END.
   chExcelApplication:VISIBLE = TRUE NO-ERROR.
   ASSIGN iColumn = iColumn + 7.
   {EXCELFEL.I}
END PROCEDURE.
