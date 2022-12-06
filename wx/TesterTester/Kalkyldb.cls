 
 /*------------------------------------------------------------------------
    File        : Kalkyldb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Mon Feb 13 13:20:42 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Modules.Kalkyl.Kalkyldb: 
   {KALKYLKAT.I}
   {KALKYLKATH.i}
   /*
   {MTRLTEMPC.I}
   */
   {KALKYLPRODATA.i}
   {KALKYLKATPRODATA.i}
   DEFINE PRIVATE VARIABLE WinApi AS HANDLE NO-UNDO.
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO 
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC  PROPERTY AppServerExtraHandle AS HANDLE NO-UNDO
   PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO.
   DEFINE PUBLIC VARIABLE Control               AS Modules.Kalkyl.Kalkyl NO-UNDO.
   DEFINE PUBLIC PROPERTY kopplataonr AS CHARACTER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC PROPERTY kopplatplnr AS CHARACTER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC PROPERTY kopplatdelnr AS INTEGER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC PROPERTY kopplatartal AS INTEGER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   
   CONSTRUCTOR PUBLIC Kalkyldb(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Connect().
      ASSIGN 
      kopplataonr = ?
      kopplatdelnr = ?
      kopplatplnr = ?
      kopplatartal = ?.
   END CONSTRUCTOR.
   
   DESTRUCTOR Kalkyldb():
      
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DELETE PROCEDURE THIS-OBJECT:AppServerHandle NO-ERROR.
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN DELETE PROCEDURE THIS-OBJECT:AppServerExtraHandle NO-ERROR.
      
   END DESTRUCTOR.
   
   /* Gör en koppling till appserver */
   METHOD PUBLIC VOID Connect():
      IF Guru.Konstanter:appcon THEN DO:
         RUN KALKBERAPPDS.p PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
         IF Guru.Konstanter:varforetypval[27] =  0 THEN RUN KALKBERAPPDSEXTRA.p PERSISTENT SET AppServerExtraHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
      END.
      ELSE DO:
         RUN KALKBERAPPDS.p PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globanv).
         IF Guru.Konstanter:varforetypval[27] =  0 THEN RUN KALKBERAPPDSEXTRA.p PERSISTENT SET AppServerExtraHandle (INPUT Guru.Konstanter:globanv).
      END. 
   END METHOD.
   METHOD PUBLIC VOID KalkNrGet(OUTPUT nrvar AS INTEGER,OUTPUT omr AS CHARACTER):
      ASSIGN 
      nrvar = CONTROL:KalkNrvar
      omr = CONTROL:Omradevar.
   END METHOD.
   METHOD PUBLIC VOID InitializeTTs():
      {KALKYLKATTTH.i}
      
      
   END METHOD.
   /*HÄMTAR UPP STARTEN TILL KALKYLEN  OCH SKAPAR KALKYLHUVUD*/
   METHOD PUBLIC LOGICAL CreateKalkyl():
      RUN LaddaKalkyl IN AppServerHandle (INPUT ?,INPUT ?,OUTPUT DATASET KalkylDS).
      THIS-OBJECT:KalkSpara().
      THIS-OBJECT:KalkTracking(TRUE).
      EMPTY TEMP-TABLE ekalkhuvtt NO-ERROR.
      IF Guru.Konstanter:varforetypchar[3] = "" THEN  RUN startny_UI IN AppServerHandle 
      (INPUT Guru.Konstanter:globomr,INPUT Guru.Konstanter:globanv,OUTPUT TABLE ekalkhuvtt, OUTPUT TABLE kalkylkatalogtt).
      ELSE RUN startny_UI IN AppServerHandle 
      (INPUT Guru.Konstanter:varforetypchar[3],INPUT Guru.Konstanter:globanv,OUTPUT TABLE ekalkhuvtt, OUTPUT TABLE kalkylkatalogtt).
      FIND FIRST ekalkhuvtt NO-LOCK NO-ERROR.
      CREATE kalkhuvtt.
      BUFFER-COPY ekalkhuvtt TO kalkhuvtt.
      kalkhuvtt.TTRECID = RECID(kalkhuvtt).
      EMPTY TEMP-TABLE ekalkhuvtt NO-ERROR. 
      FIND FIRST kalkhuvtt NO-LOCK NO-ERROR.
      ASSIGN 
      kalkhuvtt.ANVANDARE                    = Guru.Konstanter:globanv
      kalkhuvtt.KALKANV                      = Guru.Konstanter:globanvpkod.
   END METHOD.
   /*ÄR KALKYLEN KOPPLAD*/
   METHOD PUBLIC CHARACTER PlnrAonr():
      DEFINE VARIABLE koppladtill AS CHARACTER NO-UNDO.
      FIND FIRST kalkhuvtt  WHERE NO-LOCK NO-ERROR.
      FIND FIRST kalkaonrTT WHERE kalkaonrTT.KALKNR = kalkhuvtt.KALKNR AND kalkaonrTT.OMRADE = kalkhuvtt.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE kalkaonrTT THEN DO TRANSACTION:
         IF kalkaonrTT.AONR NE ? THEN DO:
            koppladtill = "Kopplad till " + Guru.Konstanter:gaok + STRING(kalkaonrTT.AONR) + " " + STRING(kalkaonrTT.DELNR,Guru.Konstanter:varforetypchar[1]).
         END.
         ELSE IF kalkaonrTT.PLANNR NE ? THEN DO:
            koppladtill = "Kopplad till " + Guru.Konstanter:gplk + kalkaonrTT.PLANNR + " " + STRING(kalkaonrTT.ARTAL).
         END.   
      END.  
      RETURN koppladtill.  
   END METHOD.
   METHOD PUBLIC CHARACTER PlnrAonrMulti():
      DEFINE VARIABLE koppladtill AS CHARACTER NO-UNDO.
      koppladtill = "Kopplad till " + Guru.Konstanter:gaok + " ".
      FOR EACH kalkaonrTT WHERE NO-LOCK:
         IF kalkaonrTT.AONR NE ? THEN DO:
            IF koppladtill MATCHES "*" + STRING(kalkaonrTT.AONR) + " " + STRING(kalkaonrTT.DELNR,Guru.Konstanter:varforetypchar[1]) + "*" THEN.
            ELSE koppladtill =  koppladtill + STRING(kalkaonrTT.AONR) + " " + STRING(kalkaonrTT.DELNR,Guru.Konstanter:varforetypchar[1]) + ", ".
         END.
         ELSE IF kalkaonrTT.PLANNR NE ? THEN DO:
            IF koppladtill MATCHES "*" + kalkaonrTT.PLANNR + " " + STRING(kalkaonrTT.ARTAL) + "*" THEN.
            ELSE koppladtill = koppladtill + kalkaonrTT.PLANNR + " " + STRING(kalkaonrTT.ARTAL) + ", ".
         END.   
      END.  
      
      RETURN koppladtill.  
   END METHOD.
   METHOD PUBLIC CHARACTER VisaKalkNrMulti( ):
     DEFINE VARIABLE kalknrmulti AS CHARACTER NO-UNDO.
     FOR EACH kalkhuvtt WHERE NO-LOCK:
        kalknrmulti = kalknrmulti + STRING(kalkhuvtt.KALKNR) + ", ".
     END.
     FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
     RETURN  kalknrmulti.
   END METHOD. 
   METHOD PUBLIC CHARACTER VisaKalkOmrMulti( ):
     DEFINE VARIABLE kalkomrmulti AS CHARACTER NO-UNDO.
     FOR EACH kalkhuvtt WHERE NO-LOCK:
        IF kalkomrmulti MATCHES "*" + STRING(kalkhuvtt.OMRADE) + ",*" THEN.
        ELSE DO:
           Guru.Konstanter:OmradeTTh:FIND-FIRST("WHERE OMRADE = '" + STRING(kalkhuvtt.OMRADE) + "'") NO-ERROR.
           kalkomrmulti = kalkomrmulti + STRING(kalkhuvtt.OMRADE) + " " + STRING(Guru.Konstanter:OmradeTTh:BUFFER-FIELD("NAMN"):BUFFER-VALUE) + ", ".
        END.   
        
     END.
     FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
     RETURN kalkomrmulti.
   END METHOD.
   METHOD PUBLIC CHARACTER VisaKalkAnmMulti( ):
     DEFINE VARIABLE kalkanrrmulti AS CHARACTER NO-UNDO.
     FOR EACH kalkhuvtt WHERE NO-LOCK:
        kalkanrrmulti = kalkanrrmulti + STRING(kalkhuvtt.ANMARKNING) + ", ".
     END.
     FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
     RETURN kalkanrrmulti.
   END METHOD.
   
   /*SPARA KALKYLEN*/
   METHOD PUBLIC VOID KalkSpara():
      DEFINE VARIABLE tk AS DECIMAL NO-UNDO.
      DEFINE VARIABLE fk AS DECIMAL NO-UNDO.
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      
      FOR EACH kalknumttbuf BREAK BY kalknumttbuf.KALKNR:
         ACCUMULATE kalknumttbuf.TOTKOST (TOTAL BY kalknumttbuf.KALKNR). 
         ACCUMULATE kalknumttbuf.FRITOTKOST (TOTAL BY kalknumttbuf.KALKNR). 
         IF LAST-OF(kalknumttbuf.KALKNR) THEN DO:
            tk = ACCUM TOTAL BY kalknumttbuf.KALKNR kalknumttbuf.TOTKOST. 
            fk = ACCUM TOTAL BY kalknumttbuf.KALKNR kalknumttbuf.FRITOTKOST.               
         END.
      END.
      IF THIS-OBJECT:CONTROL = ? THEN.
      ELSE DO:
         
         THIS-OBJECT:Control:GridKalkylKoder:GuruUpdateTitle("Summa för Grundkalkyl :" + STRING(tk,"->>>>>>>>>>>>9")).
      END.   
     
      THIS-OBJECT:KalkTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylDS:HANDLE).
      
     /*
      hDSChanges:WRITE-XML("FILE", "C:\CTest.xml"). 
      */
      RUN SparaProDataSetKalkylDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET KalkylDS:HANDLE).
      THIS-OBJECT:KalkTracking(TRUE).              
      FIND FIRST kalkhuvtt  WHERE NO-LOCK NO-ERROR.
                 
   END METHOD.
   
   /*Robin xml*/
   METHOD PUBLIC VOID XmlExport(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):  
      DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFile           AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lWriteBeforeImage   AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE globanv AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE dResult AS System.Windows.Forms.DialogResult NO-UNDO.
      
      Guru.Konstanter:globanv = Guru.Konstanter:globanv.
      cFile = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN cFile = webclienttempdir.
      cFile = cFile  + STRING(kalknr) + omr + kalkhuvtt.BENAMNING + ".xml".
      
      ASSIGN
      cTargetType       = "FILE"
      lFormatted        = NO
      cEncoding         = ?
      cSchemaLocation   = ?
      lWriteSchema      = YES
      lMinSchema        = FALSE
      lWriteBeforeImage = FALSE.
      DATASET KalkylDS:WRITE-XML (cTargetType, cFile, lFormatted, cEncoding, cSchemaLocation, lWriteSchema, lMinSchema, lWriteBeforeImage). 
      /*EPOST*/
      dResult = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetString(84) + cFile + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(85), THIS-OBJECT:Root:LanguageManager:GetString(83), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF dResult:ToString() EQ "YES" THEN DO:
         THIS-OBJECT:XmlToMail(cFile).
      END.
      /*FIX*/
      FIND FIRST kalkhuvtt  WHERE NO-LOCK NO-ERROR.    
   END METHOD.
   
   /*EPOST Skriv om för .net i v11?*/
   METHOD PUBLIC VOID XmlToMail(INPUT cFile AS CHARACTER):
      DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
      file-info:file-name = ".".
      orgdir = file-info:full-pathname.
       
      RUN SPECIALMAPI.P (INPUT "", INPUT "", INPUT cFile).
      THIS-OBJECT:WinApi = Guru.Konstanter:hpApi.
      RUN SetCurrentDirectoryA IN WinApi  (INPUT orgdir).
      IF VALID-HANDLE(WinApi) THEN DELETE PROCEDURE WinApi.
   END METHOD.
   
   METHOD PUBLIC LOGICAL XmlImport():
      DO ON ERROR UNDO, LEAVE:   
         DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
         DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
         DEFINE VARIABLE cReadMode AS CHARACTER NO-UNDO.
         DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
         DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL NO-UNDO.
         DEFINE VARIABLE globanv AS CHARACTER   NO-UNDO.
         DEFINE VARIABLE initDir AS CHARACTER NO-UNDO.
         DEFINE VARIABLE dialogVar AS LOGICAL NO-UNDO.
         
         initDir = SESSION:TEMP-DIRECTORY.
         {SESSIONTEMPDIR.I}
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN initDir = webclienttempdir.
         
         SYSTEM-DIALOG GET-FILE cFile
            TITLE "Välj den fil som du vill importera"
            FILTERS 'All Files (*.xml*)' '*.xml*'
            INITIAL-DIR initDir
            UPDATE dialogVar.
         IF dialogVar EQ TRUE THEN DO.
            THIS-OBJECT:KalkTracking(FALSE).
            THIS-OBJECT:LaddaKalkyl(?,?). 
            THIS-OBJECT:KalkTracking(TRUE).
            
            ASSIGN
            cSourceType             = "FILE"
            cReadMode               = "MERGE"
            cSchemaLocation         = ?
            lOverrideDefaultMapping = FALSE.
            DATASET KalkylDS:READ-XML (cSourceType, cFile, cReadMode, cSchemaLocation, lOverrideDefaultMapping).
            
            RETURN TRUE.
         END.
      END.
      RETURN FALSE.
   END METHOD.
  
   
   METHOD PUBLIC VOID KopierakalkylXML(INPUT nykatidvar AS INTEGER,INPUT nyomrvar AS CHARACTER,OUTPUT felmed AS LOGICAL): 
      DEFINE VARIABLE nykalknr AS INTEGER NO-UNDO.
      /*hitta nytt kalkylnr*/
      RUN omradekoll_UI IN AppServerHandle (INPUT nyomrvar, OUTPUT nykalknr).
      /*byt kalkylnr på allt*/
      FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
      ASSIGN 
      kalkhuvtt.KALKNR = nykalknr
      kalkhuvtt.KLOGID = nykatidvar
      kalkhuvtt.OMRADE = nyomrvar                      
      kalkhuvtt.ANVANDARE = Guru.Konstanter:globanv
      kalkhuvtt.KALKANV = Guru.Konstanter:globanvpkod.
      FIND FIRST kalkaonrTT WHERE NO-LOCK NO-ERROR.
      ASSIGN
      kalkaonrTT.AONR   = ?
      kalkaonrTT.DELNR = ?
      kalkaonrTT.KALKNR = nykalknr
      kalkaonrTT.OMRADE  = nyomrvar. 
      FOR EACH kalknumtt WHERE NO-LOCK:
         ASSIGN
         kalknumtt.OMRADE = nyomrvar
         kalknumtt.KALKNR = nykalknr.
         FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM.
            ASSIGN
            kalknumsubtt.OMRADE = nyomrvar
            kalknumsubtt.KALKNR = nykalknr.
         END.
      END.
      /*skapa kalkylimport*/
      FOR EACH kalknumtt WHERE NO-LOCK:
         IF kalknumtt.ARBKOD = "EGEN" THEN.
         ELSE DO:
            CREATE KalkylimportTT.
            BUFFER-COPY kalknumtt TO KalkylimportTT.
            KalkylimportTT.TTRECID = RECID(KalkylimportTT).
            /*deleta kalknum och subbar*/
            FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM.
               DELETE kalknumsubtt.
            END.
            DELETE kalknumtt.
         END.           
      END. 
      /*Spara kalkyl*/
      
      THIS-OBJECT:KalkSpara(). 
      /*Ladda kalkyl*/
      
      THIS-OBJECT:CONTROL:LoadKalkyl(INPUT nykalknr, INPUT nyomrvar, INPUT FALSE). 
   END METHOD.
   
   METHOD PUBLIC VOID KalkTracking(INPUT onoff AS LOGICAL):
      
      TEMP-TABLE kalknumtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkaonrTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkfaktorertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkegnaprisertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalktmtrlTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkttidlageTT:TRACKING-CHANGES = onoff.
                
   END METHOD.    
   /*HÄMTAR KALATLOGER MM OCH ALLT SOM RÖR KALKYLEN*/
   METHOD PUBLIC LOGICAL FetchKalkyl(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO. 
      DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
      RUN kathmt_UI IN AppServerHandle (INPUT kalknr,INPUT omr, INPUT Guru.Konstanter:globanv,OUTPUT felmed, OUTPUT TABLE kalkylarbkodertt,
      OUTPUT TABLE kalkylloppostertt,OUTPUT TABLE kalkylkatalogtt, OUTPUT TABLE markfiltertt).
     
      THIS-OBJECT:FetchPriser(kalknr, omr).
      THIS-OBJECT:LaddaKalkyl(INPUT kalknr,INPUT omr).
      /*
      RUN LaddaKalkyl IN AppServerHandle (INPUT kalknr,INPUT omr,OUTPUT DATASET KalkylDS).
      */
      THIS-OBJECT:anvkalkyl (INPUT kalknr,INPUT omr).  
      THIS-OBJECT:KalkTracking(TRUE).
      IF felmed NE "" THEN DO:
         res = System.Windows.Forms.MessageBox:Show(felmed,"",System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.         
      END.   
      FIND FIRST kalkhuvtt  WHERE NO-LOCK NO-ERROR. 
      FIND FIRST kalkylkatalogtt WHERE NO-LOCK NO-ERROR.
      FIND FIRST markfiltertt WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkylkatalogtt THEN DO:        
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(53), "", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.
      END.
     
      THIS-OBJECT:Root:DatabaseManager:Global:FetchDefaultValues("KALKYL",Guru.GlobalaVariabler:GuruDefaultAnv,kalknr, omr).
      RETURN TRUE.
   END METHOD.
   /*HÄMTAR KALYL DATASET*/
   METHOD PUBLIC VOID LaddaKalkyl(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):
      RUN LaddaKalkyl IN AppServerHandle (INPUT kalknr,INPUT omr,OUTPUT DATASET KalkylDS).
      THIS-OBJECT:KalkSpara().
   END METHOD.
   METHOD PUBLIC LOGICAL FetchVisaKalkyl(INPUT extravaldfastth AS HANDLE):
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO. 
      DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      extravaldfastth:FIND-FIRST() NO-ERROR.
      THIS-OBJECT:CONTROL:KalkNrvar = extravaldfastth:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
      THIS-OBJECT:CONTROL:Omradevar = extravaldfastth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.      
      RUN kathmt_UI IN AppServerHandle (INPUT extravaldfastth:BUFFER-FIELD("KALKNR"):BUFFER-VALUE,INPUT extravaldfastth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE, INPUT Guru.Konstanter:globanv,OUTPUT felmed, OUTPUT TABLE kalkylarbkodertt,
      OUTPUT TABLE kalkylloppostertt,OUTPUT TABLE kalkylkatalogtt, OUTPUT TABLE markfiltertt).
     
      THIS-OBJECT:FetchPriser(INPUT extravaldfastth:BUFFER-FIELD("KALKNR"):BUFFER-VALUE,INPUT extravaldfastth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE).
      
      queryvar =  "FOR EACH " + extravaldfastth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(extravaldfastth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         RUN LaddaKalkyl IN AppServerHandle (INPUT extravaldfastth:BUFFER-FIELD("KALKNR"):BUFFER-VALUE,INPUT extravaldfastth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE,OUTPUT DATASET KalkylDS APPEND).
         qH:GET-NEXT().    
      END.
         
         
      THIS-OBJECT:KalkTracking(FALSE).
      IF felmed NE "" THEN DO:
         res = System.Windows.Forms.MessageBox:Show(felmed,"",System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.         
      END.   
      FIND FIRST kalkhuvtt  WHERE NO-LOCK NO-ERROR. 
      FIND FIRST kalkylkatalogtt WHERE NO-LOCK NO-ERROR.
      FIND FIRST markfiltertt WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkylkatalogtt THEN DO:        
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(53), "", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.
      END.
     
      THIS-OBJECT:Root:DatabaseManager:Global:FetchDefaultValues("KALKYL",Guru.GlobalaVariabler:GuruDefaultAnv,kalknr, omr).
      RETURN TRUE.
   END METHOD.
   /*hämtar dataset som är skapade i method ovan*/
   METHOD PUBLIC VOID HmtLaddaKalkylDs(OUTPUT DATASET KalkylDS):
      THIS-OBJECT:KalkSpara().
      RETURN.
   END METHOD.
   /*hämtar temtable som är skapade i method raknaala*/
   METHOD PUBLIC VOID HmtLaddaKalkylBer(OUTPUT TABLE kalkantal,OUTPUT TABLE kalkkostnad,OUTPUT TABLE kalkylprisertt, OUTPUT TABLE kalkvisningtt):
      RETURN.
   END METHOD.
   /*HÄMTAR UNDERLAG TILL EGNAPRISER OCH FAKTORER*/
   METHOD PUBLIC VOID FetchPriser(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):
      RUN kphmt IN AppServerHandle (INPUT kalknr, INPUT omr,OUTPUT TABLE kalkylprisertt, OUTPUT TABLE kalkvisningtt).                
   END METHOD.
   /*hämtar mallar*/
   METHOD PUBLIC VOID MallarHmt():
      RUN LaddaMallar IN AppServerHandle (INPUT 0 ,OUTPUT DATASET KalkylMallarDS).
      
   END METHOD.
   /*BERÄKNAR SCHAKTVOLYM*/
   METHOD PUBLIC VOID VolymBer():
      DEFINE VARIABLE tangens AS DECIMAL NO-UNDO.
      DEFINE VARIABLE breddvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE djupvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE langdvar AS DECIMAL NO-UNDO.  
      DEFINE VARIABLE markvar AS DECIMAL NO-UNDO.  
      ASSIGN
      tangens = 0.26795
      breddvar = VolymBerTT.SCHAKTBREDD / 100
      djupvar = VolymBerTT.SCHAKTDJUP / 100
      langdvar = VolymBerTT.SCHAKTLANGD.      
      IF  VolymBerTT.SVARMARK = TRUE THEN markvar = 0.2.
      ELSE markvar = 0.1.       
      VolymBerTT.TOTVOLYM = ((breddvar * djupvar + djupvar * tangens * djupvar) * langdvar).      
      IF VolymBerTT.RORFOR = TRUE THEN DO:
         ASSIGN 
         VolymBerTT.SKYDDFYLLNING = 0
         VolymBerTT.TILLAGGVOLYM = VolymBerTT.TOTVOLYM.
      END.
      ELSE DO:
         ASSIGN
         VolymBerTT.SKYDDFYLLNING    = ((breddvar * markvar + markvar * tangens * markvar) * langdvar)
         VolymBerTT.TILLAGGVOLYM = VolymBerTT.TOTVOLYM - VolymBerTT.SKYDDFYLLNING   .
      END. 
      /*ANTALET KVADRAT METER SOM YTBELÄGGNINGEN UTGÖR*/            
      VolymBerTT.YTBELAGGD  = ((2 * djupvar * tangens) + breddvar) * langdvar.   
   
   END METHOD.
   
   METHOD PUBLIC VOID VolymBerHmt():
      CREATE VolymberTT.
      VolymberTT.TTRECID = RECID(VolymberTT).  
      CREATE VolymberTT.
      VolymberTT.TTRECID = RECID(VolymberTT).
      CREATE VolymberTT.
      VolymberTT.TTRECID = RECID(VolymberTT).
      CREATE VolymberTT.
      VolymberTT.TTRECID = RECID(VolymberTT).
      CREATE VolymberTT.         
      VolymberTT.TTRECID = RECID(VolymberTT).
   END METHOD.
   METHOD PUBLIC VOID ArendeStatusHmt(INPUT kalkdatah AS HANDLE):
      
      RUN ArendeStatus_UI IN AppServerHandle (INPUT kalkdatah,OUTPUT kalkttidlageTTh).
              
   END METHOD.
   
   METHOD PUBLIC VOID TidutHmt(vad AS CHARACTER):
      CREATE tidut.
      tidut.UT = vad.  
      tidut.TTRECID = RECID(tidut).            
   END METHOD.
   
   METHOD PUBLIC VOID FrekvensHmt(INPUT klsubid AS INTEGER, INPUT arbkodvar AS CHARACTE, INPUT lopnrvar AS INTEGER):
      RUN LaddaFrekvensHmt IN AppServerHandle (INPUT klsubid,INPUT arbkodvar, INPUT lopnrvar,OUTPUT DATASET KalkylFrekDS).                 
   END METHOD.
   
   /*HÄMTAR AVTALSKALKYLEN*/
   METHOD PUBLIC VOID FetchAvtal(INPUT Kartal AS INTEGER):
      RUN avtalhmt IN AppServerHandle (INPUT Kartal, OUTPUT TABLE Avtalskalktt).
   END METHOD.
   /*SPARAR PRISER OCH FAKTORER*/
   METHOD PUBLIC VOID SavePrisFakt():
      THIS-OBJECT:KalkSpara().      
   END METHOD.
   /*VID ÅTERSÄLLNING AV FRIKALKYL*/   
   METHOD PUBLIC VOID FetchFriKoder(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(46),
      THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "No" THEN RETURN.
       THIS-OBJECT:RestoreFriKod().   
   END METHOD.
   
   /*VID ÅTERSÄLLNING AV FRIKALKYL funkade */ 
   METHOD PUBLIC VOID RestoreFriKod():
      DEFINE VARIABLE num AS INTEGER NO-UNDO.
      FOR EACH kalknumtt,
      EACH kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM: 
         IF kalknumtt.ARBKOD = "EGEN" THEN DO:
            ASSIGN 
            kalknumsubtt.FRIBENAMNING = kalknumsubtt.BENAMNING         
            kalknumsubtt.FRIKOSTNAD   = kalknumsubtt.KOSTNAD
            kalknumsubtt.FRIPRIS      = kalknumsubtt.PRIS
            kalknumsubtt.FRIAVRUND    = kalknumsubtt.AVRUND 
            kalknumsubtt.FRITIMMAR    = kalknumsubtt.TIMMAR.
         END.
         ELSE DO:
            FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalknumtt.KLOGSUBID AND 
            kalkylprisertt.KPID = kalknumsubtt.KPID NO-LOCK NO-ERROR.
            ASSIGN 
            kalknumsubtt.FRIBENAMNING = kalknumsubtt.BENAMNING         
            kalknumsubtt.FRIKOSTNAD   = kalknumsubtt.KOSTNAD
            kalknumsubtt.FRIPRIS      = kalkylprisertt.PRIS
            kalknumsubtt.FRIAVRUND    = kalknumsubtt.AVRUND 
            kalknumsubtt.PRIS         = kalkylprisertt.PRIS
            kalknumsubtt.FRITIMMAR    = kalknumsubtt.TIMMAR.
         END.          
      END.
      THIS-OBJECT:KalkSpara().       
   END METHOD.
   
   /*VID NU KALKYL BLA. SPARA */
   METHOD PUBLIC VOID KopplatAoPnr(aonrvar AS CHARACTER,delnrvar AS INTEGER,pnrvar AS CHARACTER,artalvar AS INTEGER):
      ASSIGN 
      kopplataonr = aonrvar
      kopplatdelnr = delnrvar
      kopplatplnr = pnrvar
      kopplatartal = artalvar.
   END METHOD.   
   METHOD PUBLIC VOID SaveHuvud(OUTPUT txt AS CHARACTER, OUTPUT isit AS LOGICAL):
      EMPTY TEMP-TABLE ekalkhuvtt NO-ERROR.
      CREATE ekalkhuvtt.
      BUFFER-COPY kalkhuvtt TO ekalkhuvtt. 
      
      RUN sparakalkhuv_UI IN AppServerHandle (OUTPUT isit,OUTPUT txt,INPUT-OUTPUT TABLE ekalkhuvtt).
      IF isit = TRUE THEN RETURN.
      FIND FIRST ekalkhuvtt WHERE NO-LOCK NO-ERROR.
      BUFFER-COPY ekalkhuvtt TO kalkhuvtt.
      kalkhuvtt.TTRECID = RECID(kalkhuvtt).
      EMPTY TEMP-TABLE ekalkhuvtt NO-ERROR.  
      FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
      FIND FIRST kalkaonrTT WHERE kalkaonrTT.KALKNR = kalkhuvtt.KALKNR AND kalkaonrTT.OMRADE = kalkhuvtt.OMRADE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkaonrTT THEN DO:
         CREATE kalkaonrTT.
         ASSIGN
         kalkaonrTT.KALKNR = kalkhuvtt.KALKNR
         kalkaonrTT.OMRADE = kalkhuvtt.OMRADE
         kalkaonrTT.TYP = kalkhuvtt.TYP
         kalkaonrTT.AONR   = kopplataonr 
         kalkaonrTT.DELNR  = kopplatdelnr 
         kalkaonrTT.PLANNR = kopplatplnr 
         kalkaonrTT.ARTAL  = kopplatartal
         kalkaonrTT.AKTIV = TRUE 
         kalkaonrTT.TTRECID = RECID(kalkaonrTT).
         IF Guru.GlobalaVariabler:arendekalk = "ÄRENDE" THEN kalkaonrTT.STATUSNIV = Guru.GlobalaVariabler:arendekalk. 
         
      END.
      IF Guru.GlobalaVariabler:arendekalk = "ÄRENDE" THEN DO:
         FIND FIRST kalkttidlageTT NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kalkttidlageTT THEN DO:
            RUN kalktidl_UI IN AppServerHandle (INPUT kalkhuvtt.KALKNR, INPUT kalkhuvtt.OMRADE, OUTPUT TABLE ekalkttidlageTT).
            FOR EACH ekalkttidlageTT NO-LOCK:
               CREATE kalkttidlageTT.
               BUFFER-COPY ekalkttidlageTT TO kalkttidlageTT.
               IF kalkttidlageTT.ORDNING = "1" THEN DO:
                  kalkttidlageTT.DATUMTID = DATETIME(TODAY, MTIME).
                  kalkttidlageTT.ANVANDARE1 = Guru.Konstanter:globanv.
                  Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "'")  NO-ERROR.
                  IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
                     ASSIGN
                     kalkttidlageTT.NAMNANVANDARE1 = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("AV-NAMN"):BUFFER-VALUE.
                  END.
               END.  
               kalkttidlageTT.TTRECID = RECID(kalkttidlageTT).               
            END. 
            THIS-OBJECT:CONTROL:GridTidlage:GuruReopen().
         END.   
      END.   
        
      THIS-OBJECT:KalkSpara().
      FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
      FIND FIRST kalkaonrTT WHERE kalkaonrTT.KALKNR = kalkhuvtt.KALKNR AND kalkaonrTT.OMRADE = kalkhuvtt.OMRADE NO-LOCK NO-ERROR.
      RUN startstatusnivkoll_UI IN AppServerHandle (INPUT kalkhuvtt.KALKNR, INPUT kalkhuvtt.OMRADE, OUTPUT kalkaonrTT.STATUSNIV).   
      THIS-OBJECT:KalkSpara().        
   END METHOD.
   
   /*BRÄKNING FÖR ATT VISA*/ 
   METHOD PUBLIC VOID RaknaAllaKoder():       
      EMPTY TEMP-TABLE KalkRubrikTT NO-ERROR.
      EMPTY TEMP-TABLE kalkantal NO-ERROR. 
      EMPTY TEMP-TABLE kalkkostnad NO-ERROR.
      THIS-OBJECT:RaknaEnKod(TRUE).
      FOR EACH kalkantal WHERE kalkantal.KVID = 0 NO-LOCK:
         DELETE kalkantal.
      END.
      FOR EACH kalkantal WHERE kalkantal.TIMTYP = "" NO-LOCK:
         DELETE kalkantal.
      END.
      FOR EACH kalkkostnad WHERE kalkkostnad.KVID = 0 NO-LOCK:
         DELETE kalkkostnad.
      END.
      FOR EACH kalkkostnad WHERE kalkkostnad.KOSTTYP = "" NO-LOCK:
         DELETE kalkkostnad.
      END.
      FOR EACH kalkantal BREAK BY kalkantal.TIMTYP:
         ACCUMULATE kalkantal.SUMMA (TOTAL BY kalkantal.TIMTYP).
         ACCUMULATE kalkantal.FRISUMMA (TOTAL BY kalkantal.TIMTYP). 
         IF LAST-OF(kalkantal.TIMTYP) THEN DO:
            IF (ACCUM TOTAL BY kalkantal.TIMTYP kalkantal.SUMMA) = 0 AND (ACCUM TOTAL BY kalkantal.TIMTYP kalkantal.FRISUMMA) = 0  THEN DO:
               FOR EACH kalkantalbuf  WHERE kalkantalbuf.TIMTYP = kalkantal.TIMTYP NO-LOCK:
                  DELETE kalkantalbuf.
               END.
            END.        
         END.
      END.
      FOR EACH kalkkostnad BREAK BY kalkkostnad.KOSTTYP:
         ACCUMULATE kalkkostnad.TOTKOST (TOTAL BY kalkkostnad.KOSTTYP). 
         ACCUMULATE kalkkostnad.FRITOTKOST (TOTAL BY kalkkostnad.KOSTTYP). 
         IF LAST-OF(kalkkostnad.KOSTTYP) THEN DO:
            IF (ACCUM TOTAL BY kalkkostnad.KOSTTYP kalkkostnad.TOTKOST) = 0 AND (ACCUM TOTAL BY kalkkostnad.KOSTTYP kalkkostnad.FRITOTKOST) = 0 THEN DO:
               FOR EACH kalkkostnadbuf  WHERE kalkkostnadbuf.KOSTTYP = kalkkostnad.KOSTTYP NO-LOCK:
                  DELETE kalkkostnadbuf.
               END.
            END.        
         END.
      END.
      IF Guru.GlobalaVariabler:FranUppf = TRUE THEN DO:
         FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
         IF kalkhuvtt.UTYP NE 1 THEN DO:
            FIND FIRST kalkvisningtt WHERE kalkvisningtt.KOSTTYP = "Materiel" NO-LOCK NO-ERROR.
            IF AVAILABLE kalkvisningtt THEN DO:
               FOR EACH kalkkostnad WHERE kalkkostnad.KVID = kalkvisningtt.KVID NO-LOCK:
                  ASSIGN
                  kalkkostnad.TOTKOST = 0
                  kalkkostnad.FRITOTKOST = 0.
               END.   
            END.
            
            FIND FIRST kalkkostnad WHERE kalkkostnad.KVID = kalkvisningtt.KVID NO-LOCK NO-ERROR.
            kalkkostnad.TOTKOST = THIS-OBJECT:MtrlKost(?).
         END.   
      END.    
      ELSE DO:
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" THEN DO:
            FIND FIRST kalkvisningtt WHERE kalkvisningtt.KOSTTYP = "Materiel" NO-LOCK NO-ERROR.
            IF AVAILABLE kalkvisningtt THEN DO:
               FOR EACH kalkkostnad WHERE kalkkostnad.KVID = kalkvisningtt.KVID NO-LOCK:
                  ASSIGN
                  kalkkostnad.TOTKOST = 0
                  kalkkostnad.FRITOTKOST = 0.
               END.   
            END.   
         END.
      END.      
       
      THIS-OBJECT:KalkRubrik().
   END METHOD.
   /*RUBRIKER TILL VISNING*/
   METHOD PUBLIC VOID KalkRubrik():       
      FOR EACH kalkvisningtt WHERE NO-LOCK:
         FIND FIRST KalkRubrikTT WHERE KalkRubrikTT.BENAMNING = kalkvisningtt.KOSTTYP AND KalkRubrikTT.KOSTTIMM = TRUE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KalkRubrikTT THEN DO:
            FIND FIRST kalkkostnad WHERE kalkkostnad.KOSTTYP = kalkvisningtt.KOSTTYP  NO-LOCK NO-ERROR.
            IF AVAILABLE kalkkostnad THEN DO:
               IF kalkvisningtt.KOSTTYP NE "" THEN DO:
                  CREATE KalkRubrikTT.
                  ASSIGN  
                  KalkRubrikTT.BENAMNING = kalkvisningtt.KOSTTYP
                  KalkRubrikTT.KVID = kalkvisningtt.KVID
                  KalkRubrikTT.KOSTTIMM = TRUE 
                  KalkRubrikTT.ORDNING = kalkvisningtt.ORDNINGKOSTNAD.
               END.
            END.      
         END.         
         FIND FIRST KalkRubrikTT WHERE KalkRubrikTT.BENAMNING = kalkvisningtt.TIMTYP AND KalkRubrikTT.KOSTTIMM = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KalkRubrikTT THEN DO:
            FIND FIRST kalkantal WHERE kalkantal.TIMTYP = kalkvisningtt.TIMTYP  NO-LOCK NO-ERROR.
            IF AVAILABLE kalkantal THEN DO:
               IF kalkvisningtt.TIMTYP NE "" THEN DO:
                  CREATE KalkRubrikTT.
                  ASSIGN  
                  KalkRubrikTT.BENAMNING = kalkvisningtt.TIMTYP
                  KalkRubrikTT.KVID = kalkvisningtt.KVID
                  KalkRubrikTT.KOSTTIMM = FALSE  
                  KalkRubrikTT.ORDNING = kalkvisningtt.ORDNINGKOSTNAD.
               END.
            END.      
         END.
         IF kalkvisningtt.EABER = TRUE THEN DO:
            FIND FIRST KalkRubrikTT WHERE KalkRubrikTT.BENAMNING = "EA" AND KalkRubrikTT.KOSTTIMM = TRUE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE KalkRubrikTT THEN DO:         
               CREATE KalkRubrikTT.
               ASSIGN  
               KalkRubrikTT.BENAMNING = "EA"
               KalkRubrikTT.KVID = 0
               KalkRubrikTT.KOSTTIMM = TRUE  
               KalkRubrikTT.ORDNING = 0.
            END.   
         END.     
      END. 
   END METHOD.
   /*beräknakoden*/
   METHOD PUBLIC VOID RaknaEnKod(allakoder AS LOGICAL):
      DEFINE VARIABLE totkostvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE fritotkostvar AS DECIMAL NO-UNDO.
      EMPTY TEMP-TABLE kalkantal NO-ERROR. 
      EMPTY TEMP-TABLE kalkkostnad NO-ERROR.     
      IF allakoder = FALSE THEN DO:
         IF NOT AVAILABLE kalknumtt THEN DO:
            FIND FIRST kalknumtt WHERE kalknumtt.NUM = kalknumsubtt.NUM NO-ERROR. 
         END.
         OPEN QUERY ksq FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM NO-LOCK.
         GET FIRST ksq NO-LOCK.     
      END.
      ELSE DO:
         OPEN QUERY ksq FOR EACH kalknumsubtt NO-LOCK.
         GET FIRST ksq NO-LOCK.
      END.    
      DO WHILE AVAILABLE(kalknumsubtt):
         IF allakoder = TRUE THEN DO:
            FIND FIRST kalknumtt WHERE kalknumtt.NUM = kalknumsubtt.NUM NO-ERROR.  
         END.
                       
         CREATE kalkantal.
         /*kalkberin*/
         BUFFER-COPY kalknumtt TO kalkantal.
         CREATE kalkkostnad.
         BUFFER-COPY kalknumtt TO kalkkostnad.
         ASSIGN
         kalkkostnad.FRITOTKOST = 0
         kalkkostnad.TOTKOST = 0.
         FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalknumtt.KLOGSUBID AND 
         kalkylprisertt.KPID = kalknumsubtt.KPID NO-LOCK NO-ERROR.
         FIND FIRST kalkvisningtt WHERE kalkvisningtt.KVID = kalkylprisertt.KVID NO-LOCK NO-ERROR.
         IF AVAILABLE kalkvisningtt THEN DO:    
            ASSIGN 
            kalkantal.TIMTYP = kalkvisningtt.TIMTYP
            kalkantal.KPID = kalkylprisertt.KPID
            kalkantal.KVID = kalkylprisertt.KVID.   
            IF allakoder = TRUE THEN DO:          
               kalkantal.SUMMA = kalkantal.SUMMA + KalkBerAntal(kalknumsubtt.TIMMAR) * kalknumtt.ANTAL.
               kalkantal.FRISUMMA = kalkantal.FRISUMMA + KalkBerAntal(kalknumsubtt.FRITIMMAR) * kalknumtt.ANTAL.
            END.
            ELSE DO:
               kalkantal.SUMMA = kalkantal.SUMMA + kalknumsubtt.TIMMAR * kalknumtt.ANTAL.
               kalkantal.FRISUMMA = kalkantal.FRISUMMA + kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL.
            END.      
            ASSIGN 
            kalkkostnad.KOSTTYP = kalkvisningtt.KOSTTYP
            kalkkostnad.KPID = kalkylprisertt.KPID
            kalkkostnad.KVID = kalkylprisertt.KVID. 
            /*kalkberin*/   
            IF allakoder = TRUE THEN DO: 
               IF kalknumtt.ARBKOD = "EGEN" THEN DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + KalkBerKost(1,kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalknumsubtt.PRIS)
                  + KalkBerKost(2,kalknumtt.ANTAL * kalknumsubtt.KOSTNAD) + kalknumsubtt.AVRUND.
               END.
               ELSE DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + KalkBerKost(1,kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalkylprisertt.PRIS)
                  + KalkBerKost(2,kalknumtt.ANTAL * kalknumsubtt.KOSTNAD) + kalknumsubtt.AVRUND.
                       
               END.   
            END.
            ELSE DO:
               IF kalknumtt.ARBKOD = "EGEN" THEN DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + (kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalknumsubtt.PRIS) 
                  + (kalknumtt.ANTAL * kalknumsubtt.KOSTNAD) + kalknumsubtt.AVRUND.
               END.
               ELSE DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + (kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalkylprisertt.PRIS) 
                  + (kalknumtt.ANTAL * kalknumsubtt.KOSTNAD) + kalknumsubtt.AVRUND.
               END.      
            END.    
            /*kalkberin*/
            kalkkostnad.FRITOTKOST = kalkkostnad.FRITOTKOST + (kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL * kalknumsubtt.FRIPRIS) 
            + (kalknumtt.ANTAL * kalknumsubtt.FRIKOSTNAD) + kalknumsubtt.FRIAVRUND.
            totkostvar = totkostvar + kalkkostnad.TOTKOST.
            fritotkostvar = fritotkostvar + kalkkostnad.FRITOTKOST.
         END.
         FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalknumtt.KLOGSUBID AND kalkylprisertt.KVID = 0 AND kalkylprisertt.SOKBENAMNING = "RÖRLIGKOSTNAD EA"  NO-LOCK NO-ERROR.
         IF AVAILABLE kalkylprisertt THEN DO:            
            IF kalkantal.KVID = 2 THEN DO:
               ASSIGN
               kalkkostnad.FRIEAMANGD = kalkantal.FRISUMMA
               kalkkostnad.EAMANGD = kalkantal.SUMMA.
            END.
            IF kalkantal.KVID = 3 OR kalkantal.KVID = 6 OR kalkantal.KVID = 7 THEN DO:
               ASSIGN
               kalkkostnad.FRIEAMANGD = kalkkostnad.FRITOTKOST / kalkylprisertt.PRIS
               kalkkostnad.EAMANGD = kalkkostnad.TOTKOST / kalkylprisertt.PRIS.
            END.                                
         END.
         ELSE DO:
            ASSIGN
            kalkkostnad.EAMANGD  = 0
            kalkkostnad.FRIEAMANGD = 0.        
         END.         
         GET NEXT ksq NO-LOCK.
      END.            
      IF allakoder = FALSE THEN DO:
         ASSIGN
         kalknumtt.FRITOTKOST = fritotkostvar
         kalknumtt.TOTKOST = totkostvar.   
         EMPTY TEMP-TABLE kalkantal NO-ERROR.
         EMPTY TEMP-TABLE kalkkostnad NO-ERROR.       
      END.     
  END METHOD.
  /*RÄKNA MED FAKTOR*/
  METHOD PUBLIC DECIMAL KalkBerAntal(INPUT kalktimantal AS DECIMAL):
     IF Guru.GlobalaVariabler:FranUppf = TRUE THEN RETURN kalktimantal.
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" THEN DO:
        FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
        IF AVAILABLE kalkfaktorertt THEN DO:
           RETURN kalktimantal * kalkfaktorertt.FAKTOR .             
        END.
     END.     
     RETURN  kalktimantal.
  END METHOD.   
  /*RÄKNA MED EGANAPRISER*/
  METHOD PUBLIC DECIMAL KalkBerKost(INPUT vad AS INTEGER, INPUT kalktimkost AS DECIMAL):
     IF Guru.GlobalaVariabler:FranUppf = TRUE THEN RETURN kalktimkost.
     IF vad = 1 THEN DO:       
        IF kalknumtt.ARBKOD = "EGEN" THEN.
        ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisEgnaPriser") = "yes" THEN DO:               
           FIND FIRST kalkegnaprisertt  WHERE  
           kalkegnaprisertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
           IF AVAILABLE kalkegnaprisertt THEN DO:
              IF kalkylprisertt.EGENPRISUPP = TRUE THEN DO:
                 IF kalkylprisertt.PRIS = 0 THEN kalktimkost = kalkegnaprisertt.PRIS * kalknumsubtt.TIMMAR * kalknumtt.ANTAL.
                 ELSE kalktimkost = kalkegnaprisertt.PRIS * (kalktimkost / kalkylprisertt.PRIS).                   
              END.
           END.   
        END.   
     END.
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" THEN DO:
        FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
        IF AVAILABLE kalkfaktorertt THEN DO:
           RETURN kalktimkost * kalkfaktorertt.FAKTOR .             
        END.
     END.     
     RETURN  kalktimkost.
  END METHOD.   
   /*ny koder i kalkyl vid buttenover*/
  METHOD PUBLIC VOID CreateKoderRows(antalvar AS DECIMAL):
     DEFINE VARIABLE rmat AS INTEGER NO-UNDO.
     
     rmat = THIS-OBJECT:CONTROL:CurrentMatris.
     IF rmat EQ -1 THEN rmat = 1.
     FOR EACH kalknumtt WHERE kalknumtt.MATRIS = 0 NO-LOCK: 
        FIND LAST kalknumttbuf USE-INDEX NUM NO-ERROR.
        IF AVAILABLE kalknumttbuf THEN kalknumtt.NUM = kalknumttbuf.NUM + 1.
        ELSE kalknumtt.NUM = 1.
        ASSIGN
        kalknumtt.KALKNR  = kalkhuvtt.KALKNR
        kalknumtt.OMRADE  = kalkhuvtt.OMRADE
        kalknumtt.TYPKALK = kalkhuvtt.TYPKALK /* asdasd*/
        kalknumtt.MARKNING = kalkylarbkodertt.MARKNING
        kalknumtt.MARKSUB = kalkylarbkodertt.MARKSUB
        kalknumtt.MATRIS  = rmat
        kalknumtt.ANTAL   = antalvar.
        EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.   
       
        RUN skapanumsub_UI IN AppServerHandle (INPUT kalknumtt.KLOGSUBID,INPUT kalknumtt.ARBKOD,INPUT kalknumtt.LOPNR,OUTPUT TABLE ekalknumsubtt).
        
        FOR EACH ekalknumsubtt WHERE NO-LOCK:
           FIND LAST kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
           IF AVAILABLE kalknumsubtt THEN ekalknumsubtt.NUMSUBID =  kalknumsubtt.NUMSUBID + 1.
           ELSE  ekalknumsubtt.NUMSUBID = 1.
           ASSIGN 
           ekalknumsubtt.KALKNR = kalknumtt.KALKNR
           ekalknumsubtt.OMRADE = kalknumtt.OMRADE
           ekalknumsubtt.NUM = kalknumtt.NUM.
           CREATE kalknumsubtt.
           BUFFER-COPY ekalknumsubtt TO kalknumsubtt.
           kalknumsubtt.TTRECID = RECID(kalknumsubtt).
           DELETE ekalknumsubtt.                      
        END.
        EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.  
        THIS-OBJECT:RaknaEnKod(FALSE).                      
     END.
     
     THIS-OBJECT:KalkSpara().
    
     /*problem  
     RUN LaddaKalkyl IN AppServerHandle (INPUT CONTROL:KalkNrvar,INPUT CONTROL:Omradevar,OUTPUT DATASET KalkylDS).
     FIND FIRST kalkhuvtt NO-LOCK NO-ERROR.
    ccc*/
  END METHOD.
  
   /*ny koder i kalkyl viA IMPORT/KOPIERA KONVERTERA*/
  METHOD PUBLIC VOID CreateKodRow(INPUT kodttR AS RECID):
     /*
     FIND FIRST KalkylimportTTbuff WHERE KalkylimportTTbuff.TTRECID =  importtR NO-LOCK NO-ERROR.
     IF NOT AVAILABLE KalkylimportTTbuff THEN DO:
        RETURN.
     END.
     */
     FIND FIRST kalknumtt WHERE RECID(kalknumtt) =  kodttR NO-LOCK NO-ERROR.
     FIND LAST kalknumttbuf USE-INDEX NUM NO-ERROR.
     IF AVAILABLE kalknumttbuf THEN kalknumtt.NUM = kalknumttbuf.NUM + 1.
     ELSE kalknumtt.NUM = 1.
     ASSIGN
     kalknumtt.TTRECID    =  kodttR
     kalknumtt.KALKNR     = kalkhuvtt.KALKNR
     kalknumtt.OMRADE     = kalkhuvtt.OMRADE
     kalknumtt.TYPKALK    = kalkhuvtt.TYPKALK.
     
     /*
     kalknumtt.MARKNING   = KalkylimportTTbuff.MARKNING
     kalknumtt.MARKSUB    = KalkylimportTTbuff.MARKSUB
     kalknumtt.MATRIS     = KalkylimportTTbuff.MATRIS
     kalknumtt.ANTAL      = KalkylimportTTbuff.ANTAL
     kalknumtt.ANMARKNING = KalkylimportTTbuff.ANMARKNING
     kalknumtt.RISK       = KalkylimportTTbuff.RISK
     kalknumtt.VINST      = KalkylimportTTbuff.VINST.
     */
     EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.   
     
     RUN skapanumsub_UI IN AppServerHandle (INPUT kalknumtt.KLOGSUBID,INPUT kalknumtt.ARBKOD,INPUT kalknumtt.LOPNR,OUTPUT TABLE ekalknumsubtt).
     FOR EACH ekalknumsubtt WHERE NO-LOCK:
        FIND LAST kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
        IF AVAILABLE kalknumsubtt THEN ekalknumsubtt.NUMSUBID =  kalknumsubtt.NUMSUBID + 1.
        ELSE  ekalknumsubtt.NUMSUBID = 1.
        ASSIGN 
        ekalknumsubtt.KALKNR = kalknumtt.KALKNR
        ekalknumsubtt.OMRADE = kalknumtt.OMRADE
        ekalknumsubtt.NUM = kalknumtt.NUM.
        CREATE kalknumsubtt.
        BUFFER-COPY ekalknumsubtt TO kalknumsubtt.
        kalknumsubtt.TTRECID = RECID(kalknumsubtt).
        DELETE ekalknumsubtt.                      
     END.
     EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.  
     THIS-OBJECT:RaknaEnKod(FALSE). 
                     
     THIS-OBJECT:KalkSpara().
     /*problem ccc*/
     FIND FIRST kalknumtt WHERE RECID(kalknumtt) =  kodttR NO-LOCK NO-ERROR.
     RUN LaddaKalkyl IN AppServerHandle (INPUT kalknumtt.KALKNR,INPUT kalknumtt.OMRADE,OUTPUT DATASET KalkylDS).
     FIND FIRST kalkhuvtt NO-LOCK NO-ERROR. 
    
  END METHOD. 
   
  /*ny egen kod*/
   METHOD PUBLIC ROWID CreateEgenKod():
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      DEFINE VARIABLE rmat AS INTEGER NO-UNDO.
      DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
      DEFINE VARIABLE lopnrvar AS INTEGER NO-UNDO.
      rmat = THIS-OBJECT:CONTROL:CurrentMatris.
      IF rmat EQ -1 THEN rmat = 1.
      FIND LAST kalknumtt USE-INDEX NUM NO-ERROR.
      IF AVAILABLE kalknumtt THEN numvar = kalknumtt.NUM + 1.
      ELSE numvar = 1.
   /* Hitta senaste löpnummer */
      FIND LAST kalknumtt WHERE kalknumtt.ARBKOD = "EGEN"
      USE-INDEX ARBKOD NO-LOCK NO-ERROR.
      IF AVAILABLE kalknumtt THEN lopnrvar = kalknumtt.LOPNR + 1.
      ELSE DO:
         IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" THEN lopnrvar = 10.
         ELSE lopnrvar = 1.
      END.
      FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID = kalkhuvtt.KLOGID NO-LOCK NO-ERROR.
      CREATE kalknumtt.        
      ASSIGN
      kalknumtt.KALKNR  = kalkhuvtt.KALKNR
      kalknumtt.OMRADE  = kalkhuvtt.OMRADE
      kalknumtt.TYPKALK = kalkhuvtt.TYPKALK
      kalknumtt.NUM     = numvar
      kalknumtt.LOPNR   = lopnrvar
      kalknumtt.MATRIS  = rmat
      kalknumtt.ARBKOD = "EGEN"
      kalknumtt.ANTAL   = 1
      kalknumtt.BENAMNING = "< Ange Ny Benämning >"
      kalknumtt.ENHET = "ST"
      kalknumtt.TTRECID   = RECID(kalknumtt).
      kalknumtt.KLOGSUBID = kalkylkatalogtt.HKLOGSUBID.
      returner = ROWID(kalknumtt). 
      THIS-OBJECT:KalkSpara().
      RETURN returner. 
   END METHOD.
   /*skapa nya kalknumsubbar i egen*/
   METHOD PUBLIC VOID CreateValdaRows():
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      DEFINE VARIABLE pristypvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvarny AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ejmed AS LOGICAL NO-UNDO.
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:Control:GridEgnaKoder:ActiveRow:Cells["NUM"]:TEXT).
      FIND FIRST kalknumtt WHERE kalknumtt.NUM = hittnum NO-LOCK NO-ERROR.
      FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = 0 NO-LOCK:                                                   
         ASSIGN 
         pristypvar = "".
         ejmed = FALSE.
         ASSIGN
         kalknumsubtt.KALKNR = kalknumtt.KALKNR
         kalknumsubtt.OMRADE = kalknumtt.OMRADE
         kalknumsubtt.TTRECID = RECID(kalknumsubtt).
         IF kalknumsubtt.PRIS NE 0 THEN kalknumsubtt.TIMMAR = 1.
         ASSIGN
         kalknumsubtt.FRIBENAMNING = kalknumsubtt.BENAMNING
         kalknumsubtt.FRIPRIS = kalknumsubtt.PRIS
         kalknumsubtt.FRIAVRUND  = kalknumsubtt.AVRUND 
         kalknumsubtt.FRITIMMAR = kalknumsubtt.TIMMAR
         kalknumsubtt.FRIKOSTNAD = kalknumsubtt.KOSTNAD. 
         IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
            pristypvar = THIS-OBJECT:PrisTyp(INPUT kalknumsubtt.KPID).
            IF pristypvar = "ENTREP" THEN ejmed = TRUE.    
         END.   
         IF ejmed = FALSE THEN DO:
            IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
               pristypvarny = THIS-OBJECT:PrisTyp(INPUT kalknumsubtt.KPID).
               FOR EACH kalknumsubttbuf WHERE kalknumsubttbuf.NUM = kalknumtt.NUM AND kalknumsubttbuf.NUM > 0:
                  pristypvar = THIS-OBJECT:PrisTyp(INPUT kalknumsubttbuf.KPID).
                  IF pristypvar BEGINS "MASKIN" AND pristypvarny BEGINS "MASKIN" THEN ejmed = TRUE.
               END.   
            END.
         END.    
         IF ejmed = TRUE THEN DO:                            
            res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(49) + " " + kalknumsubtt.BENAMNING, "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
            DELETE kalknumsubtt.            
         END.
         ELSE DO:   
            kalknumsubtt.NUM    = kalknumtt.NUM.                                                                
            FIND LAST kalknumsubttbuf WHERE kalknumsubttbuf.NUM = kalknumtt.NUM AND kalknumsubttbuf.NUM > 0 USE-INDEX NUM NO-LOCK NO-ERROR.
            IF AVAILABLE kalknumsubttbuf THEN kalknumsubtt.NUMSUBID =  kalknumsubttbuf.NUMSUBID + 1.
            ELSE  kalknumsubtt.NUMSUBID = 1.  
            THIS-OBJECT:RaknaEnKod(FALSE).       
         END.   
      END.                  
      THIS-OBJECT:KalkSpara().
   END METHOD.
   /*uppdatering av t.ex. antal i kalknum både vanlig och egen.*/   
   METHOD PUBLIC VOID UpdateKod():
      THIS-OBJECT:RaknaEnKod(FALSE).
      THIS-OBJECT:KalkSpara().
   END METHOD.
   /*uppdatering av t.ex. antal,pris i kalknumsub både vanlig och egen.*/
   METHOD PUBLIC VOID UpdateKodSub():
      THIS-OBJECT:RaknaEnKod(FALSE).
      THIS-OBJECT:KalkSpara().
   END METHOD.
   
   /*TAR BORT NUMSUBBAR*/
   METHOD PUBLIC VOID RemoveKoderRow(INPUT villkor AS CHARACTER):
      DEFINE VARIABLE mat AS INTEGER NO-UNDO.
      
      mat = THIS-OBJECT:Control:CurrentMatris.
     
      THIS-OBJECT:Control:GridKalkylKoder:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
      FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM NO-LOCK:
         DELETE kalknumsubtt.
      END.
      /*
      DELETE kalknumtt.
      THIS-OBJECT:KalkSpara().
      */
      
   END METHOD.
   /*TAR BORT NUMSUBBAR*/
   METHOD PUBLIC VOID RemoveKoderRowROWID(INPUT gridrowid AS ROWID):
      KoderTTh:FIND-BY-ROWID(gridrowid).
      FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM NO-LOCK:
         DELETE kalknumsubtt.
      END.
      /*
      DELETE kalknumtt.
      THIS-OBJECT:KalkSpara().
      */ 
      
   END METHOD.
    
   /*TAR BORT EGEN NUMSUB*/
   METHOD PUBLIC VOID RemoveValdaRowEgna (INPUT villkor AS CHARACTER):
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      DEFINE VARIABLE kostvar AS DECIMAL NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:Control:GridEgnaKoder:ActiveRow:Cells["NUM"]:TEXT).
      FIND FIRST kalknumtt WHERE kalknumtt.NUM = hittnum NO-LOCK NO-ERROR.
      THIS-OBJECT:Control:GridValdaUnderKoder:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
      kostvar = kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalknumsubtt.PRIS + kalknumtt.ANTAL * kalknumsubtt.KOSTNAD.
      ASSIGN 
      kalknumtt.TOTKOST = kalknumtt.TOTKOST - kostvar
      kalknumtt.FRITOTKOST = kalknumtt.FRITOTKOST - kostvar. 
     
   END METHOD.
   /*TAR BORT EGEN KOD*/
   METHOD PUBLIC VOID RemoveEgenKod():
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:Control:GridEgnaKoder:ActiveRow:Cells["NUM"]:Text).
      FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = hittnum NO-LOCK:
         DELETE kalknumsubtt.
      END.
      THIS-OBJECT:KalkSpara().
   END METHOD.
   
   /*TAR BORT NUMSUBBAR VID buttonBackMark_Click DVS KODER*/
   METHOD PUBLIC VOID RemoveKoderSelected():
      DEFINE VARIABLE mat AS INTEGER NO-UNDO.
     
      mat = THIS-OBJECT:Control:CurrentMatris.
              
      {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:Control:GridKalkylKoder:Selected:Rows}
         THIS-OBJECT:RemoveKoderRow(villkor).
      END. /*foreach end*/    
      IF oObject = ? THEN DO:                                                   
         THIS-OBJECT:RemoveKoderRowROWID(KoderTTh:ROWID).
      END.   
   END METHOD.
   /*TAR BORT NUMSUBBAR VID buttonBackMarkPriser_Click DVS VIA EGNAKODER*/
   METHOD PUBLIC VOID RemoveValdaSelectedEgna ():                    
      EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.
      {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:Control:GridValdaUnderKoder:Selected:Rows}                                         
         THIS-OBJECT:RemoveValdaRowEgna (villkor).
      END. /*foreach end*/
   END METHOD.
  /* buttonBackMarkPriser_Click buttonBackAllaPriser_Click*/
   METHOD PUBLIC VOID RemoveValdaAllaEgna ():    
      EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.  
       {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:Control:GridValdaUnderKoder:Rows}                                         
         THIS-OBJECT:RemoveValdaRowEgna (villkor).
      END. /*foreach end*/     
   END METHOD.
   /*SKAPA MRTL*/
   METHOD PUBLIC VOID CreatevalMaterial(INPUT kalknr AS INTEGER,INPUT omr AS CHARACTER, berantal AS  DECIMAL):
      DEFINE VARIABLE cm AS INTEGER NO-UNDO.
      cm = THIS-OBJECT:CONTROL:CurrentMatris.
      IF cm EQ -1 THEN cm = 1.
      FOR EACH kalktmtrlTT WHERE kalktmtrlTT.MATRIS = 0 NO-LOCK:
         ASSIGN 
         kalktmtrlTT.OMRADE    = omr
         kalktmtrlTT.KALKNR    = kalknr
         kalktmtrlTT.MATRIS    = cm.
         IF kalktmtrlTT.BERKVANT = 0 THEN kalktmtrlTT.BERKVANT  = berantal.
         kalktmtrlTT.TTRECID   = RECID(kalktmtrlTT).
         FIND LAST kalktmtrlTTbuf USE-INDEX MID NO-LOCK NO-ERROR.
         IF AVAILABLE kalktmtrlTTbuf THEN DO:
            kalktmtrlTT.MID = kalktmtrlTTbuf.MID + 1.
         END.   
         ELSE kalktmtrlTT.MID = 1.
      END.
      THIS-OBJECT:KalkSpara().
   END METHOD.
   /*BERÄKNING AV MTRL*/
   METHOD PUBLIC DECIMAL MtrlKost(INPUT imatris AS INTEGER):
      IF imatris = ? THEN DO:
         FOR EACH kalktmtrlTT BREAK BY kalktmtrlTT.BREAKDUMMY:
            kalktmtrlTT.SUMMA = kalktmtrlTT.NPRIS *  kalktmtrlTT.BERKVANT.
            ACCUMULATE kalktmtrlTT.SUMMA (TOTAL BY kalktmtrlTT.BREAKDUMMY).
            IF LAST-OF(kalktmtrlTT.BREAKDUMMY) THEN DO:
               IF Guru.GlobalaVariabler:FranUppf = TRUE THEN RETURN ACCUM TOTAL BY kalktmtrlTT.BREAKDUMMY kalktmtrlTT.SUMMA.
               ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" THEN DO:
                  FIND FIRST kalkylprisertt WHERE kalkylprisertt.SOKBENAMNING = "Materiel" NO-LOCK NO-ERROR.
                  IF AVAILABLE kalkylprisertt THEN DO:
                     FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
                     IF AVAILABLE kalkfaktorertt THEN DO:
                        RETURN (ACCUM TOTAL BY kalktmtrlTT.BREAKDUMMY kalktmtrlTT.SUMMA) * kalkfaktorertt.FAKTOR .             
                     END.
                  END.                                     
               END.
               ELSE RETURN ACCUM TOTAL BY kalktmtrlTT.BREAKDUMMY kalktmtrlTT.SUMMA.   
            END.                   
         END.
      END.
      ELSE DO:              
         FOR EACH kalktmtrlTT WHERE kalktmtrlTT.MATRIS = imatris BREAK BY kalktmtrlTT.MATRIS:
            kalktmtrlTT.SUMMA = kalktmtrlTT.NPRIS *  kalktmtrlTT.BERKVANT.
            ACCUMULATE kalktmtrlTT.SUMMA (TOTAL BY kalktmtrlTT.MATRIS).
            IF LAST-OF(kalktmtrlTT.MATRIS) THEN DO:
               IF Guru.GlobalaVariabler:FranUppf = TRUE THEN RETURN ACCUM TOTAL BY kalktmtrlTT.MATRIS kalktmtrlTT.SUMMA.
               ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" THEN DO:
                  FIND FIRST kalkylprisertt WHERE kalkylprisertt.SOKBENAMNING = "Materiel" NO-LOCK NO-ERROR.
                  IF AVAILABLE kalkylprisertt THEN DO:
                     FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
                     IF AVAILABLE kalkfaktorertt THEN DO:
                        RETURN (ACCUM TOTAL BY kalktmtrlTT.MATRIS kalktmtrlTT.SUMMA) * kalkfaktorertt.FAKTOR .             
                     END.
                  END.                                     
               END.
               ELSE RETURN ACCUM TOTAL BY kalktmtrlTT.MATRIS kalktmtrlTT.SUMMA.                 
            END.                   
         END.
      END.
   END METHOD.
   /*VID VISNING*/
   METHOD PUBLIC VOID CalculateSum():
     THIS-OBJECT:RaknaAllaKoder().         
     THIS-OBJECT:kalkantalTTh = TEMP-TABLE kalkantal:HANDLE:DEFAULT-BUFFER-HANDLE.
     THIS-OBJECT:kalkkostnadTTh = TEMP-TABLE kalkkostnad:HANDLE:DEFAULT-BUFFER-HANDLE.     
     THIS-OBJECT:KalkRubrikTTh = TEMP-TABLE KalkRubrikTT:HANDLE:DEFAULT-BUFFER-HANDLE.
   END METHOD.
   /*GER SOKBENAMNING FÖR NUMSUB*/
   METHOD PUBLIC CHARACTER PrisTyp(kpidvar AS INTEGER):
      FIND FIRST kalkylprisertt WHERE kalkylprisertt.KPID = kpidvar NO-LOCK NO-ERROR.
      RETURN kalkylprisertt.SOKBENAMNING. 
   END METHOD.
   /*TILL AVTALSKALKYLEN*/
   METHOD PUBLIC VOID AvtalKalkAo(INPUT exelkommando AS CHARACTER, INPUT avtalid AS INTEGER, INPUT matrisvar AS LOGICAL):
      FIND FIRST Avtalskalktt  WHERE Avtalskalktt.ID = avtalid NO-LOCK NO-ERROR.       
      EMPTY TEMP-TABLE AvtalKodertt NO-ERROR. 
      IF matrisvar = TRUE THEN DO:
         FOR EACH kalknumtt BREAK BY kalknumtt.MATRIS BY kalknumtt.ARBKOD BY kalknumtt.LOPNR:
            ACCUMULATE kalknumtt.ANTAL (TOTAL BY kalknumtt.MATRIS BY kalknumtt.ARBKOD BY kalknumtt.LOPNR). 
            IF LAST-OF(kalknumtt.LOPNR) THEN DO:
               CREATE AvtalKodertt.      
               BUFFER-COPY kalknumtt TO AvtalKodertt.
               AvtalKodertt.ANTAL = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.ANTAL).                      
             END.     
         END.
      END.
      ELSE DO:
         FOR EACH kalknumtt BREAK BY kalknumtt.ARBKOD BY kalknumtt.LOPNR:
            ACCUMULATE kalknumtt.ANTAL (TOTAL BY kalknumtt.ARBKOD BY kalknumtt.LOPNR). 
            IF LAST-OF(kalknumtt.LOPNR) THEN DO:
               CREATE AvtalKodertt.      
               BUFFER-COPY kalknumtt TO AvtalKodertt.
               AvtalKodertt.ANTAL = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.ANTAL).                      
             END.     
         END.
      END.   
      FOR EACH AvtalKodertt,
      EACH kalknumtt WHERE kalknumtt.ARBKOD = AvtalKodertt.ARBKOD AND kalknumtt.LOPNR = AvtalKodertt.LOPNR:
         IF kalknumtt.ANMARKNING NE "" THEN AvtalKodertt.ANMARKNING = AvtalKodertt.ANMARKNING + " " + kalknumtt.ANMARKNING.
      END.  
   END METHOD.
   /*START FÖR EONES AVTAL*/
   METHOD PUBLIC VOID EonesAvtal(INPUT-OUTPUT sterad AS INTEGER,INPUT-OUTPUT slerad  AS INTEGER,INPUT-OUTPUT flik1 AS INTEGER,INPUT-OUTPUT flik2 AS INTEGER,
                                  INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER ,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,
                                  INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):          
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      RUN esmallhmt_UI IN AppServerHandle (OUTPUT sterad,OUTPUT slerad,OUTPUT flik1,OUTPUT flik2 ).
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "E136" :
         FIND FIRST avtbuff WHERE avtbuff.ARBKOD = "136" AND avtbuff.LOPNR = AvtalKodertt.LOPNR AND avtbuff.MATRIS = AvtalKodertt.MATRIS NO-ERROR. 
         IF NOT AVAILABLE avtbuff THEN DO:
            CREATE avtbuff.
            BUFFER-COPY AvtalKodertt TO avtbuff.
            ASSIGN
            avtbuff.ANTAL = 0
            avtbuff.ARBKOD = "136".
         END.
         ASSIGN 
         avtbuff.ANTAL = avtbuff.ANTAL + AvtalKodertt.ANTAL / 10000.
         DELETE AvtalKodertt.
      END.
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN":
         AvtalKodertt.ARBKOD = "FRI".
      END.
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EFRI":
         ASSIGN
         AvtalKodertt.ARBKOD = "FRI"
         AvtalKodertt.LOPNR = AvtalKodertt.LOPNR - 97.
         /* skall ligga    FRI03-FRI09*/
         /* lena 20120216
         AvtalKodertt.LOPNR = AvtalKodertt.LOPNR - 60.      */
      END.     
   END METHOD.
   METHOD PUBLIC VOID InfraAvtal(INPUT-OUTPUT sterad1 AS INTEGER,INPUT-OUTPUT slerad1 AS INTEGER,OUTPUT sterad2 AS INTEGER,INPUT-OUTPUT slerad2 AS INTEGER,INPUT-OUTPUT sterad3 AS INTEGER,
                                INPUT-OUTPUT slerad3 AS INTEGER,INPUT-OUTPUT frist AS INTEGER,INPUT-OUTPUT flik1 AS INTEGER,INPUT-OUTPUT flik2 AS INTEGER,
                                INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER ,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,
                                INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):          
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      RUN inframallhmt_UI IN AppServerHandle (OUTPUT sterad1,OUTPUT slerad1,OUTPUT sterad2,OUTPUT slerad2,OUTPUT sterad3,OUTPUT slerad3,OUTPUT frist,OUTPUT flik1,OUTPUT flik2 ).
   
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN":
         AvtalKodertt.ARBKOD = "FRI".
      END.
   END METHOD.
   METHOD PUBLIC VOID LimoAvtal(INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):          
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN":
         AvtalKodertt.ARBKOD = "FRI".
      END.
   END METHOD.
   METHOD PUBLIC VOID BeredningMtrl():          
      DEFINE VARIABLE nyber AS LOGICAL NO-UNDO.
      DEFINE VARIABLE beromr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE bernr AS INTEGER NO-UNDO.
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN RUN BeredningMtrl IN AppServerExtraHandle (INPUT CONTROL:KalkNrvar,INPUT CONTROL:Omradevar, OUTPUT nyber, OUTPUT beromr, OUTPUT bernr ).
      RUN STARTKALKBER.P (INPUT nyber,INPUT CONTROL:Omradevar,INPUT CONTROL:KalkNrvar,INPUT beromr,INPUT bernr,INPUT  STRING(THIS-OBJECT:CONTROL:HuvudBenamning:GuruText:Text)). 
   END METHOD.
   METHOD PUBLIC LOGICAL BeredningMtrlfinns():          
      DEFINE VARIABLE nyber AS LOGICAL NO-UNDO.
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN RUN BeredningMtrlfinns IN AppServerExtraHandle (INPUT CONTROL:KalkNrvar,INPUT CONTROL:Omradevar, OUTPUT nyber).  
      RETURN nyber.     
   END METHOD.
   METHOD PUBLIC VOID BeredningMtrlHmt(OUTPUT totbermtrl AS DECIMAL): 
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN RUN BeredningMtrlHmt IN AppServerExtraHandle (INPUT CONTROL:KalkNrvar,INPUT CONTROL:Omradevar,OUTPUT totbermtrl,OUTPUT TABLE berkalkmtrltt).
   END METHOD.
   
   METHOD PUBLIC VOID KonverteraKalkyl(INPUT nykattyp AS INTEGER,INPUT nyomrvar AS CHARACTER,OUTPUT felmed AS LOGICAL): 
      DEFINE VARIABLE nykalknr AS INTEGER NO-UNDO.
      RUN KonverteraKalkyl_UI IN AppServerHandle (INPUT CONTROL:KalkNrvar,INPUT CONTROL:Omradevar,INPUT nykattyp,INPUT nyomrvar,OUTPUT nykalknr,
      OUTPUT TABLE kalknumtt,OUTPUT TABLE tidut).
      /*
      THIS-OBJECT:CONTROL:FelMeddCheck().
      FIND FIRST tidut NO-LOCK NO-ERROR.
      IF AVAILABLE tidut THEN DO:
         felmed = TRUE.
         RETURN.
      END.
      */
      THIS-OBJECT:anvkalkyl(INPUT nykalknr,INPUT nyomrvar).  
      FOR EACH kalknumtt WHERE NO-LOCK:
         IF kalknumtt.ARBKOD = "EGEN" THEN.
         ELSE DO:
            FIND FIRST KalkylimportTT WHERE KalkylimportTT.MATRIS = kalknumtt.MATRIS AND KalkylimportTT.ARBKOD = kalknumtt.ARBKOD AND 
            KalkylimportTT.LOPNR = kalknumtt.LOPNR 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE KalkylimportTT THEN DO:
               CREATE KalkylimportTT.
               KalkylimportTT.TTRECID = RECID(KalkylimportTT).
            END.
            BUFFER-COPY kalknumtt EXCEPT ANTAL TO KalkylimportTT.
            KalkylimportTT.ANTAL = KalkylimportTT.ANTAL + kalknumtt.ANTAL.
         END.   
      END. 
      THIS-OBJECT:CONTROL:LoadKalkyl(INPUT nykalknr, INPUT nyomrvar, INPUT FALSE).
      
   END METHOD.
   
   METHOD PUBLIC VOID Kopierakalkyl(INPUT nykatidvar AS INTEGER,INPUT nyomrvar AS CHARACTER,OUTPUT felmed AS LOGICAL): 
      DEFINE VARIABLE nykalknr AS INTEGER NO-UNDO.
      RUN kopierakalkyl_UI IN AppServerHandle (INPUT CONTROL:KalkNrvar,INPUT CONTROL:Omradevar,INPUT nykatidvar,INPUT nyomrvar,OUTPUT nykalknr,OUTPUT TABLE tidut).
      /*     
      THIS-OBJECT:CONTROL:FelMeddCheck().
      FIND FIRST tidut NO-LOCK NO-ERROR.
      IF AVAILABLE tidut THEN DO:
         felmed = TRUE.
         RETURN.
      END.
      */
      THIS-OBJECT:anvkalkyl(INPUT nykalknr,INPUT nyomrvar).  
      FOR EACH kalknumtt WHERE NO-LOCK:
         IF kalknumtt.ARBKOD = "EGEN" THEN.
         ELSE DO:
            CREATE KalkylimportTT.
            BUFFER-COPY kalknumtt TO KalkylimportTT.
            KalkylimportTT.TTRECID = RECID(KalkylimportTT).
         END.   
      END. 
      THIS-OBJECT:CONTROL:LoadKalkyl(INPUT nykalknr, INPUT nyomrvar, INPUT FALSE).         
   END METHOD.
   METHOD PUBLIC CHARACTER BortTagenPersonal(INPUT pkodNamnvar AS CHARACTER):
      RUN BortTagenPersonal_UI IN AppServerHandle (INPUT-OUTPUT pkodNamnvar).
      RETURN pkodNamnvar.
   END METHOD.
   METHOD PUBLIC VOID anvkalkyl (INPUT nykalknr AS INTEGER, INPUT nyomrvar AS CHARACTER):       
      RUN anvkalkyl_UI IN AppServerHandle (INPUT nykalknr,INPUT nyomrvar).      
   END METHOD.
   
   METHOD PUBLIC VOID anvkalkylhmt(OUTPUT TABLE anvkalkyltt):       
       RUN anvkalkylhmt_UI IN AppServerHandle (OUTPUT TABLE anvkalkyltt).    
   END METHOD.
END CLASS.