 
 /*------------------------------------------------------------------------
    File        : Arendedb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Mon Feb 13 13:20:42 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Modules.Arende.Arendedb: 
   {ARENDEKAT.I}
   {ARENDEKATH.i}
   {ARENDEPRODATA.i}
   {ARENDEKATPRODATA.i}
   DEFINE TEMP-TABLE kalkylstopptt NO-UNDO
      FIELD ARENDENR AS INTEGER
      FIELD OMRADE AS CHARACTER.
   DEFINE PRIVATE VARIABLE WinApi AS HANDLE NO-UNDO.
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO 
   PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC  PROPERTY AppServerExtraHandle AS HANDLE NO-UNDO
   PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO.
   
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.Arende.ArendeShell NO-UNDO.
   DEFINE PUBLIC PROPERTY kopplataonr AS CHARACTER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC PROPERTY kopplatplnr AS CHARACTER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC PROPERTY kopplatdelnr AS INTEGER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC PROPERTY kopplatartal AS INTEGER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   
   CONSTRUCTOR PUBLIC Arendedb(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Connect().
      ASSIGN 
      kopplataonr = ?
      kopplatdelnr = ?
      kopplatplnr = ?
      kopplatartal = ?.
   END CONSTRUCTOR.
   
   DESTRUCTOR Arendedb():
      DEFINE VARIABLE lasavanv AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lasavnamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lastav AS LOGICAL NO-UNDO.
      FOR EACH kalkylstopptt WHERE NO-LOCK:
         RUN KalkStopp_UI IN AppServerHandle (INPUT 2,INPUT kalkylstopptt.ARENDENR,INPUT kalkylstopptt.OMRADE, INPUT Guru.Konstanter:globanv, OUTPUT lasavanv, OUTPUT lasavnamn, OUTPUT lastav).
      END.
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DELETE PROCEDURE THIS-OBJECT:AppServerHandle.
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN DELETE PROCEDURE THIS-OBJECT:AppServerExtraHandle.
      
   END DESTRUCTOR.
   
   /* Gör en koppling till appserver */
   METHOD PUBLIC VOID Connect():
      IF Guru.Konstanter:appcon THEN DO:
         RUN ARENDEBERAPPDS.p PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).         
      END.
      ELSE DO:
         RUN ARENDEBERAPPDS.p PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globanv).         
      END. 
   END METHOD.
   METHOD PUBLIC VOID ArendeNrGet(OUTPUT nrvar AS INTEGER,OUTPUT omr AS CHARACTER):
      ASSIGN 
      nrvar = ControlShell:KalkNrvar
      omr = ControlShell:Omradevar.
   END METHOD.
   METHOD PUBLIC VOID InitializeTTs():
      {ARENDEKATTTH.i}
      
      
   END METHOD.
   /*HÄMTAR UPP STARTEN TILL KALKYLEN  OCH SKAPAR KALKYLHUVUD*/
   METHOD PUBLIC LOGICAL CreateArende():
      RUN LaddaArende IN AppServerHandle (INPUT ?,INPUT ?,OUTPUT DATASET ArendeDS).
      IF THIS-OBJECT:ArendeTracking(TRUE) = FALSE THEN RETURN FALSE.
      EMPTY TEMP-TABLE earendehuvtt NO-ERROR.
      RUN startny_UI IN AppServerHandle 
      (INPUT "",INPUT Guru.Konstanter:globanv,OUTPUT TABLE earendehuvtt, OUTPUT TABLE kalkylkatalogtt).
      FIND FIRST earendehuvtt NO-LOCK NO-ERROR.
      IF NOT AVAILABLE earendehuvtt THEN DO:
         RETURN FALSE.
      END.
      CREATE arendehuvtt.
      BUFFER-COPY earendehuvtt TO arendehuvtt.
      arendehuvtt.TTRECID = RECID(arendehuvtt).
      EMPTY TEMP-TABLE earendehuvtt NO-ERROR. 
      FIND FIRST arendehuvtt NO-LOCK NO-ERROR.
      ASSIGN 
      arendehuvtt.ANVANDARE                    = Guru.Konstanter:globanv
      arendehuvtt.KALKANV                      = Guru.Konstanter:globanvpkod.
   END METHOD.
   /*ÄR KALKYLEN KOPPLAD*/
   METHOD PUBLIC VOID ArendeAonr(OUTPUT koppladtill AS CHARACTER,OUTPUT koppladbest AS CHARACTER):
      DEFINE VARIABLE beredarvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE refvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE ortnamn AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE kontakt AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE arbannsv AS CHARACTER NO-UNDO.
      FIND FIRST arendehuvtt  WHERE NO-LOCK NO-ERROR.
      FIND FIRST arendeaonrTT WHERE arendeaonrTT.ARENDENR = arendehuvtt.ARENDENR AND arendeaonrTT.OMRADE = arendehuvtt.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE arendeaonrTT THEN DO TRANSACTION:
         IF arendeaonrTT.AONR NE ? THEN DO:
            Guru.Konstanter:BestKundTTH:FIND-FIRST("WHERE BESTID = '" + STRING(arendehuvtt.BESTID) + "'") NO-ERROR.
            RUN AonrInfo_UI IN AppServerHandle (INPUT arendeaonrTT.AONR,INPUT arendeaonrTT.DELNR,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
            koppladtill = arendehuvtt.BENAMNING + " Kopplad till " + Guru.Konstanter:gaok + " " + STRING(arendeaonrTT.AONR) + " " + STRING(arendeaonrTT.DELNR,Guru.Konstanter:varforetypchar[1]) + " " + 
            ortnamn + " ".
            IF Guru.Konstanter:BestKundTTh:AVAILABLE THEN DO:
               koppladbest = STRING(Guru.Konstanter:BestKundTTh:BUFFER-FIELD("BESTNAMN"):BUFFER-VALUE) + " " + Guru.Konstanter:grefbefk + " " + refvar.
            END.   
         END.
         ELSE koppladtill = arendehuvtt.BENAMNING. 
      END.  
      ELSE koppladtill = arendehuvtt.BENAMNING. 
   END METHOD.
   METHOD PUBLIC VOID TotArendeSumm():
      DEFINE VARIABLE tk AS DECIMAL NO-UNDO.
      FOR EACH arendenumttbuf BREAK BY arendenumttbuf.ARENDENR:
         ACCUMULATE arendenumttbuf.TOTKOST (TOTAL BY arendenumttbuf.ARENDENR). 
         IF LAST-OF(arendenumttbuf.ARENDENR) THEN DO:
            tk = ACCUM TOTAL BY arendenumttbuf.ARENDENR arendenumttbuf.TOTKOST.                          
         END.
      END.
      IF THIS-OBJECT:ControlShell = ? THEN.
      ELSE DO:
         
         THIS-OBJECT:ControlShell:ArendeControl:GridKalkylKoder:GuruUpdateTitle("Summa för Ärende :" + STRING(tk,"->>>>>>>>>>>>9")).
      END.  
   END METHOD.
   /*SPARA KALKYLEN*/
   METHOD PUBLIC LOGICAL ArendeSpara():
      
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:TotArendeSumm().
      THIS-OBJECT:ArendeTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET ArendeDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET ArendeDS:HANDLE).
      
      /*
      hDSChanges:WRITE-XML("FILE", "C:\CTest.xml"). 
      */
      RUN SparaProDataSetArendeDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET ArendeDS:HANDLE).
     
      IF THIS-OBJECT:ArendeTracking(TRUE) = FALSE THEN RETURN FALSE.              
      FIND FIRST arendehuvtt  WHERE NO-LOCK NO-ERROR.            
   END METHOD.
   
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
      cFile = cFile  + STRING(kalknr) + omr + arendehuvtt.BENAMNING + ".xml".
      
      ASSIGN
      cTargetType       = "FILE"
      lFormatted        = NO
      cEncoding         = ?
      cSchemaLocation   = ?
      lWriteSchema      = YES
      lMinSchema        = FALSE
      lWriteBeforeImage = FALSE.
      DATASET ArendeDS:WRITE-XML (cTargetType, cFile, lFormatted, cEncoding, cSchemaLocation, lWriteSchema, lMinSchema, lWriteBeforeImage). 
      /*EPOST*/
      dResult = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetString(84) + cFile + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(85), THIS-OBJECT:Root:LanguageManager:GetString(83), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF dResult:ToString() EQ "YES" THEN DO:
         THIS-OBJECT:XmlToMail(cFile).
      END.
      /*FIX*/
      FIND FIRST arendehuvtt  WHERE NO-LOCK NO-ERROR.    
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
            THIS-OBJECT:ArendeTracking(FALSE).
            THIS-OBJECT:LaddaArende(?,?). 
            THIS-OBJECT:ArendeTracking(TRUE).
            
            ASSIGN
            cSourceType             = "FILE"
            cReadMode               = "MERGE"
            cSchemaLocation         = ?
            lOverrideDefaultMapping = FALSE.
            DATASET ArendeDS:READ-XML (cSourceType, cFile, cReadMode, cSchemaLocation, lOverrideDefaultMapping).
            
            RETURN TRUE.
         END.
      END.
      RETURN FALSE.
   END METHOD.
   
   
   METHOD PUBLIC LOGICAL ArendeTracking(INPUT onoff AS LOGICAL):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE lasavanv AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lasavnamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lastav AS LOGICAL NO-UNDO.
      IF onoff = TRUE THEN DO:
         FIND FIRST arendehuvtt WHERE NO-LOCK NO-ERROR.
         IF AVAILABLE arendehuvtt THEN DO:
            IF arendehuvtt.ARENDENR = 0 OR arendehuvtt.ARENDENR = ? THEN. 
            ELSE DO:
               FIND FIRST kalkylstopptt WHERE kalkylstopptt.ARENDENR = arendehuvtt.ARENDENR AND kalkylstopptt.OMRADE = arendehuvtt.OMRADE NO-LOCK NO-ERROR.
               IF AVAILABLE kalkylstopptt THEN.
               ELSE DO:
                  RUN KalkStopp_UI IN AppServerHandle (INPUT 1,INPUT arendehuvtt.ARENDENR,INPUT arendehuvtt.OMRADE, INPUT Guru.Konstanter:globanv, OUTPUT lasavanv, OUTPUT lasavnamn, OUTPUT lastav).
                  IF lastav = TRUE THEN DO:
                     IF Guru.Konstanter:globanv = lasavanv THEN DO:
                        rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(168) + " " + 
                        lasavanv + " " + lasavnamn + ". " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(170)," ",
                        System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).            
                        IF rrr:ToString() EQ "Yes" THEN DO:
                           CREATE kalkylstopptt.
                           BUFFER-COPY arendehuvtt TO kalkylstopptt. 
                        END.   
                         IF rrr:ToString() EQ "No" THEN DO:
                           RETURN FALSE.
                        END.    
                         
                     END.
                     ELSE DO:  
                        rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(168) + " " +
                        lasavanv + " " + lasavnamn + ". " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(169)," ",
                        System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Warning).
                        RETURN FALSE.
                     END.
                  END.
                  ELSE DO:
                     CREATE kalkylstopptt.
                     BUFFER-COPY arendehuvtt TO kalkylstopptt.
                  END.   
               END.      
            END.
         END.       
      END.
      TEMP-TABLE arendenumtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE arendenumsubtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE arendehuvtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE arendeaonrTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE arendemtrlTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE arendetidlageTT:TRACKING-CHANGES = onoff.
                
   END METHOD.    
   /*HÄMTAR KALATLOGER MM OCH ALLT SOM RÖR KALKYLEN*/
   METHOD PUBLIC LOGICAL FetchArende(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO. 
      DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
      RUN kathmt_UI IN AppServerHandle (INPUT kalknr,INPUT omr, INPUT Guru.Konstanter:globanv,OUTPUT felmed, OUTPUT TABLE kalkylarbkodertt,
      OUTPUT TABLE kalkylloppostertt,OUTPUT TABLE kalkylkatalogtt, OUTPUT TABLE markfiltertt).
      THIS-OBJECT:FetchPriser(kalknr, omr).
      THIS-OBJECT:LaddaArende(INPUT kalknr,INPUT omr).
      
      THIS-OBJECT:AnvKalkyl (INPUT kalknr,INPUT omr).  
      IF THIS-OBJECT:ArendeTracking(TRUE) = FALSE THEN RETURN FALSE.
      
      IF felmed NE "" THEN DO:
         res = System.Windows.Forms.MessageBox:Show(felmed,"",System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.         
      END.   
      FIND FIRST arendehuvtt  WHERE NO-LOCK NO-ERROR. 
      FIND FIRST kalkylkatalogtt WHERE NO-LOCK NO-ERROR.
      FIND FIRST markfiltertt WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkylkatalogtt THEN DO:        
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(53), "", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:FetchDefaultValues("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,kalknr, omr).
      RETURN TRUE.
   END METHOD.
   /*HÄMTAR KALYL DATASET*/
   METHOD PUBLIC VOID LaddaArende(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):
      RUN LaddaArende IN AppServerHandle (INPUT kalknr,INPUT omr,OUTPUT DATASET ArendeDS).
      THIS-OBJECT:TotArendeSumm().
   END METHOD.
   METHOD PUBLIC LOGICAL FetchVisaArende(INPUT extravaldfastth AS HANDLE):
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO. 
      DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      extravaldfastth:FIND-FIRST() NO-ERROR.
      THIS-OBJECT:ControlShell:KalkNrvar = extravaldfastth:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
      THIS-OBJECT:ControlShell:Omradevar = extravaldfastth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.      
      RUN kathmt_UI IN AppServerHandle (INPUT extravaldfastth:BUFFER-FIELD("KALKNR"):BUFFER-VALUE,INPUT extravaldfastth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE, INPUT Guru.Konstanter:globanv,OUTPUT felmed, OUTPUT TABLE kalkylarbkodertt,
      OUTPUT TABLE kalkylloppostertt,OUTPUT TABLE kalkylkatalogtt, OUTPUT TABLE markfiltertt).
     
      THIS-OBJECT:FetchPriser(INPUT extravaldfastth:BUFFER-FIELD("KALKNR"):BUFFER-VALUE,INPUT extravaldfastth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE).
      
      queryvar =  "FOR EACH " + extravaldfastth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(extravaldfastth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         RUN LaddaArende IN AppServerHandle (INPUT extravaldfastth:BUFFER-FIELD("KALKNR"):BUFFER-VALUE,INPUT extravaldfastth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE,OUTPUT DATASET ArendeDS APPEND).
         
         qH:GET-NEXT().    
      END.
         
      THIS-OBJECT:ArendeTracking(FALSE).
      IF felmed NE "" THEN DO:
         res = System.Windows.Forms.MessageBox:Show(felmed,"",System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.         
      END.   
      FIND FIRST arendehuvtt  WHERE NO-LOCK NO-ERROR. 
      FIND FIRST kalkylkatalogtt WHERE NO-LOCK NO-ERROR.
      FIND FIRST markfiltertt WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkylkatalogtt THEN DO:        
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(53), "", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.
      END.
      THIS-OBJECT:Root:DatabaseManager:Global:FetchDefaultValues("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,THIS-OBJECT:ControlShell:KalkNrvar, THIS-OBJECT:ControlShell:Omradevar).
           
      RETURN TRUE.
   END METHOD.
   
   /*hämtar dataset som är skapade i method ovan*/
   METHOD PUBLIC VOID HmtLaddaArendeDS(OUTPUT DATASET ArendeDS):
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
   
   METHOD PUBLIC VOID ArendeStatusHmt(INPUT kalkdatah AS HANDLE):
      
      RUN ArendeStatus_UI IN AppServerHandle (INPUT kalkdatah,OUTPUT arendetidlageTTh).
              
   END METHOD.
   
   METHOD PUBLIC VOID TidutHmt(vad AS CHARACTER):
      CREATE tidut.
      tidut.UT = vad.  
      tidut.TTRECID = RECID(tidut).            
   END METHOD.
   
   
   /*VID NU KALKYL BLA. SPARA */
   METHOD PUBLIC VOID KopplatAoArende(aonrvar AS CHARACTER,delnrvar AS INTEGER,pnrvar AS CHARACTER,artalvar AS INTEGER):
      ASSIGN 
      kopplataonr = aonrvar
      kopplatdelnr = delnrvar
      kopplatplnr = pnrvar
      kopplatartal = artalvar.
   END METHOD.   
   METHOD PUBLIC VOID SaveHuvud(OUTPUT txt AS CHARACTER, OUTPUT isit AS LOGICAL):
      EMPTY TEMP-TABLE earendehuvtt NO-ERROR.
      CREATE earendehuvtt.
      BUFFER-COPY arendehuvtt TO earendehuvtt. 
      
      RUN sparakalkhuv_UI IN AppServerHandle (OUTPUT isit,OUTPUT txt,INPUT-OUTPUT TABLE earendehuvtt).
      IF isit = TRUE THEN RETURN.
      FIND FIRST earendehuvtt WHERE NO-LOCK NO-ERROR.
      BUFFER-COPY earendehuvtt TO arendehuvtt.
      arendehuvtt.TTRECID = RECID(arendehuvtt).
      EMPTY TEMP-TABLE earendehuvtt NO-ERROR.  
      FIND FIRST arendehuvtt WHERE NO-LOCK NO-ERROR.
      FIND FIRST arendeaonrTT WHERE arendeaonrTT.ARENDENR = arendehuvtt.ARENDENR AND arendeaonrTT.OMRADE = arendehuvtt.OMRADE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE arendeaonrTT THEN DO:
         CREATE arendeaonrTT.
         ASSIGN
         arendeaonrTT.ARENDENR = arendehuvtt.ARENDENR
         arendeaonrTT.OMRADE = arendehuvtt.OMRADE
         arendeaonrTT.TYP = arendehuvtt.TYP
         arendeaonrTT.AONR   = kopplataonr 
         arendeaonrTT.DELNR  = kopplatdelnr 
         arendeaonrTT.PLANNR = kopplatplnr 
         arendeaonrTT.ARTAL  = kopplatartal
         arendeaonrTT.AKTIV = TRUE 
         arendeaonrTT.TTRECID = RECID(arendeaonrTT).
         arendeaonrTT.STATUSNIV = Guru.GlobalaVariabler:arendekalk. 
         
      END.
      
      FIND FIRST arendetidlageTT NO-LOCK NO-ERROR.
      IF NOT AVAILABLE arendetidlageTT THEN DO:
         RUN kalktidl_UI IN AppServerHandle (INPUT arendehuvtt.ARENDENR, INPUT arendehuvtt.OMRADE, OUTPUT TABLE earendetidlageTT).
         FOR EACH earendetidlageTT NO-LOCK:
            CREATE arendetidlageTT.
            BUFFER-COPY earendetidlageTT TO arendetidlageTT.
            IF arendetidlageTT.ORDNING = "1" THEN DO:
               arendetidlageTT.DATUMTID = DATETIME(TODAY, MTIME).
               arendetidlageTT.ANVANDARE1 = Guru.Konstanter:globanv.
               Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "'")  NO-ERROR.
               IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
                  ASSIGN
                  arendetidlageTT.NAMNANVANDARE1 = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("AV-NAMN"):BUFFER-VALUE.
               END.
            END.  
            arendetidlageTT.TTRECID = RECID(arendetidlageTT).               
         END. 
         EMPTY TEMP-TABLE earendetidlageTT NO-ERROR. 
         THIS-OBJECT:ControlShell:ArendeTidlControl:GridTidlage:GuruReopen().
      END.   
        
      THIS-OBJECT:ArendeSpara().
      FIND FIRST arendehuvtt WHERE NO-LOCK NO-ERROR.
      FIND FIRST arendeaonrTT WHERE arendeaonrTT.ARENDENR = arendehuvtt.ARENDENR AND arendeaonrTT.OMRADE = arendehuvtt.OMRADE NO-LOCK NO-ERROR.
         
      THIS-OBJECT:ArendeSpara().        
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
         FIND FIRST arendehuvtt WHERE NO-LOCK NO-ERROR.
         IF arendehuvtt.UTYP NE 1 THEN DO:
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
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualVALUEs("Arende",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" THEN DO:
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
         IF NOT AVAILABLE arendenumtt THEN DO:
            FIND FIRST arendenumtt WHERE arendenumtt.NUM = arendenumsubtt.NUM NO-ERROR. 
         END.
         OPEN QUERY ksq FOR EACH arendenumsubtt WHERE arendenumsubtt.NUM = arendenumtt.NUM NO-LOCK.
         GET FIRST ksq NO-LOCK.     
      END.
      ELSE DO:
         OPEN QUERY ksq FOR EACH arendenumsubtt NO-LOCK.
         GET FIRST ksq NO-LOCK.
      END.    
      DO WHILE AVAILABLE(arendenumsubtt):
         IF allakoder = TRUE THEN DO:
            FIND FIRST arendenumtt WHERE arendenumtt.NUM = arendenumsubtt.NUM NO-ERROR.  
         END.                  
         CREATE kalkantal.
         /*kalkberin*/
         BUFFER-COPY arendenumtt TO kalkantal.
         CREATE kalkkostnad.
         BUFFER-COPY arendenumtt TO kalkkostnad.
         ASSIGN
         kalkkostnad.FRITOTKOST = 0
         kalkkostnad.TOTKOST = 0.
         FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = arendenumtt.KLOGSUBID AND 
         kalkylprisertt.KPID = arendenumsubtt.KPID NO-LOCK NO-ERROR.
         FIND FIRST kalkvisningtt WHERE kalkvisningtt.KVID = kalkylprisertt.KVID NO-LOCK NO-ERROR.
         IF AVAILABLE kalkvisningtt THEN DO:    
            ASSIGN 
            kalkantal.TIMTYP = kalkvisningtt.TIMTYP
            kalkantal.KPID = kalkylprisertt.KPID
            kalkantal.KVID = kalkylprisertt.KVID.   
            IF allakoder = TRUE THEN DO:          
               kalkantal.SUMMA = kalkantal.SUMMA + KalkBerAntal(arendenumsubtt.TIMMAR) * arendenumtt.ANTAL.
               kalkantal.FRISUMMA = kalkantal.FRISUMMA + KalkBerAntal(arendenumsubtt.FRITIMMAR) * arendenumtt.ANTAL.
            END.
            ELSE DO:
               kalkantal.SUMMA = kalkantal.SUMMA + arendenumsubtt.TIMMAR * arendenumtt.ANTAL.
               kalkantal.FRISUMMA = kalkantal.FRISUMMA + arendenumsubtt.FRITIMMAR * arendenumtt.ANTAL.
            END.      
            ASSIGN 
            kalkkostnad.KOSTTYP = kalkvisningtt.KOSTTYP
            kalkkostnad.KPID = kalkylprisertt.KPID
            kalkkostnad.KVID = kalkylprisertt.KVID. 
            /*kalkberin*/   
            IF allakoder = TRUE THEN DO: 
               IF arendenumtt.ARBKOD = "EGEN" THEN DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + KalkBerKost(1,arendenumsubtt.TIMMAR * arendenumtt.ANTAL * arendenumsubtt.PRIS)
                  + KalkBerKost(2,arendenumtt.ANTAL * arendenumsubtt.KOSTNAD) + arendenumsubtt.AVRUND.
               END.
               ELSE DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + KalkBerKost(1,arendenumsubtt.TIMMAR * arendenumtt.ANTAL * kalkylprisertt.PRIS)
                  + KalkBerKost(2,arendenumtt.ANTAL * arendenumsubtt.KOSTNAD) + arendenumsubtt.AVRUND.
                       
               END.   
            END.
            ELSE DO:
               IF arendenumtt.ARBKOD = "EGEN" THEN DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + (arendenumsubtt.TIMMAR * arendenumtt.ANTAL * arendenumsubtt.PRIS) 
                  + (arendenumtt.ANTAL * arendenumsubtt.KOSTNAD) + arendenumsubtt.AVRUND.
               END.
               ELSE DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + (arendenumsubtt.TIMMAR * arendenumtt.ANTAL * kalkylprisertt.PRIS) 
                  + (arendenumtt.ANTAL * arendenumsubtt.KOSTNAD) + arendenumsubtt.AVRUND.
               END.      
            END.    
            /*kalkberin*/
            kalkkostnad.FRITOTKOST = kalkkostnad.FRITOTKOST + (arendenumsubtt.FRITIMMAR * arendenumtt.ANTAL * arendenumsubtt.FRIPRIS) 
            + (arendenumtt.ANTAL * arendenumsubtt.FRIKOSTNAD) + arendenumsubtt.FRIAVRUND.
            totkostvar = totkostvar + kalkkostnad.TOTKOST.
            fritotkostvar = fritotkostvar + kalkkostnad.FRITOTKOST.
         END.
         FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = arendenumtt.KLOGSUBID AND kalkylprisertt.KVID = 0 AND kalkylprisertt.SOKBENAMNING = "RÖRLIGKOSTNAD EA"  NO-LOCK NO-ERROR.
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
         arendenumtt.FRITOTKOST = fritotkostvar
         arendenumtt.TOTKOST = totkostvar.   
         EMPTY TEMP-TABLE kalkantal NO-ERROR.
         EMPTY TEMP-TABLE kalkkostnad NO-ERROR.       
      END.     
  END METHOD.
  /*RÄKNA MED FAKTOR*/
  METHOD PUBLIC DECIMAL KalkBerAntal(INPUT kalktimantal AS DECIMAL):
     IF Guru.GlobalaVariabler:FranUppf = TRUE THEN RETURN kalktimantal.
         
     RETURN  kalktimantal.
  END METHOD.   
  /*RÄKNA MED EGANAPRISER*/
  METHOD PUBLIC DECIMAL KalkBerKost(INPUT vad AS INTEGER, INPUT kalktimkost AS DECIMAL):
     IF Guru.GlobalaVariabler:FranUppf = TRUE THEN RETURN kalktimkost.
     IF vad = 1 THEN DO:       
        IF arendenumtt.ARBKOD = "EGEN" THEN.
       
     END.
         
     RETURN  kalktimkost.
  END METHOD.   
   /*ny koder i kalkyl vid buttenover*/
  METHOD PUBLIC VOID CreateKoderRows(antalvar AS DECIMAL):
     DEFINE VARIABLE rmat AS INTEGER NO-UNDO.
     EMPTY TEMP-TABLE earendenumtt NO-ERROR. 
     rmat = THIS-OBJECT:ControlShell:CurrentMatris.
     IF rmat EQ -1 THEN rmat = 1.
     FOR EACH arendenumtt WHERE arendenumtt.MATRIS = 0 NO-LOCK: 
        FIND LAST arendenumttbuf USE-INDEX NUM NO-ERROR.
        IF AVAILABLE arendenumttbuf THEN arendenumtt.NUM = arendenumttbuf.NUM + 1.
        ELSE arendenumtt.NUM = 1.
        ASSIGN
        arendenumtt.ARENDENR  = arendehuvtt.ARENDENR
        arendenumtt.OMRADE  = arendehuvtt.OMRADE
        arendenumtt.TYPKALK = arendehuvtt.TYPKALK /* asdasd*/
        arendenumtt.MARKNING = kalkylarbkodertt.MARKNING
        arendenumtt.MARKSUB = kalkylarbkodertt.MARKSUB
        arendenumtt.MATRIS  = rmat
        arendenumtt.ANTAL   = antalvar.
        EMPTY TEMP-TABLE earendenumsubtt NO-ERROR.   
        RUN skapanumsub_UI IN AppServerHandle (INPUT arendenumtt.KLOGSUBID,INPUT arendenumtt.ARBKOD,INPUT arendenumtt.LOPNR,OUTPUT TABLE earendenumsubtt).
        FOR EACH earendenumsubtt WHERE NO-LOCK:
           FIND LAST arendenumsubtt WHERE arendenumsubtt.NUM = arendenumtt.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
           IF AVAILABLE arendenumsubtt THEN earendenumsubtt.NUMSUBID =  arendenumsubtt.NUMSUBID + 1.
           ELSE  earendenumsubtt.NUMSUBID = 1.
           ASSIGN 
           earendenumsubtt.ARENDENR = arendenumtt.ARENDENR
           earendenumsubtt.OMRADE = arendenumtt.OMRADE
           earendenumsubtt.NUM = arendenumtt.NUM.
           CREATE arendenumsubtt.
           BUFFER-COPY earendenumsubtt TO arendenumsubtt.
           arendenumsubtt.TTRECID = RECID(arendenumsubtt).
           DELETE earendenumsubtt.                      
        END.
        EMPTY TEMP-TABLE earendenumsubtt NO-ERROR.  
        THIS-OBJECT:RaknaEnKod(FALSE).                      
     END.
     THIS-OBJECT:ArendeSpara().
     /*problem ccc*/
     RUN LaddaArende IN AppServerHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar,OUTPUT DATASET ArendeDS).
     FIND FIRST arendehuvtt NO-LOCK NO-ERROR. 
  END METHOD.
  
   /*ny koder i kalkyl viA IMPORT/KOPIERA KONVERTERA*/
  METHOD PUBLIC VOID CreateKodRow(INPUT kodttR AS RECID, INPUT spara AS LOGICAL ):
     /*
     FIND FIRST ArendeimportTT WHERE ArendeimportTT.TTRECID =  importtR NO-LOCK NO-ERROR.
     IF NOT AVAILABLE ArendeimportTT THEN DO:
        RETURN.
     END.
     */
     FIND FIRST arendenumtt WHERE RECID(arendenumtt) =  kodttR NO-LOCK NO-ERROR.
     FIND LAST arendenumttbuf USE-INDEX NUM NO-ERROR.
     IF AVAILABLE arendenumttbuf THEN arendenumtt.NUM = arendenumttbuf.NUM + 1.
     ELSE arendenumtt.NUM = 1.
     ASSIGN
     arendenumtt.TTRECID    =  kodttR
     arendenumtt.ARENDENR     = arendehuvtt.ARENDENR
     arendenumtt.OMRADE     = arendehuvtt.OMRADE
     arendenumtt.TYPKALK    = arendehuvtt.TYPKALK.
     /*
     arendenumtt.MARKNING   = ArendeimportTT.MARKNING
     arendenumtt.MARKSUB    = ArendeimportTT.MARKSUB
     arendenumtt.MATRIS     = ArendeimportTT.MATRIS
     arendenumtt.ANTAL      = ArendeimportTT.ANTAL
     arendenumtt.ANMARKNING = ArendeimportTT.ANMARKNING
     arendenumtt.RISK       = ArendeimportTT.RISK
     arendenumtt.VINST      = ArendeimportTT.VINST.
     */
     EMPTY TEMP-TABLE earendenumsubtt NO-ERROR.   
     RUN skapanumsub_UI IN AppServerHandle (INPUT arendenumtt.KLOGSUBID,INPUT arendenumtt.ARBKOD,INPUT arendenumtt.LOPNR,OUTPUT TABLE earendenumsubtt).
     FOR EACH earendenumsubtt WHERE NO-LOCK:
        FIND LAST arendenumsubtt WHERE arendenumsubtt.NUM = arendenumtt.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
        IF AVAILABLE arendenumsubtt THEN earendenumsubtt.NUMSUBID =  arendenumsubtt.NUMSUBID + 1.
        ELSE  earendenumsubtt.NUMSUBID = 1.
        ASSIGN 
        earendenumsubtt.ARENDENR = arendenumtt.ARENDENR
        earendenumsubtt.OMRADE = arendenumtt.OMRADE
        earendenumsubtt.NUM = arendenumtt.NUM.
        CREATE arendenumsubtt.
        BUFFER-COPY earendenumsubtt TO arendenumsubtt.
        arendenumsubtt.TTRECID = RECID(arendenumsubtt).
        DELETE earendenumsubtt.                      
     END.
     EMPTY TEMP-TABLE earendenumsubtt NO-ERROR.  
     THIS-OBJECT:RaknaEnKod(FALSE).                      
     IF spara = TRUE THEN DO:
        THIS-OBJECT:ArendeSpara(). 
        /*problem ccc*/
        RUN LaddaArende IN AppServerHandle (INPUT arendenumtt.ARENDENR,INPUT arendenumtt.OMRADE,OUTPUT DATASET ArendeDS).     
        FIND FIRST arendehuvtt NO-LOCK NO-ERROR.
     END.   
  END METHOD. 
  METHOD PUBLIC VOID ImportSpara():
     THIS-OBJECT:ArendeSpara(). 
     /*
     RUN LaddaArende IN AppServerHandle (INPUT arendenumtt.ARENDENR,INPUT arendenumtt.OMRADE,OUTPUT DATASET ArendeDS).
     */     
     FIND FIRST arendehuvtt NO-LOCK NO-ERROR.
  END METHOD.
   
  /*ny egen kod*/
   METHOD PUBLIC ROWID CreateEgenKod():
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      DEFINE VARIABLE rmat AS INTEGER NO-UNDO.
      DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
      DEFINE VARIABLE lopnrvar AS INTEGER NO-UNDO.
      rmat = THIS-OBJECT:ControlShell:CurrentMatris.
      IF rmat EQ -1 THEN rmat = 1.
      FIND LAST arendenumtt USE-INDEX NUM NO-ERROR.
      IF AVAILABLE arendenumtt THEN numvar = arendenumtt.NUM + 1.
      ELSE numvar = 1.
   /* Hitta senaste löpnummer */
      FIND LAST arendenumtt WHERE arendenumtt.ARBKOD = "EGEN"
      USE-INDEX ARBKOD NO-LOCK NO-ERROR.
      IF AVAILABLE arendenumtt THEN lopnrvar = arendenumtt.LOPNR + 1.
      ELSE DO:
         IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" THEN lopnrvar = 10.
         ELSE lopnrvar = 1.
      END.
      FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID = arendehuvtt.KLOGID NO-LOCK NO-ERROR.
      CREATE arendenumtt.        
      ASSIGN
      arendenumtt.ARENDENR  = arendehuvtt.ARENDENR
      arendenumtt.OMRADE  = arendehuvtt.OMRADE
      arendenumtt.TYPKALK = arendehuvtt.TYPKALK
      arendenumtt.NUM     = numvar
      arendenumtt.LOPNR   = lopnrvar
      arendenumtt.MATRIS  = rmat
      arendenumtt.ARBKOD = "EGEN"
      arendenumtt.ANTAL   = 1
      arendenumtt.BENAMNING = "< Ange Ny Benämning >"
      arendenumtt.ENHET = "ST"
      arendenumtt.TTRECID   = RECID(arendenumtt).
      arendenumtt.KLOGSUBID = kalkylkatalogtt.HKLOGSUBID.
      returner = ROWID(arendenumtt). 
      THIS-OBJECT:ArendeSpara().
      RETURN returner. 
   END METHOD.
   /*skapa nya kalknumsubbar i egen*/
   METHOD PUBLIC VOID CreateValdaRows():
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      DEFINE VARIABLE pristypvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pristypvarny AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ejmed AS LOGICAL NO-UNDO.
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:ArendeEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:TEXT).
      FIND FIRST arendenumtt WHERE arendenumtt.NUM = hittnum NO-LOCK NO-ERROR.
      FOR EACH arendenumsubtt WHERE arendenumsubtt.NUM = 0 NO-LOCK:                                                   
         ASSIGN 
         pristypvar = "".
         ejmed = FALSE.
         ASSIGN
         arendenumsubtt.ARENDENR = arendenumtt.ARENDENR
         arendenumsubtt.OMRADE = arendenumtt.OMRADE
         arendenumsubtt.TTRECID = RECID(arendenumsubtt).
         IF arendenumsubtt.PRIS NE 0 THEN arendenumsubtt.TIMMAR = 1.
         ASSIGN
         arendenumsubtt.FRIBENAMNING = arendenumsubtt.BENAMNING
         arendenumsubtt.FRIPRIS = arendenumsubtt.PRIS
         arendenumsubtt.FRIAVRUND  = arendenumsubtt.AVRUND 
         arendenumsubtt.FRITIMMAR = arendenumsubtt.TIMMAR
         arendenumsubtt.FRIKOSTNAD = arendenumsubtt.KOSTNAD. 
         IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
            pristypvar = THIS-OBJECT:PrisTyp(INPUT arendenumsubtt.KPID).
            IF pristypvar = "ENTREP" THEN ejmed = TRUE.    
         END.   
         IF ejmed = FALSE THEN DO:
            IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
               pristypvarny = THIS-OBJECT:PrisTyp(INPUT arendenumsubtt.KPID).
               FOR EACH arendenumsubttbuf WHERE arendenumsubttbuf.NUM = arendenumtt.NUM AND arendenumsubttbuf.NUM > 0:
                  pristypvar = THIS-OBJECT:PrisTyp(INPUT arendenumsubttbuf.KPID).
                  IF pristypvar BEGINS "MASKIN" AND pristypvarny BEGINS "MASKIN" THEN ejmed = TRUE.
               END.   
            END.
         END.    
         IF ejmed = TRUE THEN DO:                            
            res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(49) + " " + arendenumsubtt.BENAMNING, "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
            DELETE arendenumsubtt.            
         END.
         ELSE DO:   
            arendenumsubtt.NUM    = arendenumtt.NUM.                                                                
            FIND LAST arendenumsubttbuf WHERE arendenumsubttbuf.NUM = arendenumtt.NUM AND arendenumsubttbuf.NUM > 0 USE-INDEX NUM NO-LOCK NO-ERROR.
            IF AVAILABLE arendenumsubttbuf THEN arendenumsubtt.NUMSUBID =  arendenumsubttbuf.NUMSUBID + 1.
            ELSE  arendenumsubtt.NUMSUBID = 1.  
            THIS-OBJECT:RaknaEnKod(FALSE).       
         END.   
      END.                  
      THIS-OBJECT:ArendeSpara().
   END METHOD.
   /*uppdatering av t.ex. antal i kalknum både vanlig och egen.*/   
   METHOD PUBLIC VOID UpdateKod():
      THIS-OBJECT:RaknaEnKod(FALSE).
      THIS-OBJECT:ArendeSpara().
   END METHOD.
   /*uppdatering av t.ex. antal,pris i kalknumsub både vanlig och egen.*/
   METHOD PUBLIC VOID UpdateKodSub():
      THIS-OBJECT:RaknaEnKod(FALSE).
      THIS-OBJECT:ArendeSpara().
   END METHOD.
   /*TAR BORT NUMSUBBAR*/
   METHOD PUBLIC VOID RemoveKoderRow(INPUT villkor AS CHARACTER):
      DEFINE VARIABLE mat AS INTEGER NO-UNDO.
      mat = THIS-OBJECT:ControlShell:CurrentMatris.
      THIS-OBJECT:ControlShell:ArendeControl:GridKalkylKoder:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
      FOR EACH arendenumsubtt WHERE arendenumsubtt.NUM = arendenumtt.NUM NO-LOCK:
         DELETE arendenumsubtt.
      END.
      /*
      DELETE arendenumtt.
      THIS-OBJECT:ArendeSpara().
      */
      
   END METHOD.
   /*TAR BORT NUMSUBBAR*/
   METHOD PUBLIC VOID RemoveKoderRowROWID(INPUT gridrowid AS ROWID):
      KoderTTh:FIND-BY-ROWID(gridrowid).
      FOR EACH arendenumsubtt WHERE arendenumsubtt.NUM = arendenumtt.NUM NO-LOCK:
         DELETE arendenumsubtt.
      END.
      /*
      DELETE arendenumtt.
      THIS-OBJECT:ArendeSpara().
      */ 
      
   END METHOD.
    
   /*TAR BORT EGEN NUMSUB*/
   METHOD PUBLIC VOID RemoveValdaRowEgna (INPUT villkor AS CHARACTER):
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      DEFINE VARIABLE kostvar AS DECIMAL NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:ArendeEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:TEXT).
      FIND FIRST arendenumtt WHERE arendenumtt.NUM = hittnum NO-LOCK NO-ERROR.
      THIS-OBJECT:ControlShell:ArendeEgnaControl:GridValdaUnderKoder:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
      kostvar = arendenumsubtt.TIMMAR * arendenumtt.ANTAL * arendenumsubtt.PRIS + arendenumtt.ANTAL * arendenumsubtt.KOSTNAD.
      ASSIGN 
      arendenumtt.TOTKOST = arendenumtt.TOTKOST - kostvar
      arendenumtt.FRITOTKOST = arendenumtt.FRITOTKOST - kostvar. 
     
   END METHOD.
   /*TAR BORT EGEN KOD*/
   METHOD PUBLIC VOID RemoveEgenKod():
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:ArendeEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:Text).
      FOR EACH arendenumsubtt WHERE arendenumsubtt.NUM = hittnum NO-LOCK:
         DELETE arendenumsubtt.
      END.
      THIS-OBJECT:ArendeSpara().
   END METHOD.
   
   /*TAR BORT NUMSUBBAR VID buttonBackMark_Click DVS KODER*/
   METHOD PUBLIC VOID RemoveKoderSelected():
      DEFINE VARIABLE mat AS INTEGER NO-UNDO.
      mat = THIS-OBJECT:ControlShell:CurrentMatris.
      EMPTY TEMP-TABLE earendenumtt NO-ERROR.        
      {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:ControlShell:ArendeControl:GridKalkylKoder:Selected:Rows}
         THIS-OBJECT:RemoveKoderRow(villkor).
      END. /*foreach end*/    
      IF oObject = ? THEN DO:                                                   
         THIS-OBJECT:RemoveKoderRowROWID(KoderTTh:ROWID).
      END.   
   END METHOD.
   /*TAR BORT NUMSUBBAR VID buttonBackMarkPriser_Click DVS VIA EGNAKODER*/
   METHOD PUBLIC VOID RemoveValdaSelectedEgna ():                    
      EMPTY TEMP-TABLE earendenumsubtt NO-ERROR.
      {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:ControlShell:ArendeEgnaControl:GridValdaUnderKoder:Selected:Rows}                                         
         THIS-OBJECT:RemoveValdaRowEgna (villkor).
      END. /*foreach end*/
   END METHOD.
  /* buttonBackMarkPriser_Click buttonBackAllaPriser_Click*/
   METHOD PUBLIC VOID RemoveValdaAllaEgna ():    
      EMPTY TEMP-TABLE earendenumsubtt NO-ERROR.  
       {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:ControlShell:ArendeEgnaControl:GridValdaUnderKoder:Rows}                                         
         THIS-OBJECT:RemoveValdaRowEgna (villkor).
      END. /*foreach end*/     
   END METHOD.
   /*SKAPA MRTL*/
   METHOD PUBLIC VOID CreatevalMaterial(INPUT kalknr AS INTEGER,INPUT omr AS CHARACTER, berantal AS  DECIMAL):
      DEFINE VARIABLE cm AS INTEGER NO-UNDO.
      cm = THIS-OBJECT:ControlShell:CurrentMatris.
      IF cm EQ -1 THEN cm = 1.
      FOR EACH arendemtrlTT WHERE arendemtrlTT.MATRIS = 0 NO-LOCK:
         ASSIGN 
         arendemtrlTT.OMRADE    = omr
         arendemtrlTT.ARENDENR    = kalknr
         arendemtrlTT.MATRIS    = cm
         arendemtrlTT.BERKVANT  = berantal
         arendemtrlTT.TTRECID   = RECID(arendemtrlTT).
         FIND LAST arendemtrlTTbuf USE-INDEX MID NO-LOCK NO-ERROR.
         IF AVAILABLE arendemtrlTTbuf THEN DO:
            arendemtrlTT.MID = arendemtrlTTbuf.MID + 1.
         END.   
         ELSE arendemtrlTT.MID = 1.
      END.
      THIS-OBJECT:ArendeSpara().
   END METHOD.
   /*BERÄKNING AV MTRL*/
   METHOD PUBLIC DECIMAL MtrlKost(INPUT imatris AS INTEGER):
      IF imatris = ? THEN DO:
         FOR EACH arendemtrlTT BREAK BY arendemtrlTT.BREAKDUMMY:
            arendemtrlTT.SUMMA = arendemtrlTT.NPRIS *  arendemtrlTT.BERKVANT.
            ACCUMULATE arendemtrlTT.SUMMA (TOTAL BY arendemtrlTT.BREAKDUMMY).
            IF LAST-OF(arendemtrlTT.BREAKDUMMY) THEN DO:
               IF Guru.GlobalaVariabler:FranUppf = TRUE THEN RETURN ACCUM TOTAL BY arendemtrlTT.BREAKDUMMY arendemtrlTT.SUMMA.
               
               ELSE RETURN ACCUM TOTAL BY arendemtrlTT.BREAKDUMMY arendemtrlTT.SUMMA.   
            END.                   
         END.
      END.
      ELSE DO:              
         FOR EACH arendemtrlTT WHERE arendemtrlTT.MATRIS = imatris BREAK BY arendemtrlTT.MATRIS:
            arendemtrlTT.SUMMA = arendemtrlTT.NPRIS *  arendemtrlTT.BERKVANT.
            ACCUMULATE arendemtrlTT.SUMMA (TOTAL BY arendemtrlTT.MATRIS).
            IF LAST-OF(arendemtrlTT.MATRIS) THEN DO:
               IF Guru.GlobalaVariabler:FranUppf = TRUE THEN RETURN ACCUM TOTAL BY arendemtrlTT.MATRIS arendemtrlTT.SUMMA.
               
               ELSE RETURN ACCUM TOTAL BY arendemtrlTT.MATRIS arendemtrlTT.SUMMA.                 
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
  /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen kör ej i ärende!
        */
        /*
   METHOD PUBLIC LOGICAL BeredningMtrlfinns():          
      DEFINE VARIABLE nyber AS LOGICAL NO-UNDO.
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN RUN BeredningMtrlfinns IN AppServerExtraHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar, OUTPUT nyber).  
      RETURN nyber.     
   END METHOD.
   METHOD PUBLIC VOID BeredningMtrlHmt(OUTPUT totbermtrl AS DECIMAL): 
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN RUN BeredningMtrlHmt IN AppServerExtraHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar,OUTPUT totbermtrl,OUTPUT TABLE berkalkmtrltt).
   END METHOD.
   */
    METHOD PUBLIC CHARACTER BortTagenPersonal(INPUT pkodNamnvar AS CHARACTER):
      RUN BortTagenPersonal_UI IN AppServerHandle (INPUT-OUTPUT pkodNamnvar).
      RETURN pkodNamnvar.
   END METHOD.
   
   METHOD PUBLIC VOID AnvKalkyl (INPUT nykalknr AS INTEGER, INPUT nyomrvar AS CHARACTER):       
      RUN anvkalkyl_UI IN AppServerHandle (INPUT nykalknr,INPUT nyomrvar).      
   END METHOD.
   
   METHOD PUBLIC VOID anvkalkylhmt(OUTPUT TABLE anvkalkyltt):       
       RUN anvkalkylhmt_UI IN AppServerHandle (OUTPUT TABLE anvkalkyltt).    
   END METHOD.
END CLASS.