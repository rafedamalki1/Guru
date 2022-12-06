
 
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
   {BerKalktt.I}
   {INKSUMBERTEMP.I}
   &Scoped-define PUBLIC PUBLIC
   
   {KALKYLKATH.i}
   /*
   {MTRLTEMPC.I}
   */
   {KALKYLPRODATA.i}
   DEFINE VARIABLE SistasparaDsH AS HANDLE NO-UNDO.
   DEFINE VARIABLE KalkylAnvEgenDS AS HANDLE NO-UNDO.
   DEFINE VARIABLE VolymDS AS HANDLE NO-UNDO.
   DEFINE VARIABLE BerKalkDS AS HANDLE NO-UNDO.
   DEFINE VARIABLE BerValDS AS HANDLE NO-UNDO.
   DEFINE VARIABLE BerMtrlDS AS HANDLE NO-UNDO.
   DEFINE VARIABLE HdSchaktDS AS HANDLE NO-UNDO.
   DEFINE VARIABLE nyberkalknum AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE valdberkonst AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE valdsidnr AS INTEGER NO-UNDO.
   DEFINE PUBLIC  VARIABLE Timer                      AS Guru.Timer              NO-UNDO.
   DEFINE VARIABLE snabbspara AS LOGICAL NO-UNDO.
   DEFINE VARIABLE listitemVisaPristyp AS INTEGER NO-UNDO INITIAL -1 .
   
   {KALKYLKATPRODATA.i}
   
   DEFINE TEMP-TABLE kalkylstopptt NO-UNDO
      FIELD KALKNR AS INTEGER
      FIELD OMRADE AS CHARACTER.
      
   DEFINE PRIVATE VARIABLE WinApi AS HANDLE NO-UNDO.
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO 
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC  PROPERTY AppServerExtraHandle AS HANDLE NO-UNDO
   PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.Kalkyl.KalkylShell NO-UNDO.
   
   DEFINE PUBLIC PROPERTY kopplataonr AS CHARACTER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC PROPERTY kopplatplnr AS CHARACTER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC PROPERTY kopplatdelnr AS INTEGER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC PROPERTY kopplatartal AS INTEGER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   /*
   DEFINE PUBLIC PROPERTY berkalkylstart AS LOGICAL NO-UNDO
   PUBLIC GET. PUBLIC SET.
   */
   DEFINE PUBLIC PROPERTY VisaDirekt AS LOGICAL NO-UNDO
   PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC PROPERTY startvar AS CHARACTER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE VARIABLE totkostvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE fritotkostvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE allapris AS DECIMAL NO-UNDO.
   DEFINE VARIABLE allaprisfri AS DECIMAL NO-UNDO.
   DEFINE VARIABLE utsmetkost AS DECIMAL NO-UNDO.
   DEFINE VARIABLE projledkost AS DECIMAL NO-UNDO.
   DEFINE VARIABLE monttottimusmet AS DECIMAL NO-UNDO.
   DEFINE VARIABLE totkostusmet AS DECIMAL NO-UNDO.
   DEFINE VARIABLE efrihj AS INTEGER NO-UNDO.
   DEFINE VARIABLE egenhj AS INTEGER NO-UNDO.
   DEFINE VARIABLE BernrVar AS INTEGER NO-UNDO.
   DEFINE VARIABLE BerOmrVar AS CHARACTER NO-UNDO.
   DEFINE PUBLIC PROPERTY BerKalkNum AS INTEGER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC PROPERTY BerSchaktSid AS INTEGER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE VARIABLE kalkantalbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE kalkkostnadbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE inkberh AS HANDLE NO-UNDO. 
   CONSTRUCTOR PUBLIC Kalkyldb(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Connect().
      ASSIGN 
      kopplataonr = ?
      kopplatdelnr = ?
      kopplatplnr = ?
      kopplatartal = ?.
           
   END CONSTRUCTOR.
   CONSTRUCTOR PUBLIC Kalkyldb(INPUT r AS Guru.Root, INPUT startvarIN AS CHARACTER):
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Connect().
      ASSIGN 
      kopplataonr = ?
      kopplatdelnr = ?
      kopplatplnr = ?
      kopplatartal = ?.
      startvar = startvarIN.     
   END CONSTRUCTOR.
   CONSTRUCTOR PUBLIC Kalkyldb(INPUT r AS Guru.Root, INPUT TABLE berkalktt, INPUT bkn AS INTEGER,bsnr AS INTEGER,VisaDirektin AS LOGICAL):
      /*Kalkyl i beredning*/
      FIND FIRST berkalktt WHERE NO-LOCK NO-ERROR.        
      THIS-OBJECT:Root = r.
      BernrVar = berkalktt.BERNR.
      BerOmrVar = berkalktt.OMRADE.  
      BerKalkNum = bkn.
      BerSchaktSid = bsnr.
     
      THIS-OBJECT:Connect().
      ASSIGN 
      kopplataonr = ?
      kopplatdelnr = ?
      kopplatplnr = ?
      kopplatartal = ?.
       
      /*visning av kalkyl*/ 
      VisaDirekt = VisaDirektin.
   END CONSTRUCTOR.
    
   DESTRUCTOR Kalkyldb():
      DEFINE VARIABLE lasavanv AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lasavnamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lastav AS LOGICAL NO-UNDO.
      THIS-OBJECT:SparaMtrl().
      FOR EACH kalkylstopptt WHERE NO-LOCK:
         RUN KalkStopp_UI IN AppServerHandle (INPUT 2,INPUT kalkylstopptt.KALKNR,INPUT kalkylstopptt.OMRADE, INPUT Guru.Konstanter:globanv, OUTPUT lasavanv, OUTPUT lasavnamn, OUTPUT lastav).
      END.
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DO:
         RUN avsluta_UI IN AppServerHandle.
         DELETE PROCEDURE THIS-OBJECT:AppServerHandle NO-ERROR.
      END.
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN DO:
         RUN avsluta_UI IN AppServerExtraHandle.
         DELETE PROCEDURE THIS-OBJECT:AppServerExtraHandle NO-ERROR.
      END.
      IF VALID-HANDLE(inkberh) THEN DO: 
         RUN InkAvs_UI IN inkberh.
         DELETE PROCEDURE inkberh NO-ERROR.
      END.  
      Guru.GlobalaVariabler:KalkylSomStartiBer = FALSE.
      AppServerHandle = ?.
      AppServerExtraHandle = ?.
      {KALKYLCONTROLLDELETE.i}
   END DESTRUCTOR.
   
   /* Gör en koppling till appserver */
   METHOD PUBLIC VOID Connect():
      
      IF Guru.Konstanter:appcon THEN DO:
         THIS-OBJECT:InitTimer().
         RUN KALKBERAPPDS.p PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
         /*ej ekg*/
         IF Guru.Konstanter:varforetypval[27] =  0 THEN RUN KALKBERAPPDSEXTRA.p PERSISTENT SET AppServerExtraHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
      END.
      ELSE DO:
         RUN KALKBERAPPDS.p PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globanv).
         /*ej ekg*/
         IF Guru.Konstanter:varforetypval[27] =  0 THEN RUN KALKBERAPPDSEXTRA.p PERSISTENT SET AppServerExtraHandle (INPUT Guru.Konstanter:globanv).
      END. 
      IF Guru.Konstanter:appcon THEN DO:
         RUN INKBERAPP.P PERSISTENT SET inkberh ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
      END.                             
      ELSE DO:
         RUN INKBERAPP.P PERSISTENT SET inkberh.   
      END.
      IF  Guru.GlobalaVariabler:plusaonr = ? THEN.
      ELSE RUN beredningskontroll_UI IN inkberh (INPUT   Guru.GlobalaVariabler:plusaonr,INPUT  Guru.GlobalaVariabler:plusdnr,OUTPUT TABLE sumbernum_mtrl,OUTPUT TABLE sumberskapnum_mtrl).
      kalkantalbuffh = TEMP-TABLE kalkantal:HANDLE:DEFAULT-BUFFER-HANDLE.
      kalkkostnadbuffh = TEMP-TABLE kalkkostnad:HANDLE:DEFAULT-BUFFER-HANDLE.
         
      THIS-OBJECT:AnvEgen().
      Guru.GlobalaVariabler:KalkylSomStartiBer = THIS-OBJECT:BerKalkStart().
   END METHOD.
          
   METHOD PUBLIC VOID InitTimer():
      THIS-OBJECT:Timer = NEW Guru.Timer(THIS-OBJECT:Root).
      THIS-OBJECT:Timer:StartaTimer(300000). /*Interval i milisekuder*/
    
      THIS-OBJECT:Timer:Timer:Tick:Subscribe(THIS-OBJECT:TimerEvent).
   END METHOD.
  
   METHOD PUBLIC VOID TimerEvent(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      IF snabbspara = TRUE THEN RETURN. 
      RUN Kontakt IN AppServerHandle NO-ERROR.
      IF ERROR-STATUS:ERROR = TRUE AND ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         MESSAGE "Du har tappat kontakt med servern. Guru kommer att avslutas, du behöver även starta om Guru. Tidpunkt: " NOW
         VIEW-AS ALERT-BOX.
         THIS-OBJECT:Root:WindowManager:Wnd:Close().
      END.
      
      
   END METHOD.
   
   METHOD PUBLIC VOID AnvEgenTest():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      THIS-OBJECT:ControlShell:KalkEgnaControl:GridAnvEgen:GuruTom().
      KalkylAnvEgenDS = ?.
      RUN laddaKalkylAnvEgenDS_UI IN AppServerHandle (INPUT Guru.Konstanter:globanv,OUTPUT DATASET-HANDLE KalkylAnvEgenDS BIND).
      IF VALID-HANDLE(KalkylAnvEgenDS) THEN DO:
         Kalknumanvegenbuffh = KalkylAnvEgenDS:GET-BUFFER-HANDLE(1).
         Kalknumanvegensubbuffh = KalkylAnvEgenDS:GET-BUFFER-HANDLE(2).
         THIS-OBJECT:KalkylAnvEgenTracking(TRUE).
      END.
       
     
       THIS-OBJECT:ControlShell:KalkEgnaControl:GridAnvEgen:ReInitGrid(INPUT THIS-OBJECT:Kalknumanvegenbuffh).
       THIS-OBJECT:ControlShell:KalkEgnaControl:GridAnvEgenSub:ReInitGrid(INPUT THIS-OBJECT:Kalknumanvegensubbuffh).
       THIS-OBJECT:ControlShell:KalkEgnaControl:GridAnvEgen:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlShell:KalkEgnaControl:ValueAnvEgenKoderCh).
       THIS-OBJECT:ControlShell:KalkEgnaControl:GridAnvEgen:GuruReopen().
       
   END METHOD.
   METHOD PUBLIC VOID AnvEgen():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      KalkylAnvEgenDS = ?.
      RUN laddaKalkylAnvEgenDS_UI IN AppServerHandle (INPUT Guru.Konstanter:globanv,OUTPUT DATASET-HANDLE KalkylAnvEgenDS BIND).
      IF VALID-HANDLE(KalkylAnvEgenDS) THEN DO:
         Kalknumanvegenbuffh = KalkylAnvEgenDS:GET-BUFFER-HANDLE(1).
         Kalknumanvegensubbuffh = KalkylAnvEgenDS:GET-BUFFER-HANDLE(2).
         THIS-OBJECT:KalkylAnvEgenTracking(TRUE).
      END.
           
   END METHOD.
   METHOD PUBLIC VOID KatAnvEgen():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      IF VALID-HANDLE(KalkylAnvEgenDS) THEN.
      ELSE RETURN.
      THIS-OBJECT:Kalknumanvegenbuffh:FIND-FIRST("WHERE  ") NO-ERROR.
      IF THIS-OBJECT:Kalknumanvegenbuffh:AVAILABLE THEN DO:
        
         queryvar =  "FOR EACH " + THIS-OBJECT:Kalknumanvegenbuffh:TABLE. 
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Kalknumanvegenbuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            THIS-OBJECT:LopposterTTh:BUFFER-CREATE().
            THIS-OBJECT:LopposterTTh:BUFFER-COPY(Kalknumanvegenbuffh).
            THIS-OBJECT:LopposterTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:LopposterTTh:RECID.
            qH:GET-NEXT().
         END.
         
      END.
   END METHOD.
   
   METHOD PUBLIC VOID BerkalkStopp(INPUT StoppVar AS CHARACTER, INPUT stoppvarlg AS LOGICAL):
       IF BerKalkNum = 0 THEN DO:
          IF Guru.Konstanter:appcon THEN DO:
            RUN SCHKALKSTOPP.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT BernrVar,INPUT BerOmrVar,INPUT BerSchaktSid, INPUT stoppvarlg).
         END.
         ELSE DO:
            RUN SCHKALKSTOPP.P (INPUT BernrVar,INPUT BerOmrVar,INPUT BerSchaktSid, INPUT stoppvarlg).
         END. 
       END.
       ELSE DO:   
          IF Guru.Konstanter:appcon THEN DO:
             RUN BERKALKSTOPP.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT BernrVar,INPUT BerOmrVar,INPUT BerKalkNum, INPUT StoppVar).
          END.
          ELSE DO:
             RUN BERKALKSTOPP.P  (INPUT BernrVar,INPUT BerOmrVar,INPUT BerKalkNum, INPUT StoppVar).
          END.
       END.    
   END METHOD.  
   METHOD PUBLIC VOID BerkalkHmtgrund():
       THIS-OBJECT:BerkalkStopp(INPUT "", INPUT FALSE).
     
       IF Guru.Konstanter:appcon THEN DO:
          RUN GrundKalkBerSch.p  ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT BernrVar,INPUT BerOmrVar,INPUT BerKalkNum, INPUT BerSchaktSid, OUTPUT TABLE KalkylimportTT).
       END.
       ELSE DO:
          RUN GrundKalkBerSch.p  (INPUT BernrVar,INPUT BerOmrVar,INPUT BerKalkNum, INPUT BerSchaktSid, OUTPUT TABLE KalkylimportTT).
       END.
        THIS-OBJECT:ControlShell:ImportLista().
      
      
   END METHOD.
   METHOD PUBLIC LOGICAL BerKalkStart():
      DEFINE VARIABLE berkalkvad AS CHARACTER NO-UNDO.
      DEFINE VARIABLE bervalvad AS CHARACTER NO-UNDO.
      DEFINE VARIABLE bermtrlvad AS CHARACTER NO-UNDO.
      DEFINE VARIABLE schaktvad AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE ejstart AS LOGICAL NO-UNDO.
      DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
      DEFINE VARIABLE nyKalkNrvar AS INTEGER NO-UNDO.
      IF BernrVar = 0 THEN RETURN FALSE.
      IF ejstart = FALSE THEN RUN KalkyleraiBeredning_UI IN AppServerHandle(INPUT 0,INPUT BernrVar,INPUT BerOmrVar,OUTPUT felmedd,OUTPUT nyKalkNrvar, OUTPUT ejstart).
      IF ejstart = TRUE THEN DO:
         IF felmedd NE "" THEN System.Windows.Forms.MessageBox:Show(felmedd).
         RETURN FALSE.
      END.
      
      berkalkvad = "BERKALKOPPLA.BERNR = " + STRING(BernrVar) + " AND BERKALKOPPLA.OMRADE = '" + BerOmrVar + "'".
      bervalvad =  "BERVAL.AONR = '" + STRING(BernrVar) + "' AND BERVAL.OMRADE = '" + BerOmrVar  + "'" /*AND BERVAL.KSKAP = FALSE" */.
      bermtrlvad =  "BERMTRL.AONR = '" + STRING(BernrVar) + "' AND BERMTRL.OMRADE = '" + BerOmrVar + "'".
      schaktvad =  "HDSCHAKT.BERNR = " + STRING(BernrVar) + " AND HDSCHAKT.OMRADE = '" + BerOmrVar + "'".
      
      
      RUN laddaBerkalkDS_UI IN AppServerHandle 
      (INPUT berkalkvad,
       INPUT bervalvad,
       INPUT bermtrlvad,
       INPUT schaktvad, 
       OUTPUT DATASET-HANDLE BerKalkDS BIND,
       OUTPUT DATASET-HANDLE BerValDS BIND,
       OUTPUT DATASET-HANDLE BerMtrlDS BIND,
       OUTPUT DATASET-HANDLE HdSchaktDS BIND,
       OUTPUT TABLE-HANDLE HdSchaktStrackatth).
       HdSchaktStrackabuffh = HdSchaktStrackatth:DEFAULT-BUFFER-HANDLE.
       /*THIS-OBJECT:HdSchaktStrackabuffh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
       IF THIS-OBJECT:HdSchaktStrackabuffh:AVAILABLE THEN DO:
          MESSAGE THIS-OBJECT:HdSchaktStrackabuffh:BUFFER-FIELD("FRANTILL"):BUFFER-VALUE THIS-OBJECT:HdSchaktStrackabuffh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE THIS-OBJECT:HdSchaktStrackabuffh:BUFFER-FIELD("KLID"):BUFFER-VALUE 
          VIEW-AS ALERT-BOX.
          
       END.*/
             
      IF VALID-HANDLE(BerKalkDS) THEN DO:
         BerKalkkopplabuffh = BerKalkDS:GET-BUFFER-HANDLE(1).
         Bervallbuffh = BerValDS:GET-BUFFER-HANDLE(1).
         Bermtrlbuffh = BerMtrlDS:GET-BUFFER-HANDLE(1).
         HdSchaktbuffh = HdSchaktDS:GET-BUFFER-HANDLE(1).
         /*inget ska 
         THIS-OBJECT:BerKalkTracking(TRUE).
         */
      END.
      IF VALID-HANDLE(Bervallbuffh) THEN THIS-OBJECT:NBervallttDef().
      THIS-OBJECT:ConnectDataSet().
      RETURN   TRUE.        
   END METHOD.
   METHOD PUBLIC VOID NBervallttDef():
       CREATE TEMP-TABLE NBervalltth /*IN WIDGET-POOL "KalkDynTable"*/.
       NBervalltth:CREATE-LIKE(Bervallbuffh).
       NBervalltth:TEMP-TABLE-PREPARE("eBervaltt").
       NBervallbuffh = NBervalltth:DEFAULT-BUFFER-HANDLE.
   END METHOD.
   
   
   METHOD PUBLIC VOID ConnectDataSet():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.  
      DEFINE VARIABLE BervallKskapTT AS HANDLE NO-UNDO.
      
      CREATE TEMP-TABLE BervallKskapTT.
      BervallKskapTT:CREATE-LIKE(THIS-OBJECT:Bervallbuffh). 
      BervallKskapTT:TEMP-TABLE-PREPARE("Kskapber").
      THIS-OBJECT:BervallKskapbuffh = BervallKskapTT:DEFAULT-BUFFER-HANDLE.
      queryvar = "FOR EACH " + THIS-OBJECT:Bervallbuffh:TABLE + " WHERE KSKAP = TRUE".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Bervallbuffh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("KONSKOD"):BUFFER-VALUE = 0 OR 
            LOOKUP(STRING(THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("KONSKOD"):BUFFER-VALUE), Guru.Konstanter:varforetypchar[5]) > 0 THEN DO: 
            THIS-OBJECT:BervallKskapbuffh:FIND-FIRST("WHERE  NUM = " + STRING(THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " AND SKAPNUM = 0 "  ,NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:BervallKskapbuffh:AVAILABLE THEN.
            ELSE DO:
               THIS-OBJECT:BervallKskapbuffh:BUFFER-CREATE().
               THIS-OBJECT:BervallKskapbuffh:BUFFER-COPY(THIS-OBJECT:Bervallbuffh, "SKAPNUM").
               THIS-OBJECT:BervallKskapbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE = " , " + "Allt materiel".
            END.
            
            THIS-OBJECT:BervallKskapbuffh:BUFFER-CREATE().
            THIS-OBJECT:BervallKskapbuffh:BUFFER-COPY(THIS-OBJECT:Bervallbuffh).
            IF THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE = "99" THEN DO:
               THIS-OBJECT:BervallKskapbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE = THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE
               + " , " + "Skåpmateriel".
            END.    
            ELSE DO:
               THIS-OBJECT:BervallKskapbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE = THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE
               + " , " + 
               THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("F2"):BUFFER-VALUE + " " +
               THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("F3"):BUFFER-VALUE + " " + 
               THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("F4"):BUFFER-VALUE + " " +
               THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("F5"):BUFFER-VALUE + " " +
               THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("F6"):BUFFER-VALUE.
            END.
         END. 
         THIS-OBJECT:Bervallbuffh:BUFFER-DELETE(). 
         qH:GET-NEXT().  
      END.
      /*
      bervalDSh:ADD-RELATION(BervallNskapbuffh,BervallKskapbuffh, "NUM,NUM").
      THIS-OBJECT:Bervallbuffh:FIND-FIRST("WHERE NUM =  " + STRING(BerKalkNum),NO-LOCK) NO-ERROR.
        */    
     
   END METHOD.
   
   
   METHOD PUBLIC LOGICAL HmtBerkalkMult():
      DEFINE VARIABLE berkalkvad AS CHARACTER NO-UNDO.
      DEFINE VARIABLE bervalvad AS CHARACTER NO-UNDO.
      DEFINE VARIABLE bermtrlvad AS CHARACTER NO-UNDO.
      DEFINE VARIABLE schaktvad AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE beri AS INTEGER NO-UNDO.
      FOR EACH berkalktt WHERE NO-LOCK:
         beri = beri + 1.
      
         IF berkalktt.BERNR = BernrVar AND berkalktt.OMRADE = BerOmrVar THEN NEXT.
        
         RUN LaddaKalkyl IN AppServerHandle (INPUT berkalktt.KALKNR,INPUT berkalktt.OMRADE,OUTPUT DATASET KalkylDS APPEND).
         
         berkalkvad = "BERKALKOPPLA.BERNR = " + STRING(berkalktt.BERNR) + " AND BERKALKOPPLA.OMRADE = '" + berkalktt.OMRADE + "'".
         bervalvad =  "BERVAL.AONR = '" + STRING(berkalktt.BERNR) + "' AND BERVAL.OMRADE = '" + berkalktt.OMRADE + "' AND BERVAL.KSKAP = FALSE".
         bermtrlvad =  "BERMTRL.AONR = '" + STRING(berkalktt.BERNR) + "' AND BERMTRL.OMRADE = '" + berkalktt.OMRADE + "'".
         schaktvad =  "HDSCHAKT.BERNR = " + STRING(berkalktt.BERNR) + " AND HDSCHAKT.OMRADE = '" + berkalktt.OMRADE + "'".
         
         RUN ReladdaBerkalkDS_UI IN AppServerHandle 
         (INPUT berkalkvad,
          INPUT bervalvad,
          INPUT bermtrlvad,
          INPUT schaktvad, 
          OUTPUT DATASET-HANDLE BerKalkDS  APPEND BIND,
          OUTPUT DATASET-HANDLE BerValDS APPEND BIND,
          OUTPUT DATASET-HANDLE BerMtrlDS APPEND BIND,
          OUTPUT DATASET-HANDLE HdSchaktDS APPEND BIND,
          OUTPUT TABLE-HANDLE HdSchaktStrackatth).
        HdSchaktStrackabuffh = HdSchaktStrackatth:DEFAULT-BUFFER-HANDLE.
        /*
          IF VALID-HANDLE(BerKalkDS) THEN DO:
            BerKalkkopplabuffh = BerKalkDS:GET-BUFFER-HANDLE(1).
            Bervallbuffh = BerValDS:GET-BUFFER-HANDLE(1).
            Bermtrlbuffh = BerMtrlDS:GET-BUFFER-HANDLE(1).
            HdSchaktbuffh = HdSchaktDS:GET-BUFFER-HANDLE(1).
          
         END.
         */
         THIS-OBJECT:ControlShell:RibbonToolVisSummeraBerKalk:Checked = TRUE.
        
         THIS-OBJECT:ControlShell:RibbonToolVisSummeraBerKalk:SharedPropsInternal:Enabled = FALSE.
          
      END.
      
      /*
      THIS-OBJECT:BerKalkkopplabuffh:FIND-FIRST("WHERE "  ) NO-ERROR.
        MESSAGE BerKalkkopplabuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE 
        VIEW-AS ALERT-BOX.
         THIS-OBJECT:BerKalkkopplabuffh:FIND-last("WHERE "  ) NO-ERROR.
        MESSAGE THIS-OBJECT:BerKalkkopplabuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE 
        VIEW-AS ALERT-BOX.
      */
      IF beri <= 1 THEN RETURN FALSE.
      ELSE RETURN TRUE. 
   END METHOD.
   METHOD PUBLIC VOID BerKalkNr(OUTPUT nrK AS INTEGER, OUTPUT oVar AS CHARACTER ):
      THIS-OBJECT:BerKalkkopplabuffh:FIND-FIRST("WHERE  ") NO-ERROR.
      nrK = THIS-OBJECT:BerKalkkopplabuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
      oVar = THIS-OBJECT:BerKalkkopplabuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
   END METHOD.
   
   
   
   METHOD PUBLIC HANDLE apphandle():
      RETURN AppServerHandle.
   END METHOD.
   METHOD PUBLIC VOID KalkNrGet(OUTPUT nrvar AS INTEGER,OUTPUT omr AS CHARACTER):
      ASSIGN 
      nrvar = ControlShell:KalkNrvar
      omr = ControlShell:Omradevar.
   END METHOD.
   METHOD PUBLIC VOID InitializeTTs():
      {KALKYLKATTTH.i}
      
      
   END METHOD.
   /*HÄMTAR UPP STARTEN TILL KALKYLEN  OCH SKAPAR KALKYLHUVUD*/
   METHOD PUBLIC LOGICAL CreateKalkyl():
      RUN LaddaKalkyl IN AppServerHandle (INPUT ?,INPUT ?,OUTPUT DATASET KalkylDS).
      THIS-OBJECT:KalkSpara().
      IF THIS-OBJECT:KalkTracking(TRUE) = FALSE THEN RETURN FALSE.
      EMPTY TEMP-TABLE ekalkhuvtt NO-ERROR.
      IF Guru.Konstanter:varforetypchar[3] = "" THEN  RUN startny_UI IN AppServerHandle 
      (INPUT Guru.Konstanter:globomr,INPUT Guru.Konstanter:globanv,OUTPUT TABLE ekalkhuvtt, OUTPUT TABLE kalkylkatalogtt).
      ELSE RUN startny_UI IN AppServerHandle 
      (INPUT Guru.Konstanter:varforetypchar[3],INPUT Guru.Konstanter:globanv,OUTPUT TABLE ekalkhuvtt, OUTPUT TABLE kalkylkatalogtt).
      FIND FIRST ekalkhuvtt NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekalkhuvtt THEN DO:
         RETURN FALSE.
      END.
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
   METHOD PUBLIC CHARACTER PlnrAonr(OUTPUT koppladtill AS CHARACTER,OUTPUT koppladbest AS CHARACTER):
      DEFINE VARIABLE beredarvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE refvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE ortnamn AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE kontakt AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE arbannsv AS CHARACTER NO-UNDO. 
      FIND FIRST kalkhuvtt  WHERE NO-LOCK NO-ERROR.
      FIND FIRST kalkaonrTT WHERE kalkaonrTT.KALKNR = kalkhuvtt.KALKNR AND kalkaonrTT.OMRADE = kalkhuvtt.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE kalkaonrTT THEN DO TRANSACTION:
         IF kalkaonrTT.AONR NE ? THEN DO:
            RUN AonrPlanrInfo_UI IN AppServerHandle (INPUT 1, INPUT kalkaonrTT.AONR,INPUT kalkaonrTT.DELNR,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
            koppladtill = kalkhuvtt.BENAMNING + "  ---Kopplad till " + Guru.Konstanter:gaok + " " + STRING(kalkaonrTT.AONR) + " " + STRING(kalkaonrTT.DELNR,Guru.Konstanter:varforetypchar[1]) + " " + 
            ortnamn + " ".
            
            Guru.Konstanter:BestKundTTh:FIND-FIRST("WHERE BESTID = '" + STRING(kalkhuvtt.BESTID) + "'") NO-ERROR.
            IF Guru.Konstanter:BestKundTTh:AVAILABLE THEN DO:
               koppladbest = STRING(Guru.Konstanter:BestKundTTh:BUFFER-FIELD("BESTNAMN"):BUFFER-VALUE) + " " + Guru.Konstanter:grefbefk + " " + refvar.
            END.
            ELSE DO:
               /*Anders Olsson Elpool i Umeå AB  4 sep 2016 18:00:53 
               pg älder kalkyler kan ha sparats med vibestid 
               */
               Guru.Konstanter:BestKundTTh:FIND-FIRST("WHERE VIBESTID = '" + STRING(kalkhuvtt.BESTID) + "'") NO-ERROR.
               IF Guru.Konstanter:BestKundTTh:AVAILABLE THEN DO:
                  koppladbest = STRING(Guru.Konstanter:BestKundTTh:BUFFER-FIELD("BESTNAMN"):BUFFER-VALUE) + " " + Guru.Konstanter:grefbefk + " " + refvar.
                  kalkhuvtt.BESTID = Guru.Konstanter:BestKundTTh:BUFFER-FIELD("BESTID"):BUFFER-VALUE.
                  THIS-OBJECT:Kalkspara().
               END.
            END.   
               
         END.
         ELSE IF kalkaonrTT.PLANNR NE ? THEN DO:
            RUN AonrPlanrInfo_UI IN AppServerHandle (INPUT 2, INPUT kalkaonrTT.PLANNR,INPUT kalkaonrTT.ARTAL,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
            koppladtill = kalkhuvtt.BENAMNING + "  ---Kopplad till " + Guru.Konstanter:gplk + " " + kalkaonrTT.PLANNR + " " + STRING(kalkaonrTT.ARTAL) + " " +  ortnamn.
         END. 
         ELSE DO:
            koppladtill = kalkhuvtt.BENAMNING.
         END.     
      END. 
      ELSE koppladtill = kalkhuvtt.BENAMNING.
      
   END METHOD.
   METHOD PUBLIC CHARACTER PlnrAonrMulti(kalknrvar AS INTEGER, omvar AS CHARACTER ):
      DEFINE VARIABLE beredarvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE utfardatvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE refvar AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE ortnamn AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE kontakt AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE arbannsv AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE koppladtill AS CHARACTER NO-UNDO.
      FIND FIRST kalkaonrTT WHERE kalkaonrTT.KALKNR = kalknrvar AND kalkaonrTT.OMRADE = omvar NO-LOCK NO-ERROR.
      IF AVAILABLE kalkaonrTT THEN DO TRANSACTION:
         IF kalkaonrTT.AONR NE ? THEN DO:
            RUN AonrPlanrInfo_UI IN AppServerHandle (INPUT 1, INPUT kalkaonrTT.AONR,INPUT kalkaonrTT.DELNR,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
            koppladtill = kalkhuvtt.BENAMNING + "  ---Kopplad till " + Guru.Konstanter:gaok + " " + STRING(kalkaonrTT.AONR) + " " + STRING(kalkaonrTT.DELNR,Guru.Konstanter:varforetypchar[1]) + " " + ortnamn.
         END.
         ELSE IF kalkaonrTT.PLANNR NE ? THEN DO:
            RUN AonrPlanrInfo_UI IN AppServerHandle (INPUT 2, INPUT kalkaonrTT.PLANNR,INPUT kalkaonrTT.ARTAL,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
            koppladtill = kalkhuvtt.BENAMNING + "  ---Kopplad till " + Guru.Konstanter:gplk + " " + kalkaonrTT.PLANNR + " " + STRING(kalkaonrTT.ARTAL) + " " +  ortnamn.
         END. 
         ELSE DO:
            koppladtill = kalkhuvtt.BENAMNING.
         END.     
      END. 
      ELSE koppladtill = kalkhuvtt.BENAMNING.
      RETURN koppladtill.  
   END METHOD.
   METHOD PUBLIC CHARACTER PlnrAonrMulti():
      DEFINE VARIABLE koppladtill AS CHARACTER NO-UNDO.
      koppladtill = "Kopplad till " + Guru.Konstanter:gaok + " ".
      FOR EACH kalkaonrTT WHERE NO-LOCK:
         IF kalkaonrTT.AONR = ? AND kalkaonrTT.PLANNR = ? THEN DO:
            koppladtill =  koppladtill + " " + ", ".
         END.   
         ELSE IF kalkaonrTT.AONR NE ? THEN DO:
            koppladtill =  koppladtill + STRING(kalkaonrTT.AONR) + " " + STRING(kalkaonrTT.DELNR,Guru.Konstanter:varforetypchar[1]) + ", ". 
         END.
         ELSE IF kalkaonrTT.PLANNR NE ? THEN DO:
            koppladtill = koppladtill + kalkaonrTT.PLANNR + " " + STRING(kalkaonrTT.ARTAL) + ", ".
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
        IF kalkhuvtt.ANMARKNING NE "" THEN kalkanrrmulti = kalkanrrmulti + STRING(kalkhuvtt.ANMARKNING) + ", ".
     END.
     FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
     RETURN kalkanrrmulti.
   END METHOD.
   METHOD PUBLIC VOID TotKalkSumm():
      DEFINE VARIABLE tk AS DECIMAL NO-UNDO.
      DEFINE VARIABLE fk AS DECIMAL NO-UNDO.
      FOR EACH kalknumttbuf BREAK BY kalknumttbuf.KALKNR:
         ACCUMULATE kalknumttbuf.TOTKOST (TOTAL BY kalknumttbuf.KALKNR). 
         ACCUMULATE kalknumttbuf.FRITOTKOST (TOTAL BY kalknumttbuf.KALKNR). 
         IF LAST-OF(kalknumttbuf.KALKNR) THEN DO:
            tk = ACCUM TOTAL BY kalknumttbuf.KALKNR kalknumttbuf.TOTKOST. 
            fk = ACCUM TOTAL BY kalknumttbuf.KALKNR kalknumttbuf.FRITOTKOST.               
         END.
      END.
      IF THIS-OBJECT:ControlShell = ? THEN.
      ELSE DO:
         THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:GuruUpdateTitle("Summa för Grundkalkyl :" + STRING(tk,"->>>>>>>>>>>>9")).
      END.
   END METHOD.
   /*SPARA KALKYLEN*/
   METHOD PUBLIC VOID KalkAnvEgenSpar():
       DEFINE VARIABLE chDS AS HANDLE NO-UNDO.
       THIS-OBJECT:KalkylAnvEgenTracking(FALSE).
       CREATE DATASET chDS.
       chDS:CREATE-LIKE(KalkylAnvEgenDS).
       chDS:GET-CHANGES(KalkylAnvEgenDS).
      
       RUN SparaDynDSstart_UI IN AppServerHandle (INPUT "KalkylAnvEgenDS", INPUT DATASET-HANDLE chDS).
     
       chDS:MERGE-CHANGES(KalkylAnvEgenDS).
       THIS-OBJECT:KalkylAnvEgenTracking(TRUE).
   END METHOD.
   METHOD PUBLIC LOGICAL KalkSpara():
      
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:TotKalkSumm().
      THIS-OBJECT:KalkTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET KalkylDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET KalkylDS:HANDLE).
      /*REJECT-CHANGES 1*/
 
      
      RUN SparaProDataSetKalkylDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET KalkylDS:HANDLE).

      /*REJECT-CHANGES 2*/
  
      
      
      IF THIS-OBJECT:KalkTracking(TRUE) = FALSE THEN RETURN FALSE.  
                  
      FIND FIRST kalkhuvtt  WHERE NO-LOCK NO-ERROR.
      RETURN TRUE. 
      
  
       /*REJECT-CHANGES 1*/
  /*
       REJECT-CHANGES( )
   DEBUGGER:SET-BREAK().
      DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      SPARAXML = "C:\CTest.xml". 
      hDSChanges:WRITE-XML("FILE", SPARAXML).
      SistasparaDsH = hDSChanges.  
MESSAGE "kalksparas"
VIEW-AS ALERT-BOX. 
      */       
      
      
/*REJECT-CHANGES 2*/
/*
 DEBUGGER:SET-BREAK().
      DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      SPARAXML = "C:\CTest.xml". 
      hDSChanges:WRITE-XML("FILE", SPARAXML).
      SPARAXML = "C:\CsTest.xml". 
      SistasparaDsH:WRITE-XML("FILE", SPARAXML).  
MESSAGE "kalksparas"
VIEW-AS ALERT-BOX. 
      KalkylDS:REJECT-CHANGES(DATASET hDSChanges:HANDLE).
  */    
      
      
      
       /*REJECT-CHANGES( )*/
      
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
      DEFINE VARIABLE dResult AS System.Windows.Forms.DialogResult NO-UNDO.
      
      cFile = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN cFile = webclienttempdir.
      cFile = cFile  + STRING(kalknr) + omr + kalkhuvtt.BENAMNING + ".xml".
      
      ASSIGN
      cTargetType       = "FILE"
      lFormatted        = NO
      cEncoding         = ?
      cSchemaLocation   = ?
      lWriteSchema      = no
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
         DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
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
            IF THIS-OBJECT:XmlImportDo(cFile) THEN RETURN TRUE.
         END.
      END.
      RETURN FALSE.
   END METHOD.
   
   METHOD PUBLIC LOGICAL XmlImportDo(INPUT cFile AS CHARACTER):
      DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cReadMode AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL NO-UNDO.
        
      IF CAPS(SUBSTRING(cFile, LENGTH(cFile) - 2)) = "XML" THEN DO: 
         DO ON ERROR UNDO, LEAVE: 
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
      kalkaonrTT.AONR   =  Guru.GlobalaVariabler:plusaonr
      kalkaonrTT.DELNR = Guru.GlobalaVariabler:plusdnr
      kalkaonrTT.PLANNR = Guru.GlobalaVariabler:plusplannr 
      kalkaonrTT.ARTAL = Guru.GlobalaVariabler:plusartal
      kalkaonrTT.KALKNR = nykalknr
      kalkaonrTT.OMRADE  = nyomrvar. 
      RUN StatusImportKoll_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,INPUT Guru.GlobalaVariabler:plusplannr,INPUT Guru.GlobalaVariabler:plusartal,INPUT kalkhuvtt.TYPKALK, OUTPUT kalkaonrTT.STATUSNIV).
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
      FOR EACH kalkfaktorertt WHERE NO-LOCK:
         ASSIGN 
         kalkfaktorertt.KALKNR = nykalknr
         kalkfaktorertt.OMRADE = nyomrvar.
         kalkfaktorertt.TTRECID = RECID(kalkfaktorertt).
      END.
      FOR EACH kalkegnaprisertt WHERE NO-LOCK:
         ASSIGN 
         kalkegnaprisertt.KALKNR = nykalknr
         kalkegnaprisertt.OMRADE = nyomrvar.
         kalkegnaprisertt.TTRECID = RECID(kalkegnaprisertt).
      END.
      
      /*Spara kalkyl*/
      THIS-OBJECT:KalkSpara(). 
      /*Ladda kalkyl*/
      THIS-OBJECT:KalkylimportTTh = TEMP-TABLE KalkylimportTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ControlShell:LoadKalkyl(INPUT nykalknr, INPUT nyomrvar, INPUT FALSE).
   /*   THIS-OBJECT:KopKonvBenamning().*/ 
   END METHOD.
   METHOD PUBLIC VOID KalkylAnvEgenTracking(INPUT onoff AS LOGICAL):
      IF onoff = TRUE THEN snabbspara = FALSE.
      ELSE snabbspara = TRUE.
      IF NOT VALID-HANDLE(Kalknumanvegentth) THEN Kalknumanvegentth = Kalknumanvegenbuffh:TABLE-HANDLE.
      IF NOT VALID-HANDLE(Kalknumanvegensubtth) THEN Kalknumanvegensubtth = Kalknumanvegensubbuffh:TABLE-HANDLE.
      Kalknumanvegentth:TRACKING-CHANGES = onoff.
      Kalknumanvegensubtth:TRACKING-CHANGES = onoff.
      
   END METHOD.
   METHOD PUBLIC VOID BerKalkTracking(INPUT onoff AS LOGICAL):
      /*
      IF NOT VALID-HANDLE(BerKalkyltth) THEN BerKalkyltth = BerKalkylbuffh:TABLE-HANDLE.
      BerKalkyltth:TRACKING-CHANGES = onoff.
      IF NOT VALID-HANDLE(HdKalkyltth) THEN HdKalkyltth = HdKalkylbuffh:TABLE-HANDLE.
      HdKalkyltth:TRACKING-CHANGES = onoff.
      IF NOT VALID-HANDLE(KalkbefBtth) THEN KalkbefBtth = KalkbefBbuffh:TABLE-HANDLE.
      KalkbefBtth:TRACKING-CHANGES = onoff.
      */
   END METHOD.
   METHOD PUBLIC LOGICAL KalkTracking(INPUT onoff AS LOGICAL):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE lasavanv AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lasavnamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lastav AS LOGICAL NO-UNDO.
      IF onoff = TRUE THEN DO:
         snabbspara = FALSE.
         FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
         IF AVAILABLE kalkhuvtt THEN DO:
            IF kalkhuvtt.KALKNR = 0 OR kalkhuvtt.KALKNR = ? THEN. 
            ELSE DO:
               FIND FIRST kalkylstopptt WHERE kalkylstopptt.KALKNR = kalkhuvtt.KALKNR AND kalkylstopptt.OMRADE = kalkhuvtt.OMRADE NO-LOCK NO-ERROR.
               IF AVAILABLE kalkylstopptt THEN.
               ELSE DO:
                  
                  RUN KalkStopp_UI IN AppServerHandle (INPUT 1,INPUT kalkhuvtt.KALKNR,INPUT kalkhuvtt.OMRADE, INPUT Guru.Konstanter:globanv, OUTPUT lasavanv, OUTPUT lasavnamn, OUTPUT lastav).
                  IF lastav = TRUE THEN DO:
                     IF Guru.Konstanter:globanv = lasavanv THEN DO:
                        rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(168) + " " + "Kalkyl " + STRING(kalkhuvtt.KALKNR) + " " +
                        lasavanv + " " + lasavnamn + ". " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(170)," ",
                        System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).            
                        IF rrr:ToString() EQ "Yes" THEN DO:
                           CREATE kalkylstopptt.
                           BUFFER-COPY kalkhuvtt TO kalkylstopptt. 
                        END.   
                        IF rrr:ToString() EQ "No" THEN DO:
                           RETURN FALSE.
                        END.    
                         
                     END.
                     ELSE DO:  
                        rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(168) + " " + "Kalkyl " + STRING(kalkhuvtt.KALKNR) + " " +
                        lasavanv + " " + lasavnamn + ". " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(169)," ",
                        System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Warning).
                        rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(168) + " " + "Kalkyl " + STRING(kalkhuvtt.KALKNR) + " " +
                        lasavanv + " " + lasavnamn + ". " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(170)," ",
                        System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).            
                        IF rrr:ToString() EQ "Yes" THEN DO:
                           RUN KalkStopp_UI IN AppServerHandle (INPUT 2,INPUT kalkhuvtt.KALKNR,INPUT kalkhuvtt.OMRADE, INPUT lasavanv, OUTPUT lasavanv, OUTPUT lasavnamn, OUTPUT lastav).
                           RUN KalkStopp_UI IN AppServerHandle (INPUT 1,INPUT kalkhuvtt.KALKNR,INPUT kalkhuvtt.OMRADE, INPUT Guru.Konstanter:globanv, OUTPUT lasavanv, OUTPUT lasavnamn, OUTPUT lastav).
                           CREATE kalkylstopptt.
                           BUFFER-COPY kalkhuvtt TO kalkylstopptt. 
                        END.   
                        IF rrr:ToString() EQ "No" THEN DO:
                           RETURN FALSE.
                        END. 
                        
                     END.
                  END.
                  ELSE DO:
                     CREATE kalkylstopptt.
                     BUFFER-COPY kalkhuvtt TO kalkylstopptt.
                  END.   
               END.      
            END.
         END.       
      END.
      ELSE snabbspara = TRUE.
      TEMP-TABLE kalknumtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalknumsubtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkhuvtt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkaonrTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkfaktorertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkegnaprisertt:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalktmtrlTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE kalkttidlageTT:TRACKING-CHANGES = onoff.
       
      RETURN TRUE.              
   END METHOD.
   METHOD PUBLIC VOID RibbonToolDelKatFill():
      DEFINE VARIABLE wTemp AS Infragistics.Win.ValueListItem NO-UNDO.
      THIS-OBJECT:ControlShell:RibbonToolDelKat:ValueList:ValueListItems:Clear().
      CREATE kalkyldelkatalogtt.
      kalkyldelkatalogtt.BENAMNING = "Alla".
      FOR EACH kalkyldelkatalogtt BY kalkyldelkatalogtt.KLOGSUBID:
         wTemp = NEW Infragistics.Win.ValueListItem().
         wTemp:DisplayText = kalkyldelkatalogtt.BENAMNING.
         wTemp:DataVALUE = NEW Helpers.ABLDataContainer().
         CAST(wTemp:DataVALUE, Helpers.ABLDataContainer):CreateData("DelK", kalkyldelkatalogtt.KLOGSUBID).
         THIS-OBJECT:ControlShell:RibbonToolDelKat:ValueList:ValueListItems:Add(wTemp).
      END. 
   END METHOD.    
   /*HÄMTAR KALATLOGER MM OCH ALLT SOM RÖR KALKYLEN*/
   METHOD PUBLIC LOGICAL FetchKalkyl(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO. 
      DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
     
      RUN kathmt_UI IN AppServerHandle (INPUT kalknr,INPUT omr, INPUT Guru.Konstanter:globanv,OUTPUT felmed, OUTPUT TABLE kalkylarbkodertt,
      OUTPUT TABLE kalkylloppostertt,OUTPUT TABLE kalkylkatalogtt, OUTPUT TABLE markfiltertt,OUTPUT TABLE kalkyldelkatalogtt).
      THIS-OBJECT:RibbonToolDelKatFill().
      THIS-OBJECT:FetchPriser(kalknr, omr).
      THIS-OBJECT:LaddaKalkyl(INPUT kalknr,INPUT omr).
      THIS-OBJECT:AnvKalkyl (INPUT kalknr,INPUT omr).  
      IF THIS-OBJECT:KalkTracking(TRUE) = FALSE THEN RETURN FALSE.
      THIS-OBJECT:KalknumKomp().
      IF felmed NE "" THEN DO:
         res = System.Windows.Forms.MessageBox:Show(felmed,"",System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.         
      END.   
      FIND FIRST kalkhuvtt  WHERE NO-LOCK NO-ERROR. 
      FIND FIRST kalkylkatalogtt WHERE NO-LOCK NO-ERROR.
      FIND FIRST markfiltertt WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkylkatalogtt THEN DO:        
         res = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(53), "", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.
      END.
     
      THIS-OBJECT:Root:DatabaseManager:Global:FetchDefaultValues("KALKYL",Guru.GlobalaVariabler:GuruDefaultAnv,kalknr, omr).
      THIS-OBJECT:KatAnvEgen().
      RETURN TRUE.
   END METHOD.
   /*HÄMTAR KALYL DATASET*/
   METHOD PUBLIC VOID LaddaKalkyl(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):
      
      RUN LaddaKalkyl IN AppServerHandle (INPUT kalknr,INPUT omr,OUTPUT DATASET KalkylDS).
      
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN DO:
         
         THIS-OBJECT:LaddaByggNr().
         THIS-OBJECT:KalkTracking(TRUE).
         FOR EACH kalknumtt WHERE kalknumtt.TOTKOST = 0 NO-LOCK:
            THIS-OBJECT:RaknaEnKod(FALSE).
         END.      
      END.
      THIS-OBJECT:TotKalkSumm().
      
   END METHOD.
   METHOD PUBLIC VOID LaddaByggNr():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      queryvar =  "FOR EACH " + THIS-OBJECT:Bervallbuffh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Bervallbuffh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:         
         FOR EACH kalknumtt WHERE kalknumtt.BERNUM = THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE AND (kalknumtt.ID2 = "" OR kalknumtt.EXTRA1 = ""): 
            kalknumtt.ID2 = THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE .
            kalknumtt.EXTRA1 = THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE .
         END.   
         qH:GET-NEXT().    
      END.

      queryvar =  "FOR EACH " + THIS-OBJECT:HdSchaktStrackabuffh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:HdSchaktStrackabuffh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         FOR EACH kalknumtt WHERE kalknumtt.MATRIS = THIS-OBJECT:HdSchaktStrackabuffh:BUFFER-FIELD("KLID"):BUFFER-VALUE AND kalknumtt.SID NE 0 AND
         kalknumtt.SID = THIS-OBJECT:HdSchaktStrackabuffh:BUFFER-FIELD("SID"):BUFFER-VALUE AND kalknumtt.FRANTILL = "" : 
            kalknumtt.FRANTILL = THIS-OBJECT:HdSchaktStrackabuffh:BUFFER-FIELD("FRANTILL"):BUFFER-VALUE .            
         END.   
         qH:GET-NEXT().    
      END.
      
   END METHOD.
   METHOD PUBLIC VOID KalknumKomp():
      
      FOR EACH kalknumtt WHERE kalknumtt.KLOGSUBID = 0 NO-LOCK:
         FIND FIRST kalkylloppostertt WHERE trim(kalkylloppostertt.ARBKOD) = TRIM(kalknumtt.ARBKOD) AND 
         kalkylloppostertt.LOPNR = kalknumtt.LOPNR  NO-LOCK NO-ERROR.
         IF AVAILABLE kalkylloppostertt THEN DO:
            BUFFER-COPY kalkylloppostertt EXCEPT kalkylloppostertt.TKOST TO kalknumtt.
         END.  
         THIS-OBJECT:skapanumsub().
      END.
      THIS-OBJECT:KalkSpara().
   END METHOD.
   /*visa kalkyl från huvudmeny*/
   METHOD PUBLIC LOGICAL FetchVisaKalkyl(INPUT extravaldfastth AS HANDLE):
      DEFINE VARIABLE res AS System.Windows.Forms.DialogResult NO-UNDO. 
      DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      extravaldfastth:FIND-FIRST() NO-ERROR.
      THIS-OBJECT:ControlShell:KalkNrvar = extravaldfastth:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
      THIS-OBJECT:ControlShell:Omradevar = extravaldfastth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.      
      RUN kathmt_UI IN AppServerHandle (INPUT extravaldfastth:BUFFER-FIELD("KALKNR"):BUFFER-VALUE,INPUT extravaldfastth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE, INPUT Guru.Konstanter:globanv,OUTPUT felmed, OUTPUT TABLE kalkylarbkodertt,
      OUTPUT TABLE kalkylloppostertt,OUTPUT TABLE kalkylkatalogtt, OUTPUT TABLE markfiltertt,OUTPUT TABLE kalkyldelkatalogtt).
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
     
      THIS-OBJECT:Root:DatabaseManager:Global:FetchDefaultValues("KALKYL",Guru.GlobalaVariabler:GuruDefaultAnv,THIS-OBJECT:ControlShell:KalkNrvar, THIS-OBJECT:ControlShell:Omradevar).
      RETURN TRUE.
   END METHOD.
   /*hämtar dataset som är skapade i method ovan för att kunna använda någon annstans */
   METHOD PUBLIC VOID HmtLaddaKalkylDs(OUTPUT DATASET KalkylDS):
    /*
      THIS-OBJECT:KalkSpara().
      */
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
   METHOD PUBLIC VOID FetchDefv(INPUT kalknr AS INTEGER, INPUT omr AS CHARACTER):
      THIS-OBJECT:Root:DatabaseManager:Global:FetchDefaultValues("KALKYL",Guru.GlobalaVariabler:GuruDefaultAnv,kalknr, omr).
      THIS-OBJECT:GuruDefaultsTTh = THIS-OBJECT:Root:DatabaseManager:Global:GuruDefaultsTTh.
   END METHOD.
   /*hämtar mallar*/
   METHOD PUBLIC VOID MallarHmt():
      RUN LaddaMallar IN AppServerHandle (INPUT 0 ,OUTPUT DATASET KalkylMallarDS).
      
   END METHOD.
   /*för att hämta och spara volym*/
   METHOD PUBLIC VOID VolymSpar():
       DEFINE VARIABLE chDS AS HANDLE NO-UNDO.
       THIS-OBJECT:VolymberTracking(FALSE).
       CREATE DATASET chDS.
       chDS:CREATE-LIKE(VolymDS).
       chDS:GET-CHANGES(VolymDS).
       /*
       DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      SPARAXML = "C:\CTest.xml". 
      VolymDS:WRITE-XML("FILE", SPARAXML).
       */ 
      
       RUN SparaDynDSstart_UI IN AppServerHandle (INPUT "VolymDS", INPUT DATASET-HANDLE chDS).
       chDS:MERGE-CHANGES(VolymDS).
     
       THIS-OBJECT:VolymberTracking(TRUE).
       
   END METHOD.
   METHOD PUBLIC VOID VolymDSLadda():
      VolymDS = ?.
      RUN laddaScHVolymDS_UI IN AppServerHandle (INPUT Guru.GlobalaVariabler:BeredningsNummer, INPUT Guru.GlobalaVariabler:BeredningsOmrade,INPUT Guru.GlobalaVariabler:SchaktSid,OUTPUT DATASET-HANDLE VolymDS BIND).
     
      IF VALID-HANDLE(VolymDS) THEN DO:
         VolymberTTbuffh = VolymDS:GET-BUFFER-HANDLE(1).
         THIS-OBJECT:VolymberTracking(TRUE).
         
      END.
   
   END METHOD. 
   METHOD PUBLIC VOID KalVolymDSLadda():
      DEFINE VARIABLE klidvar AS INTEGER NO-UNDO.
      VolymDS = ?.
      RUN laddaKalkVolymDS_UI IN AppServerHandle (INPUT THIS-OBJECT:ControlShell:KalkNrvar, INPUT THIS-OBJECT:ControlShell:Omradevar,INPUT 0,INPUT 0,OUTPUT DATASET-HANDLE VolymDS BIND).
     
      IF VALID-HANDLE(VolymDS) THEN DO:
         VolymberTTbuffh = VolymDS:GET-BUFFER-HANDLE(1).
        
         THIS-OBJECT:VolymberTracking(TRUE).
         
         VolymberTTbuffh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
         IF VolymberTTbuffh:AVAILABLE THEN.
         ELSE DO:
            REPEAT:     
               VolymberTTbuffh:BUFFER-CREATE().
               klidvar = klidvar + 1.
               VolymberTTbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = THIS-OBJECT:ControlShell:KalkNrvar.
               VolymberTTbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = THIS-OBJECT:ControlShell:Omradevar.
               VolymberTTbuffh:BUFFER-FIELD("KLID"):BUFFER-VALUE = klidvar.
               VolymberTTbuffh:BUFFER-FIELD("STRACKA"):BUFFER-VALUE = "Tilläggssträcka " + STRING(klidvar).
               VolymberTTbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = VolymberTTbuffh:RECID.
               IF klidvar = 3 THEN LEAVE.   
            END.
         END.
      END.
   
   END METHOD.  
     
       /*schakt*/
   METHOD PUBLIC VOID VolymberTracking(INPUT onoff AS LOGICAL):
      IF NOT VALID-HANDLE(VolymberTTh) THEN VolymberTTh = VolymberTTbuffh:TABLE-HANDLE.
      VolymberTTh:TRACKING-CHANGES = onoff.
      
     
   END METHOD.
   
   /*BERÄKNAR SCHAKTVOLYM från schakt*/
   METHOD PUBLIC VOID VolymBer():
      DEFINE VARIABLE tangens AS DECIMAL NO-UNDO.
      DEFINE VARIABLE breddvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE djupvar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE langdvar AS DECIMAL NO-UNDO.  
      DEFINE VARIABLE markvar AS DECIMAL NO-UNDO.  

      ASSIGN
      tangens = 0.26795
      breddvar = THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("SCHAKTBREDD"):BUFFER-VALUE / 100
      djupvar = THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("SCHAKTDJUP"):BUFFER-VALUE / 100
      langdvar = THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("SCHAKTLANGD"):BUFFER-VALUE.  
          
      IF  THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("SVARMARK"):BUFFER-VALUE = TRUE THEN markvar = 0.2.
      ELSE markvar = 0.1.       
      THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("TOTVOLYM"):BUFFER-VALUE = ((breddvar * djupvar + djupvar * tangens * djupvar) * langdvar).      
      IF THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("RORFOR"):BUFFER-VALUE = TRUE THEN DO:
         ASSIGN 
         THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("SKYDDFYLLNING"):BUFFER-VALUE = 0
         THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("TILLAGGVOLYM"):BUFFER-VALUE = THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("TOTVOLYM"):BUFFER-VALUE.
      END.
      ELSE DO:
         ASSIGN
         THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("SKYDDFYLLNING"):BUFFER-VALUE    = ((breddvar * markvar + markvar * tangens * markvar) * langdvar)
         THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("TILLAGGVOLYM"):BUFFER-VALUE = THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("TOTVOLYM"):BUFFER-VALUE - THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("SKYDDFYLLNING"):BUFFER-VALUE   .
      END. 
      /*ANTALET KVADRAT METER SOM YTBELÄGGNINGEN UTGÖR*/            
      THIS-OBJECT:VolymberTTbuffh:BUFFER-FIELD("YTBELAGGD"):BUFFER-VALUE  = ((2 * djupvar * tangens) + breddvar) * langdvar.
      
      
            
      
   END METHOD. 
   
   METHOD PUBLIC VOID VolymBerKalk():
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
   
   /*endast från kalkyl ej schakt*/
   
   /*endast från kalkyl ej schakt*/
   METHOD PUBLIC VOID VolymBerHmt():
    
     
      /*
      CREATE VolymberTT.
      VolymberTT.TTRECID = RECID(VolymberTT). 
      VolymberTT.STRACKA = "Tilläggssträcka 1 ". 
      CREATE VolymberTT.
      VolymberTT.TTRECID = RECID(VolymberTT).
      VolymberTT.STRACKA = "Tilläggssträcka 2 ".
      CREATE VolymberTT.
      VolymberTT.TTRECID = RECID(VolymberTT).
      VolymberTT.STRACKA = "Tilläggssträcka 3 ".
      */
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
   METHOD PUBLIC VOID FrekvensHmtAppend(INPUT klsubid AS INTEGER, INPUT arbkodvar AS CHARACTE, INPUT lopnrvar AS INTEGER):
      RUN LaddaFrekvensHmt IN AppServerHandle (INPUT klsubid,INPUT arbkodvar, INPUT lopnrvar,OUTPUT DATASET KalkylFrekDS APPEND).                 
   END METHOD.
   /*HÄMTAR AVTALSKALKYLEN*/
   METHOD PUBLIC VOID FetchAvtal(INPUT Kartal AS INTEGER):
      RUN avtalhmt IN AppServerHandle (INPUT Kartal, OUTPUT TABLE Avtalskalktt).
      
   END METHOD.
   /*SPARAR PRISER OCH FAKTORER*/
   METHOD PUBLIC VOID SavePrisFakt():
      DEFINE VARIABLE erid AS ROWID NO-UNDO.
      DEFINE VARIABLE frid AS ROWID NO-UNDO.
      IF THIS-OBJECT:EgnaPriserTTh:AVAILABLE THEN erid = THIS-OBJECT:EgnaPriserTTh:ROWID. 
      ELSE erid = ?.     
      IF THIS-OBJECT:FaktorerTTh:AVAILABLE THEN frid = THIS-OBJECT:FaktorerTTh:ROWID.
      ELSE frid = ?.
      THIS-OBJECT:KalkSpara().
      IF erid = ? THEN THIS-OBJECT:EgnaPriserTTh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      ELSE THIS-OBJECT:EgnaPriserTTh:FIND-BY-ROWID(erid,NO-LOCK) NO-ERROR.
      IF frid = ? THEN THIS-OBJECT:FaktorerTTh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      ELSE THIS-OBJECT:FaktorerTTh:FIND-BY-ROWID(frid,NO-LOCK) NO-ERROR.                   
                                   
   END METHOD.
    /*enbart hjälp vid räkning mellan priser och procent*/
   METHOD PUBLIC VOID Beraknaprisproc(INPUT kolnamn  AS CHARACTER,kolvarde AS CHARACTER):
           
      IF THIS-OBJECT:EgnaPriserTTh:AVAILABLE THEN DO:  
            
         IF STRING(THIS-OBJECT:EgnaPriserTTh:BUFFER-FIELD(kolnamn)) = kolnamn AND THIS-OBJECT:EgnaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde THEN RETURN.
         THIS-OBJECT:EgnaPriserTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
         
         FIND FIRST kalkhuvtt WHERE kalkhuvtt.KALKNR = kalkegnaprisertt.KALKNR AND kalkhuvtt.OMRADE = kalkegnaprisertt.OMRADE NO-LOCK NO-ERROR.
         
         FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalkegnaprisertt.KLOGSUBID AND kalkylprisertt.KPID = kalkegnaprisertt.KPID NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kalkylprisertt THEN DO:
            FIND FIRST kalkylprisertt WHERE  kalkylprisertt.KPID = kalkegnaprisertt.KPID NO-LOCK NO-ERROR.
         END.
         IF AVAILABLE kalkylprisertt THEN DO:
            IF kolnamn = "PRIS" THEN DO:
               IF AVAILABLE kalkegnaprisertt THEN DO:
                  IF kalkylprisertt.PRIS > 0 THEN DO:
                     kalkegnaprisertt.PROCENT = ((kalkegnaprisertt.PRIS / kalkylprisertt.PRIS) - 1 ) * 100.                     
                  END.                     
               END.
            END.
            IF kolnamn = "PROCENT" THEN DO:
               IF AVAILABLE kalkegnaprisertt THEN DO:
                  IF kalkegnaprisertt.PROCENT > 0 AND kalkylprisertt.PRIS > 0 THEN DO:
                     kalkegnaprisertt.PRIS =  kalkylprisertt.PRIS *  ( 1 + (kalkegnaprisertt.PROCENT / 100 )).                     
                  END.                     
                  ELSE DO :
                     kalkegnaprisertt.PRIS =  kalkylprisertt.PRIS.
                     kalkegnaprisertt.PROCENT = 0.
                  END.   
               END.
            END.
         END.               
         THIS-OBJECT:ControlShell:KalkFaktControl:GridPriser:GuruReopen().
         
      END.                
   END METHOD.
   
    /*enbart hjälp vid räkning mellan priser och procent*/
   METHOD PUBLIC VOID Beraknaprocalla():
      FOR EACH kalkegnaprisertt:         
         FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalkegnaprisertt.KLOGSUBID AND kalkylprisertt.KPID = kalkegnaprisertt.KPID NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kalkylprisertt THEN DO:
            FIND FIRST kalkylprisertt WHERE kalkylprisertt.KPID = kalkegnaprisertt.KPID NO-LOCK NO-ERROR.
         END.
         IF AVAILABLE kalkylprisertt THEN DO:
            IF kalkylprisertt.PRIS > 0 THEN DO:
               kalkegnaprisertt.PROCENT = ((kalkegnaprisertt.PRIS / kalkylprisertt.PRIS ) - 1 ) * 100.                     
            END.
         END.
      END.                            
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
            IF NOT AVAILABLE kalkylprisertt THEN DO:
               FIND FIRST kalkylprisertt WHERE  kalkylprisertt.KPID = kalknumsubtt.KPID NO-LOCK NO-ERROR.
            END.
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
   
   /*VID Ny KALKYL BLA. SPARA */
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
      EMPTY TEMP-TABLE kodantaltemp NO-ERROR. 
      FOR EACH kalknumtt WHERE NO-LOCK:
         CREATE kodantaltemp.
         BUFFER-COPY kalknumtt TO kodantaltemp.
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
      /*Anders Olsson Elpool i Umeå AB  17 feb 2016 13:50:38 
      om inte det finns timmar på någon post. 
      */
      
      /*   
      FIND FIRST kalkantal WHERE kalkantal.TIMTYP = "Montör" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkantal THEN DO:
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
      END.
      */
        
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
      
      
      /*Anders Olsson Elpool i Umeå AB  8 apr 2016 18:25:36 
     
      */
     
      IF Guru.GlobalaVariabler:FranUppf = TRUE THEN DO:
         FIND FIRST kalkhuvtt WHERE NO-LOCK NO-ERROR.
         IF kalkhuvtt.UTYP =  3 THEN DO:
            /*MTRL FRÅN BEREDNING*/
            FIND FIRST kalkvisningtt WHERE kalkvisningtt.KOSTTYP = "Materiel" NO-LOCK NO-ERROR.
            IF AVAILABLE kalkvisningtt THEN DO:
               FOR EACH kalkkostnad WHERE kalkkostnad.KVID = kalkvisningtt.KVID NO-LOCK:
                  ASSIGN
                  kalkkostnad.TOTKOST = 0
                  kalkkostnad.FRITOTKOST = 0.
               END.   
            END.
            THIS-OBJECT:BeredningMtrlHmtUppf().
            FIND FIRST kalkkostnad WHERE kalkkostnad.KVID = kalkvisningtt.KVID NO-LOCK NO-ERROR.
            kalkkostnad.TOTKOST = THIS-OBJECT:MtrlKost(?).   
            kalkkostnad.FRITOTKOST = THIS-OBJECT:MtrlKost(?).         
         END.  
         ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
         THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "3" THEN DO:
            /*eget mtrl*/
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
            kalkkostnad.FRITOTKOST = THIS-OBJECT:MtrlKost(?).   
         END.
         ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "2" THEN DO:
            /*tillkommande eget mtrl*/
            FIND FIRST kalkvisningtt WHERE kalkvisningtt.KOSTTYP = "Materiel" NO-LOCK NO-ERROR.
            IF AVAILABLE kalkvisningtt THEN DO:
               CREATE kalkkostnad.
               ASSIGN 
               kalkkostnad.KALKNR = kalkhuvtt.KALKNR
               kalkkostnad.KVID = kalkvisningtt.KVID.
               kalkkostnad.TOTKOST = THIS-OBJECT:MtrlKost(?).
               kalkkostnad.FRITOTKOST = THIS-OBJECT:MtrlKost(?).  
            END.    
         END. 
      END.    
      ELSE DO:
         /*eget mtrl*/       
         IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "1" OR 
         THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisPriser") = "3"
         THEN DO:
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
      IF THIS-OBJECT:BeredningMtrlFinns() = TRUE THEN DO:
         FIND FIRST KalkRubrikTT WHERE KalkRubrikTT.KOSTTIMM = TRUE  AND KalkRubrikTT.BENAMNING = "Materiel" NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KalkRubrikTT THEN DO:
            FIND FIRST kalkvisningtt WHERE kalkvisningtt.KOSTTYP = "Materiel" NO-LOCK NO-ERROR.
            IF AVAILABLE kalkvisningtt THEN DO:  
               CREATE KalkRubrikTT.
               ASSIGN  
               KalkRubrikTT.BENAMNING = kalkvisningtt.KOSTTYP
               KalkRubrikTT.KVID = kalkvisningtt.KVID
               KalkRubrikTT.KOSTTIMM = TRUE 
               KalkRubrikTT.ORDNING = kalkvisningtt.ORDNINGKOSTNAD.
            END.       
         END.
      END. 
   END METHOD.
   METHOD PUBLIC VOID RaknaEgen():
      FOR EACH kalknumtt WHERE kalknumtt.ARBKOD = "EGEN" NO-LOCK:
         THIS-OBJECT:RaknaEnKod(FALSE).
      END.
   END METHOD.
   /*beräknakoden egen sparad*/
   METHOD PUBLIC VOID RaknaAnvEgen():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      
      fritotkostvar = 0.
      totkostvar = 0.
      kalkantalbuffh:EMPTY-TEMP-TABLE ().
      kalkkostnadbuffh:EMPTY-TEMP-TABLE ().
      queryvar =  "FOR EACH " + THIS-OBJECT:Kalknumanvegensubbuffh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE). 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Kalknumanvegensubbuffh,queryvar).
      qH:GET-FIRST().
  
      DO WHILE qH:QUERY-OFF-END = FALSE:
         kalkantalbuffh:BUFFER-CREATE().
         kalkantalbuffh:BUFFER-COPY(Kalknumanvegenbuffh).
         
         kalkkostnadbuffh:BUFFER-CREATE().
         kalkkostnadbuffh:BUFFER-COPY(Kalknumanvegenbuffh).
         THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE = 0.
         THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE = 0.
                   
         FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE AND 
         kalkylprisertt.KPID = THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("KPID"):BUFFER-VALUE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kalkylprisertt THEN DO:
            FIND FIRST kalkylprisertt WHERE kalkylprisertt.KPID = THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("KPID"):BUFFER-VALUE NO-LOCK NO-ERROR.
         END.
         FIND FIRST kalkvisningtt WHERE kalkvisningtt.KVID = kalkylprisertt.KVID NO-LOCK NO-ERROR.
         IF AVAILABLE kalkvisningtt THEN DO:    
            THIS-OBJECT:kalkantalbuffh:BUFFER-FIELD("TIMTYP"):BUFFER-VALUE = kalkvisningtt.TIMTYP.
            THIS-OBJECT:kalkantalbuffh:BUFFER-FIELD("KPID"):BUFFER-VALUE = kalkylprisertt.KPID.
            THIS-OBJECT:kalkantalbuffh:BUFFER-FIELD("KVID"):BUFFER-VALUE = kalkylprisertt.KVID.
            THIS-OBJECT:kalkantalbuffh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE =
            THIS-OBJECT:kalkantalbuffh:BUFFER-FIELD("SUMMA"):BUFFER-VALUE  + 
            THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE .

            THIS-OBJECT:kalkantalbuffh:BUFFER-FIELD("FRISUMMA"):BUFFER-VALUE =
            THIS-OBJECT:kalkantalbuffh:BUFFER-FIELD("FRISUMMA"):BUFFER-VALUE  + 
            THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("FRITIMMAR"):BUFFER-VALUE * THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE .
                
             
            THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("KOSTTYP"):BUFFER-VALUE = kalkvisningtt.KOSTTYP.
            THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("KPID"):BUFFER-VALUE = kalkylprisertt.KPID.
            THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("KVID"):BUFFER-VALUE = kalkylprisertt.KVID. 
          
            THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE = 
            THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE +
            (THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("TIMMAR"):BUFFER-VALUE * THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE * THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("PRIS"):BUFFER-VALUE) +
            (THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE * THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("KOSTNAD"):BUFFER-VALUE + THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("AVRUND"):BUFFER-VALUE).
            
            THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE = 
            THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE +
            (THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("FRITIMMAR"):BUFFER-VALUE * THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE * THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("FRIPRIS"):BUFFER-VALUE) +
            (THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE + THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("FRIKOSTNAD"):BUFFER-VALUE + THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("FRIAVRUND"):BUFFER-VALUE).
             
            totkostvar = totkostvar + THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.
            fritotkostvar = fritotkostvar + THIS-OBJECT:kalkkostnadbuffh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE.
         END.
         
         qH:GET-NEXT().
      END.
      
      THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE = fritotkostvar.
      THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE = totkostvar.
      kalkantalbuffh:EMPTY-TEMP-TABLE ().
      kalkkostnadbuffh:EMPTY-TEMP-TABLE ().
      THIS-OBJECT:KalkAnvEgenSpar().  
        
      
  END METHOD.
   
   
   
   /*beräknakoden*/
   METHOD PUBLIC VOID RaknaEnKod(allakoder AS LOGICAL):
   
      fritotkostvar = 0.
      totkostvar = 0.
      EMPTY TEMP-TABLE kalkantal NO-ERROR. 
      EMPTY TEMP-TABLE kalkkostnad NO-ERROR.     
      IF allakoder = FALSE THEN DO:
         IF NOT AVAILABLE kalknumtt THEN DO:
            FIND FIRST kalknumtt WHERE kalknumtt.KALKNR = kalknumsubtt.KALKNR AND kalknumtt.NUM = kalknumsubtt.NUM NO-ERROR. 
         END.
         OPEN QUERY ksq FOR EACH kalknumsubtt WHERE kalknumsubtt.KALKNR = kalknumtt.KALKNR AND kalknumsubtt.NUM = kalknumtt.NUM NO-LOCK.
         GET FIRST ksq NO-LOCK.     
      END.
      ELSE DO:
         OPEN QUERY ksq FOR EACH kalknumsubtt  NO-LOCK BY kalknumsubtt.NUM BY kalknumsubtt.NUMSUB.
         GET FIRST ksq NO-LOCK.
      END.    
      DO WHILE AVAILABLE(kalknumsubtt):
         IF allakoder = TRUE THEN DO:
            FIND FIRST kalknumtt WHERE kalknumtt.KALKNR = kalknumsubtt.KALKNR AND kalknumtt.NUM = kalknumsubtt.NUM NO-ERROR.  
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
         IF NOT AVAILABLE kalkylprisertt THEN DO:
            FIND FIRST kalkylprisertt WHERE  kalkylprisertt.KPID = kalknumsubtt.KPID AND  kalkylprisertt.EGENKODUPP = TRUE 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kalkylprisertt THEN DO:
               FIND FIRST kalkylprisertt WHERE  kalkylprisertt.KPID = kalknumsubtt.KPID  
               NO-LOCK NO-ERROR.
            END.
         END.
         FIND FIRST kalkvisningtt WHERE kalkvisningtt.KVID = kalkylprisertt.KVID NO-LOCK NO-ERROR.
         
         IF AVAILABLE kalkvisningtt THEN DO:
            kalkantal.TIMTYP = kalkvisningtt.TIMTYP.
            kalkkostnad.KOSTTYP = kalkvisningtt.KOSTTYP.
         END.       
            ASSIGN 
            
            kalkantal.KPID = kalkylprisertt.KPID
            kalkantal.KVID = kalkylprisertt.KVID.   
            IF allakoder = TRUE THEN DO:           
               kalkantal.SUMMA = kalkantal.SUMMA + KalkBerAntal(kalknumsubtt.TIMMAR,kalkantal.KVID) * kalknumtt.ANTAL.
               kalkantal.FRISUMMA = kalkantal.FRISUMMA + KalkBerAntal(kalknumsubtt.FRITIMMAR,kalkantal.KVID) * kalknumtt.ANTAL.
            END.
            ELSE DO:
               kalkantal.SUMMA = kalkantal.SUMMA + kalknumsubtt.TIMMAR * kalknumtt.ANTAL.
               kalkantal.FRISUMMA = kalkantal.FRISUMMA + kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL.
            END.      
            ASSIGN 
            
            kalkkostnad.KPID = kalkylprisertt.KPID
            kalkkostnad.KVID = kalkylprisertt.KVID. 
            /*kalkberin*/   
            IF allakoder = TRUE THEN DO: 
               IF kalknumtt.ARBKOD = "EGEN" THEN DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + KalkBerKost(1,kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalknumsubtt.PRIS,kalkkostnad.KVID)
                  + KalkBerKost(2,kalknumtt.ANTAL * kalknumsubtt.KOSTNAD,kalkkostnad.KVID) + kalknumsubtt.AVRUND.
                  
                  kalkkostnad.FRITOTKOST = kalkkostnad.FRITOTKOST + KalkBerKost(1,kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL * kalknumsubtt.FRIPRIS,kalkkostnad.KVID)
                  + KalkBerKost(2,kalknumtt.ANTAL * kalknumsubtt.FRIKOSTNAD,kalkkostnad.KVID) + kalknumsubtt.FRIAVRUND.
                  
               END.
               ELSE DO:
                  
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + KalkBerKost(1,kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalkylprisertt.PRIS,kalkkostnad.KVID)
                  + KalkBerKost(2,kalknumtt.ANTAL * kalknumsubtt.KOSTNAD,kalkkostnad.KVID) + kalknumsubtt.AVRUND.
                  /*Anders Olsson Elpool i Umeå AB  6 okt 2014 17:55:24 
                    /* FAKTORER OM INTE GRUNDKALKYL*/ 
                  */
                  kalkkostnad.FRITOTKOST = kalkkostnad.FRITOTKOST + KalkBerKost(1,kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL * kalknumsubtt.FRIPRIS,kalkkostnad.KVID) 
                  + KalkBerKost(2,kalknumtt.ANTAL * kalknumsubtt.FRIKOSTNAD,kalkkostnad.KVID) + kalknumsubtt.FRIAVRUND.
               END.   
            END.
            ELSE DO:
               IF kalknumtt.ARBKOD = "EGEN" THEN DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + (kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalknumsubtt.PRIS) 
                  + (kalknumtt.ANTAL * kalknumsubtt.KOSTNAD) + kalknumsubtt.AVRUND.
                  
                  kalkkostnad.FRITOTKOST = kalkkostnad.FRITOTKOST + (kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL * kalknumsubtt.FRIPRIS) 
                  + (kalknumtt.ANTAL * kalknumsubtt.FRIKOSTNAD) + kalknumsubtt.FRIAVRUND.
               END.
               ELSE DO:
                  kalkkostnad.TOTKOST = kalkkostnad.TOTKOST + (kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalkylprisertt.PRIS) 
                  + (kalknumtt.ANTAL * kalknumsubtt.KOSTNAD) + kalknumsubtt.AVRUND.
                  kalkkostnad.FRITOTKOST = kalkkostnad.FRITOTKOST + (kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL * kalknumsubtt.FRIPRIS) 
                  + (kalknumtt.ANTAL * kalknumsubtt.FRIKOSTNAD) + kalknumsubtt.FRIAVRUND.
               END.      
            END.    
            /*Anders Olsson Elpool i Umeå AB  6 okt 2014 17:53:13 
             /*INGA FAKTORER OM INTE GRUNDKALKYL*/ 
             kalkkostnad.FRITOTKOST = kalkkostnad.FRITOTKOST + (kalknumsubtt.FRITIMMAR * kalknumtt.ANTAL * kalknumsubtt.FRIPRIS) 
            + (kalknumtt.ANTAL * kalknumsubtt.FRIKOSTNAD) + kalknumsubtt.FRIAVRUND.
            */
            /*kalkberin*/
            
            totkostvar = totkostvar + kalkkostnad.TOTKOST.
           
           
            fritotkostvar = fritotkostvar + kalkkostnad.FRITOTKOST.
         
         FIND FIRST kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = kalknumtt.KLOGSUBID AND kalkylprisertt.KVID = 0 AND kalkylprisertt.SOKBENAMNING = "RÖRLIGKOSTNAD EA"  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kalkylprisertt THEN DO:
            FIND FIRST kalkylprisertt WHERE   kalkylprisertt.KVID = 0 AND kalkylprisertt.SOKBENAMNING = "RÖRLIGKOSTNAD EA" NO-LOCK NO-ERROR.
         END.
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
  METHOD PUBLIC DECIMAL KalkBerAntal(INPUT kalktimantal AS DECIMAL, kvidvar AS INTEGER):
     
     /*
     IF Guru.GlobalaVariabler:FranUppf = TRUE THEN RETURN kalktimantal.
     */
     
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" THEN DO:
        /*EXTRA KLOGSUBID*/
        FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KLOGSUBID = kalkylprisertt.KLOGSUBID AND kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
        IF NOT AVAILABLE kalkfaktorertt THEN DO:
           FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.BENAMNING = kalkylprisertt.BENAMNING NO-LOCK NO-ERROR.
           
        END.
        
        IF NOT AVAILABLE kalkfaktorertt THEN DO:
           /*maskiner med andra timmpriser än standrad*/
           IF kvidvar = 3 THEN DO:
              FKTORANTAL:
              FOR EACH kalkylpriserttbuf WHERE kalkylpriserttbuf.KLOGSUBID = kalkylprisertt.KLOGSUBID AND kalkylpriserttbuf.KVID = kvidvar AND 
              kalkylpriserttbuf.EGENKODUPP = TRUE:
                 FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KLOGSUBID = kalkylpriserttbuf.KLOGSUBID AND kalkfaktorertt.KPID = kalkylpriserttbuf.KPID NO-LOCK NO-ERROR.
                 IF AVAILABLE kalkfaktorertt THEN DO:
                    IF kalkfaktorertt.FAKTOR NE 1 THEN LEAVE FKTORANTAL.
                 END.   
              END.
              IF NOT AVAILABLE kalkfaktorertt THEN DO:
                 FKTOREANTAL:
                 FOR EACH kalkylpriserttbuf WHERE kalkylpriserttbuf.KVID = kvidvar AND 
                 kalkylpriserttbuf.EGENKODUPP = TRUE:
                    FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KLOGSUBID = kalkylpriserttbuf.KLOGSUBID AND kalkfaktorertt.KPID = kalkylpriserttbuf.KPID NO-LOCK NO-ERROR.
                    IF AVAILABLE kalkfaktorertt THEN DO:
                       IF kalkfaktorertt.FAKTOR NE 1 THEN LEAVE FKTOREANTAL.
                    END.   
                 END.
              END.   
           END.    
        END.
        
        IF AVAILABLE kalkfaktorertt THEN DO:
           RETURN kalktimantal * kalkfaktorertt.FAKTOR .             
        END.
     END.     
     RETURN  kalktimantal.
  END METHOD.   
  /*RÄKNA MED EGANAPRISER*/
  METHOD PUBLIC DECIMAL KalkBerKost(INPUT vad AS INTEGER, INPUT kalktimkost AS DECIMAL, INPUT kvidvar AS INTEGER ):
     /*
     IF Guru.GlobalaVariabler:FranUppf = TRUE THEN RETURN kalktimkost.
     */
     IF vad = 1 THEN DO:       
        IF kalknumtt.ARBKOD = "EGEN" THEN.
        ELSE IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisEgnaPriser") = "yes" THEN DO:               
            /*EXTRA KLOGSUBID*/
           FIND FIRST kalkegnaprisertt  WHERE  kalkegnaprisertt.KLOGSUBID = kalkylprisertt.KLOGSUBID  AND 
           kalkegnaprisertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
           IF NOT AVAILABLE  kalkegnaprisertt THEN DO:
              /*
              FIND FIRST kalkegnaprisertt  WHERE kalkegnaprisertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
              */
              FIND FIRST kalkegnaprisertt  WHERE kalkegnaprisertt.BENAMNING = kalkylprisertt.BENAMNING NO-LOCK NO-ERROR.
           END.
           IF AVAILABLE kalkegnaprisertt THEN DO:
              /*
              IF kalkylprisertt.EGENPRISUPP = TRUE THEN DO:
                 IF kalkylprisertt.PRIS = 0 THEN kalktimkost = kalkegnaprisertt.PRIS * kalknumsubtt.TIMMAR * kalknumtt.ANTAL.
                 ELSE kalktimkost = kalkegnaprisertt.PRIS * (kalktimkost / kalkylprisertt.PRIS).                   
              END.
              */
              IF kalkylprisertt.PRIS = 0 THEN kalktimkost = kalkegnaprisertt.PRIS * kalknumsubtt.TIMMAR * kalknumtt.ANTAL.
              ELSE kalktimkost = kalkegnaprisertt.PRIS * (kalktimkost / kalkylprisertt.PRIS).
           END.   
        END.   
     END.
     IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" THEN DO:
        FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KLOGSUBID = kalkylprisertt.KLOGSUBID AND kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
        IF NOT AVAILABLE kalkfaktorertt THEN DO:
           FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
        END.
        IF NOT AVAILABLE kalkfaktorertt THEN DO:
           /*maskiner med andra timmpriser än standrad*/
           IF kvidvar = 3 THEN DO:
              FKTORPRIS:
              FOR EACH kalkylpriserttbuf WHERE kalkylpriserttbuf.KLOGSUBID = kalkylprisertt.KLOGSUBID AND kalkylpriserttbuf.KVID = kvidvar AND 
              kalkylpriserttbuf.EGENKODUPP = TRUE:
                 FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KLOGSUBID = kalkylpriserttbuf.KLOGSUBID AND kalkfaktorertt.KPID = kalkylpriserttbuf.KPID NO-LOCK NO-ERROR.
                 IF AVAILABLE kalkfaktorertt THEN DO:
                    IF kalkfaktorertt.FAKTOR NE 1 THEN LEAVE FKTORPRIS.
                 END.   
              END.
              IF NOT AVAILABLE kalkfaktorertt THEN DO:
                 FKTORAPRIS:
                 FOR EACH kalkylpriserttbuf WHERE kalkylpriserttbuf.KVID = kvidvar AND 
                 kalkylpriserttbuf.EGENKODUPP = TRUE:
                    FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KLOGSUBID = kalkylpriserttbuf.KLOGSUBID AND kalkfaktorertt.KPID = kalkylpriserttbuf.KPID NO-LOCK NO-ERROR.
                    IF AVAILABLE kalkfaktorertt THEN DO:
                       IF kalkfaktorertt.FAKTOR NE 1 THEN LEAVE FKTORAPRIS.
                    END.   
                 END.
              END.   
           END.    
        END.
        IF AVAILABLE kalkfaktorertt THEN DO:
           RETURN kalktimkost * kalkfaktorertt.FAKTOR .             
        END.
     END.     
     RETURN  kalktimkost.
  END METHOD.  
  /*Alla koder i en kalkyl*/
  METHOD PUBLIC VOID AllaKoder():
     DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
     DEFINE VARIABLE whererqueryvar AS CHARACTER NO-UNDO.
     DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE query2var AS CHARACTER NO-UNDO.
     
     whererqueryvar = THIS-OBJECT:ControlShell:KalkControl:GridArbetsKoder:GuruFiltreraGet().
     
     queryvar = "FOR EACH " + THIS-OBJECT:ArbetskoderTTh:TABLE + " WHERE " + whererqueryvar. 
     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(ArbetskoderTTh,queryvar).
     qH:GET-FIRST().  
     DO WHILE qH:QUERY-OFF-END = FALSE:
        query2var = "FOR EACH " + THIS-OBJECT:LopposterTTh:TABLE + " WHERE ARBKOD = '" + THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE + "'".
        q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(LopposterTTh,query2var).
        q2H:GET-FIRST().  
        DO WHILE q2H:QUERY-OFF-END = FALSE:
           THIS-OBJECT:KoderTTh:BUFFER-CREATE().         
           THIS-OBJECT:KoderTTh:BUFFER-COPY(LopposterTTh).
           THIS-OBJECT:KoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:KoderTTh:RECID.
           q2H:GET-NEXT().
        END.
        qH:GET-NEXT().
     END.    
     THIS-OBJECT:CreateKoderRows(1).
     THIS-OBJECT:ControlShell:KalkControl:GridArbetsKoder:GuruFirstrow().    
     THIS-OBJECT:ControlShell:KalkControl:GridLopKoder:GuruFirstrow().
     THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:GuruReopen().
     THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:GuruFirstrow().
  END METHOD. 
  
  METHOD PUBLIC VOID PlusMinus():
     DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
     DEFINE VARIABLE whererqueryvar AS CHARACTER NO-UNDO.
     whererqueryvar = "".
     queryvar = "FOR EACH " + THIS-OBJECT:KoderTTh:TABLE + " WHERE " + whererqueryvar. 
     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(KoderTTh,queryvar).
     qH:GET-FIRST().  
     DO WHILE qH:QUERY-OFF-END = FALSE:
        THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = (THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE) * (- 1).
        THIS-OBJECT:UpdateKod().
        THIS-OBJECT:ControlShell:BerKalkKod().         
        qH:GET-NEXT().
     END.
     /*THIS-OBJECT:CalculateSum().*/    
     THIS-OBJECT:KalkSpara().   
     THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:GuruReopen().
     /*
     THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:GuruFirstrow().
     */
  END METHOD. 
  
   /*ny eGEN koder i copy från sparade egnakoder*/
  METHOD PUBLIC VOID  CreateAnvEgenKoderRows(antalvar AS DECIMAL):
     DEFINE VARIABLE rmat AS INTEGER NO-UNDO.
     DEFINE VARIABLE sparnum AS INTEGER NO-UNDO.
     DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
     rmat = THIS-OBJECT:ControlShell:CurrentMatris.
     sparnum = kalknumtt.NUM.
     IF rmat EQ -1 THEN rmat = 1.
     
     FIND LAST kalknumttbuf USE-INDEX NUM NO-ERROR.
     IF AVAILABLE kalknumttbuf THEN DO:
        kalknumtt.NUM = kalknumttbuf.NUM + 1.
     END.   
     ELSE kalknumtt.NUM = 1.

     ASSIGN
     kalknumtt.BERNUM = BerKalkNum
     kalknumtt.SID  =   BerSchaktSid
     kalknumtt.LOPNR = 0
     kalknumtt.KALKNR  = kalkhuvtt.KALKNR
     kalknumtt.OMRADE  = kalkhuvtt.OMRADE
     kalknumtt.TYPKALK = kalkhuvtt.TYPKALK. /* asdasd*/
     
     IF THIS-OBJECT:Kalknumanvegenbuffh:AVAILABLE THEN DO:
        kalknumtt.MARKNING = THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("MARKNING"):BUFFER-VALUE.
        kalknumtt.MARKSUB = THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("MARKSUB"):BUFFER-VALUE.
     END.
     
     IF kalknumtt.MATRIS  = 0 THEN kalknumtt.MATRIS  = rmat.
     ASSIGN    
     kalknumtt.ANTAL   = antalvar.
     FIND LAST kalknumttbuf WHERE kalknumttbuf.ARBKOD = "EGEN" USE-INDEX ARBKOD NO-ERROR.
     IF AVAILABLE kalknumttbuf THEN DO:
        kalknumtt.LOPNR = kalknumttbuf.LOPNR + 1.
     END.   
     ELSE kalknumtt.LOPNR = 1.
         
     THIS-OBJECT:Kalknumanvegenbuffh:FIND-FIRST("WHERE  ARBKOD =  'EGEN' AND LOPNR = " + STRING(THIS-OBJECT:LopposterTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE) ) NO-ERROR.

     queryvar =  "FOR EACH " + THIS-OBJECT:Kalknumanvegensubbuffh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE).
     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Kalknumanvegensubbuffh,queryvar).
     qH:GET-FIRST().
     DO WHILE qH:QUERY-OFF-END = FALSE:
        THIS-OBJECT:ValdaPriserTTh:BUFFER-CREATE().
        THIS-OBJECT:ValdaPriserTTh:BUFFER-COPY(THIS-OBJECT:Kalknumanvegensubbuffh).
        THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = kalknumtt.KALKNR.
        THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = kalknumtt.OMRADE.
        THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE =  kalknumtt.NUM.
        THIS-OBJECT:ValdaPriserTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = ValdaPriserTTh:RECID.
          
        qH:GET-NEXT().    
     END.
  END METHOD. 
  
    /*ny eGEN koder i från KLISTRA IN*/
  METHOD PUBLIC VOID  CreateEgenklister(INPUT frannum AS INTEGER):
     DEFINE VARIABLE rmat AS INTEGER NO-UNDO.
     DEFINE VARIABLE sparnum AS INTEGER NO-UNDO.
     DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
     DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
     DEFINE VARIABLE numsubvar AS INTEGER NO-UNDO.
     rmat = THIS-OBJECT:ControlShell:CurrentMatris.
     sparnum = kalknumtt.NUM.
     IF rmat EQ -1 THEN rmat = 1.
     
     FIND LAST kalknumttbuf USE-INDEX NUM NO-ERROR.
     IF AVAILABLE kalknumttbuf THEN DO:
        kalknumtt.NUM = kalknumttbuf.NUM + 1.
     END.   
     ELSE kalknumtt.NUM = 1.
    
     ASSIGN
     kalknumtt.BERNUM = BerKalkNum
     kalknumtt.SID  =   BerSchaktSid
     kalknumtt.LOPNR = 0
     kalknumtt.KALKNR  = kalkhuvtt.KALKNR
     kalknumtt.OMRADE  = kalkhuvtt.OMRADE
     kalknumtt.TYPKALK = kalkhuvtt.TYPKALK
     kalknumtt.MATRIS  = rmat.
     FIND FIRST kalknumsubtt WHERE kalknumsubtt.NUM = frannum NO-LOCK NO-ERROR.
     IF NOT AVAILABLE kalknumsubtt THEN DO:
        DELETE kalknumtt.
        RETURN.
     END.
     FIND LAST kalknumttbuf WHERE kalknumttbuf.ARBKOD = "EGEN" USE-INDEX ARBKOD NO-ERROR.
     IF AVAILABLE kalknumttbuf THEN DO:
        kalknumtt.LOPNR = kalknumttbuf.LOPNR + 1.
     END.   
     ELSE kalknumtt.LOPNR = 1.

     EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.  
     FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = frannum NO-LOCK:
        numvar = kalknumsubtt.NUM.
        numsubvar = kalknumsubtt.NUMSUBID.
        CREATE ekalknumsubtt.
        BUFFER-COPY kalknumsubtt EXCEPT NUM NUMSUBID TO ekalknumsubtt.
        ASSIGN
        ekalknumsubtt.NUM = numvar
        ekalknumsubtt.NUMSUBID = numsubvar.
     END.
     FOR EACH ekalknumsubtt WHERE NO-LOCK:
        FIND LAST kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
        IF AVAILABLE kalknumsubtt THEN ekalknumsubtt.NUMSUBID =  kalknumsubtt.NUMSUBID + 1.
        ELSE  ekalknumsubtt.NUMSUBID = 1.
        ASSIGN 
        ekalknumsubtt.KALKNR = kalknumtt.KALKNR
        ekalknumsubtt.OMRADE = kalknumtt.OMRADE
        ekalknumsubtt.NUM    = kalknumtt.NUM.
        
        numvar = ekalknumsubtt.NUM.
        numsubvar = ekalknumsubtt.NUMSUBID.
        
        CREATE kalknumsubtt.
        BUFFER-COPY ekalknumsubtt EXCEPT NUM NUMSUBID TO kalknumsubtt.
        ASSIGN
        kalknumsubtt.NUM = numvar
        kalknumsubtt.NUMSUBID = numsubvar.
        kalknumsubtt.TTRECID = RECID(kalknumsubtt).
        DELETE ekalknumsubtt.                      
     END.
     
  END METHOD.
 
   
  /*ny kalkpost från KLISTRA IN*/
  
  METHOD PUBLIC VOID CreateKlistraKoderRows(frannum AS INTEGER):
     DEFINE VARIABLE rmat AS INTEGER NO-UNDO.
     DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
     DEFINE VARIABLE numsubvar AS INTEGER NO-UNDO.
     rmat = THIS-OBJECT:ControlShell:CurrentMatris.
     IF rmat EQ -1 THEN rmat = 1.
     
     FOR EACH kalknumtt WHERE kalknumtt.MATRIS = 0 NO-LOCK:
        /*MESSAGE "ARBKOD" kalknumtt.ARBKOD "LOPNR" kalknumtt.LOPNR "ANT" kalknumtt.ANTAL
        VIEW-AS ALERT-BOX.*/
        IF kalknumtt.ARBKOD = "EGEN" THEN DO:
           THIS-OBJECT:CreateEgenklister(frannum).
           IF NOT AVAILABLE kalknumtt THEN DO:
              RETURN.
           END.
        END.   
        ELSE DO:
           FIND LAST kalknumttbuf USE-INDEX NUM NO-ERROR.
           FIND FIRST kalkylarbkodertt WHERE kalkylarbkodertt.ARBKOD = kalknumtt.ARBKOD NO-LOCK NO-ERROR.
           IF AVAILABLE kalknumttbuf THEN kalknumtt.NUM = kalknumttbuf.NUM + 1.
           ELSE kalknumtt.NUM = 1.
           ASSIGN
           kalknumtt.BERNUM = BerKalkNum
           kalknumtt.SID  =   BerSchaktSid
           kalknumtt.KALKNR  = kalkhuvtt.KALKNR
           kalknumtt.OMRADE  = kalkhuvtt.OMRADE
           kalknumtt.TYPKALK = kalkhuvtt.TYPKALK /* asdasd*/
           kalknumtt.MARKNING = kalkylarbkodertt.MARKNING
           kalknumtt.MARKSUB = kalkylarbkodertt.MARKSUB
           kalknumtt.MATRIS  = rmat
           nyberkalknum = kalknumtt.NUM.
           EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.  
           FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = frannum NO-LOCK:
              numvar = kalknumsubtt.NUM.
              numsubvar = kalknumsubtt.NUMSUBID.
              
              CREATE ekalknumsubtt.
              BUFFER-COPY kalknumsubtt EXCEPT NUM NUMSUBID TO ekalknumsubtt.
              ASSIGN
              ekalknumsubtt.NUM = numvar
              ekalknumsubtt.NUMSUBID = numsubvar.
             
           END.
 
           FOR EACH ekalknumsubtt WHERE NO-LOCK:
              FIND LAST kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
              IF AVAILABLE kalknumsubtt THEN ekalknumsubtt.NUMSUBID =  kalknumsubtt.NUMSUBID + 1.
              ELSE  ekalknumsubtt.NUMSUBID = 1.
              ASSIGN 
              ekalknumsubtt.KALKNR = kalknumtt.KALKNR
              ekalknumsubtt.OMRADE = kalknumtt.OMRADE
              ekalknumsubtt.NUM = kalknumtt.NUM.
              
              numvar = ekalknumsubtt.NUM.
              numsubvar = ekalknumsubtt.NUMSUBID.
              
              CREATE kalknumsubtt.
              BUFFER-COPY ekalknumsubtt EXCEPT NUM NUMSUBID  TO kalknumsubtt.
              kalknumsubtt.TTRECID = RECID(kalknumsubtt).
                    ASSIGN
              kalknumsubtt.NUM = numvar
              kalknumsubtt.NUMSUBID = numsubvar.
              DELETE ekalknumsubtt.        
                            
           END.
           /*MESSAGE "BERNUM" kalknumtt.BERNUM "FRANTILL" kalknumtt.frantill "SID" kalknumtt.sid "KLID" kalknumtt.MATRIS
           VIEW-AS ALERT-BOX.*/
           IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE AND kalknumtt.BERNUM > 0 THEN DO:
              kalknumtt.ID2 = "".
              kalknumtt.EXTRA1 = "".  
              kalknumtt.FRANTILL = "".            
              THIS-OBJECT:Bervallbuffh:FIND-FIRST("WHERE NUM  = " + STRING(kalknumtt.BERNUM) ,NO-LOCK) NO-ERROR.
              IF THIS-OBJECT:Bervallbuffh:AVAILABLE THEN DO:
                 kalknumtt.ID2 = THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE .
                 kalknumtt.EXTRA1 = THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE .
              END.
           END.   
           IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE AND kalknumtt.SID > 0 THEN DO:  
              kalknumtt.ID2 = "".
              kalknumtt.EXTRA1 = "".  
              kalknumtt.FRANTILL = "".
              THIS-OBJECT:HdSchaktStrackabuffh:FIND-FIRST("WHERE KLID  = " + STRING(kalknumtt.MATRIS) + " AND SID = " + STRING(kalknumtt.SID)) NO-ERROR.
              IF THIS-OBJECT:HdSchaktStrackabuffh:AVAILABLE THEN DO:
                 IF kalknumtt.SID NE 0 THEN kalknumtt.FRANTILL = THIS-OBJECT:HdSchaktStrackabuffh:BUFFER-FIELD("FRANTILL"):BUFFER-VALUE .
              END.
           END.
           
           
           EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.
        END.
        THIS-OBJECT:RaknaEnKod(FALSE).
                              
     END.
     
     
     THIS-OBJECT:KalkSpara().
     FIND FIRST kalkhuvtt NO-LOCK NO-ERROR.
    
  END METHOD.
  
  
  /*ny kalkpost*/
  METHOD PUBLIC VOID CreateKoderRows(antalvar AS DECIMAL):
     DEFINE VARIABLE rmat AS INTEGER NO-UNDO.
     
     rmat = THIS-OBJECT:ControlShell:CurrentMatris.
     IF rmat EQ -1 THEN rmat = 1.
     
     FOR EACH kalknumtt WHERE kalknumtt.MATRIS = 0 NO-LOCK:
        IF kalknumtt.ARBKOD = "EGEN" THEN DO:
           /*händer nog aldrig*/
           THIS-OBJECT:Kalknumanvegenbuffh:FIND-FIRST("WHERE ARBKOD = 'EGEN' AND LOPNR = " + STRING(kalknumtt.LOPNR)) NO-ERROR.
           THIS-OBJECT:CreateAnvEgenKoderRows(antalvar).
        END.   
        ELSE DO:
           FIND LAST kalknumttbuf USE-INDEX NUM NO-ERROR.
           FIND FIRST kalkylarbkodertt WHERE kalkylarbkodertt.ARBKOD = kalknumtt.ARBKOD NO-LOCK NO-ERROR.
           IF AVAILABLE kalknumttbuf THEN kalknumtt.NUM = kalknumttbuf.NUM + 1.
           ELSE kalknumtt.NUM = 1.
           ASSIGN
           kalknumtt.BERNUM = BerKalkNum
           kalknumtt.SID  =   BerSchaktSid
           kalknumtt.KALKNR  = kalkhuvtt.KALKNR
           kalknumtt.OMRADE  = kalkhuvtt.OMRADE
           kalknumtt.TYPKALK = kalkhuvtt.TYPKALK /* asdasd*/
           kalknumtt.MARKNING = kalkylarbkodertt.MARKNING
           kalknumtt.MARKSUB = kalkylarbkodertt.MARKSUB
           kalknumtt.MATRIS  = rmat
           kalknumtt.ANTAL   = antalvar.
           nyberkalknum = kalknumtt.NUM.
           /* 
           IF valdberkonst > 0 THEN kalknumtt.BERNUM = valdberkonst.
           IF valdsidnr > 0 THEN kalknumtt.SID = valdsidnr.
           */
       
           IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE AND kalknumtt.BERNUM > 0 THEN DO:
              THIS-OBJECT:Bervallbuffh:FIND-FIRST("WHERE NUM  = " + STRING(kalknumtt.BERNUM) ,NO-LOCK) NO-ERROR.
              IF THIS-OBJECT:Bervallbuffh:AVAILABLE THEN DO:
                 kalknumtt.ID2 = THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE .
                 kalknumtt.EXTRA1 = THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE .
              END.
           END.
           THIS-OBJECT:skapanumsub().
        END.
     END.
     THIS-OBJECT:KalkSpara().
     FIND FIRST kalkhuvtt NO-LOCK NO-ERROR.
    
  END METHOD.
  METHOD PUBLIC VOID skapanumsub():
     DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
     DEFINE VARIABLE numsubvar AS INTEGER NO-UNDO.
     EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.  
     RUN skapanumsub_UI IN AppServerHandle (INPUT kalknumtt.KLOGSUBID,INPUT kalknumtt.ARBKOD,INPUT kalknumtt.LOPNR,OUTPUT TABLE ekalknumsubtt).
     FOR EACH ekalknumsubtt WHERE NO-LOCK:
        FIND LAST kalknumsubtt WHERE kalknumsubtt.NUM = kalknumtt.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
       
        
        
        IF AVAILABLE kalknumsubtt THEN ekalknumsubtt.NUMSUBID =  kalknumsubtt.NUMSUBID + 1.
        ELSE  ekalknumsubtt.NUMSUBID = 1.
        ekalknumsubtt.NUM = kalknumtt.NUM.
        
        numvar = ekalknumsubtt.NUM.
        numsubvar = ekalknumsubtt.NUMSUBID.
        ASSIGN 
        ekalknumsubtt.KALKNR = kalknumtt.KALKNR
        ekalknumsubtt.OMRADE = kalknumtt.OMRADE
        ekalknumsubtt.NUM = kalknumtt.NUM.
        CREATE kalknumsubtt.
        BUFFER-COPY ekalknumsubtt EXCEPT NUM NUMSUBID TO kalknumsubtt.
        ASSIGN
        kalknumsubtt.TTRECID = RECID(kalknumsubtt)
        kalknumsubtt.NUM = numvar
        kalknumsubtt.NUMSUBID = numsubvar.
        DELETE ekalknumsubtt.    
                          
     END.
     EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.
     THIS-OBJECT:RaknaEnKod(FALSE).
  END METHOD.
   /*ny koder i kalkyl viA IMPORT/KOPIERA KONVERTERA*/
  METHOD PUBLIC VOID CreateKodRow(INPUT kodttR AS RECID, INPUT spara AS LOGICAL):
     FIND FIRST kalknumtt WHERE RECID(kalknumtt) =  kodttR NO-LOCK NO-ERROR.
     FIND LAST kalknumttbuf USE-INDEX NUM NO-ERROR.
     IF AVAILABLE kalknumttbuf THEN kalknumtt.NUM = kalknumttbuf.NUM + 1.
     ELSE kalknumtt.NUM = 1.
     ASSIGN
     kalknumtt.TTRECID    =  kodttR
     kalknumtt.KALKNR     = kalkhuvtt.KALKNR
     kalknumtt.OMRADE     = kalkhuvtt.OMRADE
     kalknumtt.TYPKALK    = kalkhuvtt.TYPKALK.
     IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN.
     ELSE DO:
        kalknumtt.BERNUM = BerKalkNum.
        kalknumtt.SID  =   BerSchaktSid.
     END.      
     THIS-OBJECT:skapanumsub().
      
     IF spara = TRUE THEN DO:                
        THIS-OBJECT:KalkSpara().
     END.
   
  END METHOD. 
  METHOD PUBLIC VOID ImportSpara():
     THIS-OBJECT:KalkSpara().
    
     FIND FIRST kalkhuvtt NO-LOCK NO-ERROR. 
  END METHOD. 
  /*NY ANVEGENKOD*/
  METHOD PUBLIC ROWID CreateAnvEgenKod():
     DEFINE VARIABLE returner AS ROWID NO-UNDO.
     DEFINE VARIABLE numvarsub AS INTEGER NO-UNDO.
     DEFINE VARIABLE lopnrvar AS INTEGER NO-UNDO.
     DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
     DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
     THIS-OBJECT:Kalknumanvegenbuffh:FIND-LAST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "' AND ARBKOD = 'EGEN' USE-INDEX ANVEGEN") NO-ERROR.
     IF THIS-OBJECT:Kalknumanvegenbuffh:AVAILABLE THEN DO:
        lopnrvar = THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE + 1. 
        
     END.
     ELSE lopnrvar = 1.
     THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-CREATE().
     THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-COPY(THIS-OBJECT:KoderTTh).
     THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE = Guru.Konstanter:globanv.
     THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 1.
     THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = lopnrvar.
     THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE = lopnrvar.
     THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = Kalknumanvegenbuffh:RECID.
     returner = Kalknumanvegenbuffh:ROWID.  
     THIS-OBJECT:LopposterTTh:BUFFER-CREATE().
     THIS-OBJECT:LopposterTTh:BUFFER-COPY(Kalknumanvegenbuffh).
     THIS-OBJECT:LopposterTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:LopposterTTh:RECID.
     THIS-OBJECT:ControlShell:KalkControl:GridLopKoder:GuruReopen().
     
     queryvar =  "FOR EACH " + THIS-OBJECT:ValdaPriserTTh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:KoderTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE).
     qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ValdaPriserTTh,queryvar).
     qH:GET-FIRST().
     DO WHILE qH:QUERY-OFF-END = FALSE:
        numvarsub = numvarsub + 1.
        THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-CREATE().
        THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-COPY(THIS-OBJECT:ValdaPriserTTh).
        THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE = Guru.Konstanter:globanv.
        
        THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE =  THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE.
        THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("NUMSUBID"):BUFFER-VALUE = numvarsub.
        THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = Kalknumanvegensubbuffh:RECID.
        
        qH:GET-NEXT().    
     END.
     THIS-OBJECT:RaknaAnvEgen().
     
     RETURN returner.
  END METHOD.
  METHOD PUBLIC ROWID CreateEgenKodCopyAnvEgen():
     DEFINE VARIABLE returner AS ROWID NO-UNDO.
     DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
     DEFINE VARIABLE lopnrvar AS INTEGER NO-UNDO.
     FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID = kalkhuvtt.KLOGID NO-LOCK NO-ERROR.
     THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE  ARBKOD =  'EGEN' AND LOPNR = " + STRING(THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE)) NO-ERROR.
     IF THIS-OBJECT:LopposterTTh:AVAILABLE THEN DO:
        
        THIS-OBJECT:KoderTTh:BUFFER-CREATE().
        THIS-OBJECT:KoderTTh:BUFFER-COPY(LopposterTTh).
        THIS-OBJECT:KoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE = kalkylkatalogtt.HKLOGSUBID.
       
        IF  THIS-OBJECT:ControlShell:CurrentMatris = -1 THEN THIS-OBJECT:KoderTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE =  THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE. 
        /*
        THIS-OBJECT:KoderTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE =  THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
        THIS-OBJECT:KoderTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE =  THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
        */
        THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = 1.
        THIS-OBJECT:KoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:KoderTTh:RECID.
        THIS-OBJECT:CreateAnvEgenKoderRows(THIS-OBJECT:KoderTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE).
        returner = THIS-OBJECT:KoderTTh:ROWID.
        THIS-OBJECT:RaknaEnKod(FALSE).
     END.
     THIS-OBJECT:KalkSpara().
 
     
     RETURN returner.      
  END METHOD.
  METHOD PUBLIC VOID CreateTomEgen():
     DEFINE VARIABLE rid AS ROWID NO-UNDO.
     rid = THIS-OBJECT:CreateEgenKod().
     FIND FIRST kalknumtt WHERE ROWID(kalknumtt) = rid  NO-LOCK NO-ERROR.
     ASSIGN
     kalknumtt.ANTAL   = 0
     kalknumtt.BENAMNING = "Inga Kalylkoder".
    
  END METHOD.
  /*ny egen kod*/
   METHOD PUBLIC ROWID CreateEgenKod():
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      DEFINE VARIABLE rmat AS INTEGER NO-UNDO.
      DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
      DEFINE VARIABLE lopnrvar AS INTEGER NO-UNDO.
      rmat = THIS-OBJECT:ControlShell:CurrentMatris.
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
      kalknumtt.BERNUM = BerKalkNum
      kalknumtt.SID  =   BerSchaktSid
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
/*
      IF valdberkonst > 0 THEN kalknumtt.BERNUM = valdberkonst.
      IF valdsidnr > 0 THEN kalknumtt.SID = valdsidnr.
  */
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
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:TEXT).
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
            pristypvar = THIS-OBJECT:PrisTyp(INPUT kalknumsubtt.KPID,kalknumtt.KLOGSUBID).
            IF pristypvar = "ENTREP" THEN ejmed = TRUE.    
         END.   
         IF ejmed = FALSE THEN DO:
            IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
               pristypvarny = THIS-OBJECT:PrisTyp(INPUT kalknumsubtt.KPID,kalknumtt.KLOGSUBID).
               FOR EACH kalknumsubttbuf WHERE kalknumsubttbuf.NUM = kalknumtt.NUM AND kalknumsubttbuf.NUM > 0:
                  pristypvar = THIS-OBJECT:PrisTyp(INPUT kalknumsubttbuf.KPID,kalknumtt.KLOGSUBID).
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
      
      mat = THIS-OBJECT:ControlShell:CurrentMatris.
     
      THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
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
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:TEXT).
      FIND FIRST kalknumtt WHERE kalknumtt.NUM = hittnum NO-LOCK NO-ERROR.
      THIS-OBJECT:ControlShell:KalkEgnaControl:GridValdaUnderKoder:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
      kostvar = kalknumsubtt.TIMMAR * kalknumtt.ANTAL * kalknumsubtt.PRIS + kalknumtt.ANTAL * kalknumsubtt.KOSTNAD.
      ASSIGN 
      kalknumtt.TOTKOST = kalknumtt.TOTKOST - kostvar
      kalknumtt.FRITOTKOST = kalknumtt.FRITOTKOST - kostvar. 
     
   END METHOD.
    /*TAR BORT anvEGEN KOD*/
   METHOD PUBLIC VOID RemoveAnvEgenKod():
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      /*Anders Olsson Elpool i Umeå AB  26 jan 2016 10:12:03 
      sparad egen  ska inte finnas i vallistan tillsammans med övriga koder  
      */
      /*
      THIS-OBJECT:LopposterTTh:FIND-FIRST("WHERE  ARBKOD =  'EGEN' AND LOPNR = " + STRING(THIS-OBJECT:Kalknumanvegenbuffh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE)) NO-ERROR.
      IF THIS-OBJECT:LopposterTTh:AVAILABLE THEN DO:
         THIS-OBJECT:LopposterTTh:BUFFER-DELETE().
         THIS-OBJECT:ControlShell:KalkControl:GridLopKoder:GuruReopen().
      END.
        */  
       
    
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:KalkEgnaControl:GridAnvEgen:ActiveRow:Cells["NUM"]:Text).
           
      queryvar =  "FOR EACH " + THIS-OBJECT:Kalknumanvegensubbuffh:TABLE + " WHERE NUM = " + STRING(hittnum).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Kalknumanvegensubbuffh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:Kalknumanvegensubbuffh:BUFFER-DELETE.
         qH:GET-NEXT().
      END.
      THIS-OBJECT:KalkAnvEgenSpar().
   END METHOD.
   /*TAR BORT EGEN KOD*/
   METHOD PUBLIC VOID RemoveEgenKod():
      DEFINE VARIABLE hittnum AS INTEGER NO-UNDO.
      hittnum =  INTEGER(THIS-OBJECT:ControlShell:KalkEgnaControl:GridEgnaKoder:ActiveRow:Cells["NUM"]:Text).
      FOR EACH kalknumsubtt WHERE kalknumsubtt.NUM = hittnum NO-LOCK:
         DELETE kalknumsubtt.
      END.
      THIS-OBJECT:KalkSpara().
   END METHOD.
   METHOD PUBLIC VOID BeraknaMarkerade():
      DEFINE VARIABLE res       AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE delsumma AS INTEGER NO-UNDO FORMAT ">>>>,>>>,999".
      DEFINE VARIABLE desummavar AS DECIMAL NO-UNDO.
      DEFINE VARIABLE mat AS INTEGER NO-UNDO.
     
      mat = THIS-OBJECT:ControlShell:CurrentMatris.
              
      {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:Selected:Rows}
          THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:Guruegenskap:ttBufferHandle:FIND-FIRST(villkor) NO-ERROR.
          
          IF THIS-OBJECT:ControlShell:RibbonToolFriKalk:Checked = FALSE THEN desummavar = desummavar + THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:Guruegenskap:ttBufferHandle:BUFFER-FIELD("TOTKOST"):BUFFER-VALUE.  
          ELSE desummavar = desummavar + THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:Guruegenskap:ttBufferHandle:BUFFER-FIELD("FRITOTKOST"):BUFFER-VALUE. 
      END. /*foreach end*/    
      delsumma = desummavar.
      
      res = System.Windows.Forms.MessageBox:Show(STRING(delsumma) + " " + THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(245)  , "" , System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
   END METHOD.
   
   
   /*TAR BORT NUMSUBBAR VID buttonBackMark_Click DVS KODER*/
   METHOD PUBLIC VOID RemoveKoderSelected():
      DEFINE VARIABLE mat AS INTEGER NO-UNDO.
     
      mat = THIS-OBJECT:ControlShell:CurrentMatris.
              
      {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:ControlShell:KalkControl:GridKalkylKoder:Selected:Rows}
         THIS-OBJECT:RemoveKoderRow(villkor).
      END. /*foreach end*/    
      IF oObject = ? THEN DO:                                                   
         THIS-OBJECT:RemoveKoderRowROWID(KoderTTh:ROWID).
      END.   
   END METHOD.
   /*TAR BORT NUMSUBBAR VID buttonBackMarkPriser_Click DVS VIA EGNAKODER*/
   METHOD PUBLIC VOID RemoveValdaSelectedEgna ():                    
      EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.
      {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:ControlShell:KalkEgnaControl:GridValdaUnderKoder:Selected:Rows}                                         
         THIS-OBJECT:RemoveValdaRowEgna (villkor).
      END. /*foreach end*/
   END METHOD.
  /* buttonBackMarkPriser_Click buttonBackAllaPriser_Click*/
   METHOD PUBLIC VOID RemoveValdaAllaEgna ():    
      EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR.  
       {WALLMAN\foreachultra.i System.Object oObject in THIS-OBJECT:ControlShell:KalkEgnaControl:GridValdaUnderKoder:Rows}                                         
         THIS-OBJECT:RemoveValdaRowEgna (villkor).
      END. /*foreach end*/     
   END METHOD.
   /*SKAPA MRTL*/
   METHOD PUBLIC VOID CreatevalMaterial(INPUT kalknr AS INTEGER,INPUT omr AS CHARACTER, berantal AS  DECIMAL):
      DEFINE VARIABLE cm AS INTEGER NO-UNDO.
      cm = THIS-OBJECT:ControlShell:CurrentMatris.
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
         /*
         kalktmtrlTT.SUMMA = kalktmtrlTT.NPRIS * kalktmtrlTT.BERKVANT.
         */
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
                     FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KLOGSUBID = kalkylprisertt.KLOGSUBID AND kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE kalkfaktorertt THEN DO:
                        FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
                     END.
                     IF AVAILABLE kalkfaktorertt THEN DO:
                        RETURN (ACCUM TOTAL BY kalktmtrlTT.BREAKDUMMY kalktmtrlTT.SUMMA) * kalkfaktorertt.FAKTOR .             
                     END.
                  END.                                     
               END.
               ELSE RETURN ACCUM TOTAL BY kalktmtrlTT.BREAKDUMMY kalktmtrlTT.SUMMA.   
            END.                   
         END.
         RETURN 0.
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
                     FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KLOGSUBID = kalkylprisertt.KLOGSUBID AND kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE kalkfaktorertt THEN DO:
                        FIND FIRST kalkfaktorertt WHERE kalkfaktorertt.KPID = kalkylprisertt.KPID NO-LOCK NO-ERROR.
                     END.
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
     THIS-OBJECT:kodantaltempTTh = TEMP-TABLE kodantaltemp:HANDLE:DEFAULT-BUFFER-HANDLE.
     THIS-OBJECT:kalkkostnadTTh = TEMP-TABLE kalkkostnad:HANDLE:DEFAULT-BUFFER-HANDLE.     
     THIS-OBJECT:KalkRubrikTTh = TEMP-TABLE KalkRubrikTT:HANDLE:DEFAULT-BUFFER-HANDLE.
     
     
   END METHOD.
   /*GER SOKBENAMNING FÖR NUMSUB*/
   METHOD PUBLIC CHARACTER PrisTyp(kpidvar AS INTEGER,klogidvar AS INTEGER):
      
      
      FIND FIRST kalkylprisertt WHERE kalkylprisertt.KPID = kpidvar AND kalkylprisertt.KLOGSUBID = klogidvar NO-LOCK NO-ERROR.
      
      RETURN kalkylprisertt.SOKBENAMNING. 
   END METHOD.
   /*TILL AVTALSKALKYLEN*/
   METHOD PUBLIC VOID AvtalKalkAo(INPUT exelkommando AS CHARACTER, INPUT avtalid AS INTEGER, INPUT matrisvar AS LOGICAL):
      FIND FIRST Avtalskalktt  WHERE Avtalskalktt.ID = avtalid NO-LOCK NO-ERROR.       
      EMPTY TEMP-TABLE AvtalKodertt NO-ERROR. 
      IF matrisvar = TRUE THEN DO:
         FOR EACH kalknumtt BREAK BY kalknumtt.MATRIS BY kalknumtt.ARBKOD BY kalknumtt.LOPNR:
            ACCUMULATE kalknumtt.ANTAL (TOTAL BY kalknumtt.MATRIS BY kalknumtt.ARBKOD BY kalknumtt.LOPNR).
            ACCUMULATE kalknumtt.TOTKOST (TOTAL BY kalknumtt.ARBKOD BY kalknumtt.LOPNR). 
            IF LAST-OF(kalknumtt.LOPNR) THEN DO:
               CREATE AvtalKodertt.      
               BUFFER-COPY kalknumtt TO AvtalKodertt.               
               AvtalKodertt.ANTAL = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.ANTAL).    
               AvtalKodertt.TOTKOST = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.TOTKOST).                  
             END.     
         END.
      END.
      ELSE DO:
         FOR EACH kalknumtt BREAK BY kalknumtt.ARBKOD BY kalknumtt.LOPNR:
            ACCUMULATE kalknumtt.ANTAL (TOTAL BY kalknumtt.ARBKOD BY kalknumtt.LOPNR).
            ACCUMULATE kalknumtt.TOTKOST (TOTAL BY kalknumtt.ARBKOD BY kalknumtt.LOPNR). 
            IF LAST-OF(kalknumtt.LOPNR) THEN DO:
               CREATE AvtalKodertt.      
               BUFFER-COPY kalknumtt TO AvtalKodertt.
               AvtalKodertt.ANTAL = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.ANTAL).
               AvtalKodertt.TOTKOST = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.TOTKOST).                      
             END.     
         END.
      END.   
      FOR EACH AvtalKodertt,
      EACH kalknumtt WHERE kalknumtt.ARBKOD = AvtalKodertt.ARBKOD AND kalknumtt.LOPNR = AvtalKodertt.LOPNR:
         IF kalknumtt.ANMARKNING NE "" THEN AvtalKodertt.ANMARKNING = AvtalKodertt.ANMARKNING + " " + kalknumtt.ANMARKNING.
      END.
       
         
   END METHOD.
   METHOD PUBLIC VOID SumNstn():
     
      FOR EACH BerAvtalKodertt :
         THIS-OBJECT:Bervallbuffh:FIND-FIRST("WHERE NUM =" + STRING(BerAvtalKodertt.BERNUM)  + " USE-INDEX ORD",NO-LOCK) NO-ERROR.
         ASSIGN BerAvtalKodertt.NGRUPP = THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("NGRUPP"):BUFFER-VALUE.
      END.
      /*OUTPUT TO c:\aa\BerAvtalKodertt.txt.
      FOR EACH BerAvtalKodertt USE-INDEX bernum  NO-LOCK:
         PUT UNFORMATTED BerAvtalKodertt.ngrupp "ng" BerAvtalKodertt.BERnum " bn " BerAvtalKodertt.arbkod " " BerAvtalKodertt.lopnr " " BerAvtalKodertt.antal BerAvtalKodertt.BERID  " berid "  BerAvtalKodertt.NUM " num "    BerAvtalKodertt.MATRIS " m " SKIP.
      END.
       
      OUTPUT CLOSE.*/
      
      FOR EACH BerAvtalKodertt WHERE BerAvtalKodertt.NGRUPP > 0 BREAK BY BerAvtalKodertt.NGRUPP BY BerAvtalKodertt.ARBKOD BY BerAvtalKodertt.LOPNR:
         ACCUMULATE BerAvtalKodertt.ANTAL (TOTAL BY BerAvtalKodertt.NGRUPP BY BerAvtalKodertt.ARBKOD BY BerAvtalKodertt.LOPNR).
         ACCUMULATE BerAvtalKodertt.TOTKOST (TOTAL BY BerAvtalKodertt.NGRUPP BY BerAvtalKodertt.ARBKOD BY BerAvtalKodertt.LOPNR). 
         IF LAST-OF(BerAvtalKodertt.LOPNR) THEN DO:
            CREATE BerAvtalKoderSumtt.      
            BUFFER-COPY BerAvtalKodertt TO BerAvtalKoderSumtt.
            BerAvtalKoderSumtt.ANTAL = (ACCUM TOTAL BY  BerAvtalKodertt.LOPNR BerAvtalKodertt.ANTAL).
            BerAvtalKoderSumtt.TOTKOST = (ACCUM TOTAL BY BerAvtalKodertt.LOPNR BerAvtalKodertt.TOTKOST).                      
          END.
      END.
      /*OUTPUT TO c:\aa\BerAvtalKodersumtt.txt.
      FOR EACH BerAvtalKodersumtt USE-INDEX bernum  NO-LOCK:
         PUT UNFORMATTED BerAvtalKodersumtt.ngrupp  " " BerAvtalKodersumtt.BERnum " " BerAvtalKodersumtt.arbkod " " BerAvtalKodersumtt.lopnr " " BerAvtalKodersumtt.antal SKIP.
      END.
      OUTPUT CLOSE.*/
   END METHOD.   
   METHOD PUBLIC VOID AvtalKalkAoBerSch(INPUT exelkommando AS CHARACTER, INPUT avtalid AS INTEGER, INPUT matrisvar AS LOGICAL):
      DEFINE VARIABLE avtschakt  AS INTEGER NO-UNDO.
      
      RUN schaktberidlhmt_UI IN AppServerHandle (INPUT BernrVar,INPUT BerOmrVar, OUTPUT avtschakt) .
            
      EMPTY TEMP-TABLE BerAvtalKodertt NO-ERROR. 
      /*kalkylkoder från beredningen  BERNUM>0 */
      FOR EACH kalknumtt WHERE kalknumtt.BERNUM > 0 BREAK BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR:
         ACCUMULATE kalknumtt.ANTAL (TOTAL BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR).
         ACCUMULATE kalknumtt.TOTKOST (TOTAL BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR). 
         IF LAST-OF(kalknumtt.LOPNR) THEN DO:
            CREATE BerAvtalKodertt.      
            BUFFER-COPY kalknumtt TO BerAvtalKodertt.
            BerAvtalKodertt.ANTAL = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.ANTAL).
            BerAvtalKodertt.TOTKOST = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.TOTKOST).                      
          END.     
      END.
      /*OUTPUT TO c:\aa\1BerAvtalKoderttavtal.txt.
      FOR EACH BerAvtalKodertt USE-INDEX bernum  NO-LOCK:
         PUT UNFORMATTED BerAvtalKodertt.ngrupp "ng" BerAvtalKodertt.BERnum " bn " BerAvtalKodertt.arbkod " " BerAvtalKodertt.lopnr " " BerAvtalKodertt.antal BerAvtalKodertt.BERID  " berid "  BerAvtalKodertt.NUM " num "    BerAvtalKodertt.MATRIS " m " SKIP.
      END.
       
      OUTPUT CLOSE.*/
      /*Import av konstruktionerna från beredning i ett schakt
       hdschakprottemp.FRANTILL = kon_display.ID2 + "    " + kon_display.EXTRA1.
       hdschakprottemp.BERNUM = kon_display.NUM.
       hdschakprottemp.BERID = INTEGER(kon_display.ID2). 
       Manuellt skapade sträckor får BERNUM = 0 BERID= 0
       Lena 20200224*/
      DEBUGGER:SET-BREAK().
      
      IF ExelKommando MATCHES "*Vattenfall*" OR ExelKommando MATCHES "*C4*" THEN DO:
         /*beredningskalkyl ska ut på samma sätt som i kalkyl.MÅSTE SUMMERAS PÅ ARBKOD ANTAL LENA 20201016*/
        /* FOR EACH kalknumtt WHERE /*kalknumtt.SID = avtschakt AND kalknumtt.BERNUM > 0*/ BREAK BY kalknumtt.SID BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR:
            ACCUMULATE kalknumtt.ANTAL (TOTAL BY kalknumtt.SID BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR).
            ACCUMULATE kalknumtt.TOTKOST (TOTAL BY kalknumtt.SID BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR). */
         FOR EACH kalknumtt WHERE BREAK BY kalknumtt.ARBKOD BY kalknumtt.LOPNR:
            ACCUMULATE kalknumtt.ANTAL (TOTAL  BY kalknumtt.ARBKOD BY kalknumtt.LOPNR).
            ACCUMULATE kalknumtt.TOTKOST (TOTAL BY kalknumtt.ARBKOD BY kalknumtt.LOPNR).
            
            IF LAST-OF(kalknumtt.LOPNR) THEN DO:
               CREATE AvtalKodertt.      
               BUFFER-COPY kalknumtt TO AvtalKodertt.
              /* AvtalKodertt.BERNUM = kalknumtt.BERNUM.*/
               AvtalKodertt.ANTAL = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.ANTAL).
               AvtalKodertt.TOTKOST = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.TOTKOST).                      
             END.     
         END. 
      END.
      /* dessa kalkylkoder är redan hämtade på  BERNUM ovan Lena 20211007
         felet har inte märks när det skrivs per konstruktion, då skriver programmet bara över, men när det summeras blir schaktkoderna dubbla
      ELSE IF avtschakt > 0 THEN DO:                  
         FOR EACH kalknumtt WHERE kalknumtt.SID = avtschakt AND kalknumtt.BERNUM > 0 BREAK BY kalknumtt.SID BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR:
            ACCUMULATE kalknumtt.ANTAL (TOTAL BY kalknumtt.SID BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR). 
            ACCUMULATE kalknumtt.TOTKOST (TOTAL BY kalknumtt.SID BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR).
            IF LAST-OF(kalknumtt.LOPNR) THEN DO:
               CREATE BerAvtalKodertt.      
               BUFFER-COPY kalknumtt TO BerAvtalKodertt.
               BerAvtalKodertt.BERNUM = kalknumtt.BERNUM.
               BerAvtalKodertt.ANTAL = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.ANTAL).
               BerAvtalKodertt.TOTKOST = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.TOTKOST).                                              
             END.     
         END.                  
      END.*/               
      ELSE IF Guru.Konstanter:GForetag = "AKEAUTBI" THEN DO:
         FOR EACH kalknumtt WHERE /*kalknumtt.SID = avtschakt AND kalknumtt.BERNUM > 0*/ BREAK BY kalknumtt.SID BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR:
            ACCUMULATE kalknumtt.ANTAL (TOTAL BY kalknumtt.SID BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR).
            ACCUMULATE kalknumtt.TOTKOST (TOTAL BY kalknumtt.SID BY kalknumtt.BERNUM BY kalknumtt.ARBKOD BY kalknumtt.LOPNR). 
            IF LAST-OF(kalknumtt.LOPNR) THEN DO:
               CREATE AvtalKodertt.      
               BUFFER-COPY kalknumtt TO AvtalKodertt.
               AvtalKodertt.BERNUM = kalknumtt.BERNUM.
               AvtalKodertt.ANTAL = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.ANTAL).
               AvtalKodertt.TOTKOST = (ACCUM TOTAL BY kalknumtt.LOPNR kalknumtt.TOTKOST).                      
             END.     
         END. 
      END.            
      
      
   END METHOD.
   
   /*START FÖR EONES AVTAL*/
   METHOD PUBLIC VOID EonesAvtal(INPUT-OUTPUT avtalvar AS CHARACTER , INPUT-OUTPUT sterad AS INTEGER,INPUT-OUTPUT slerad  AS INTEGER,INPUT-OUTPUT flik1 AS INTEGER,INPUT-OUTPUT flik2 AS INTEGER,
                                  INPUT-OUTPUT frist AS INTEGER,INPUT-OUTPUT frisl AS INTEGER,INPUT-OUTPUT oversatt137 AS LOGICAL , INPUT-OUTPUT ExelKommando AS CHARACTER,
                                  INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER ,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,
                                  INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):          
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      RUN esmallhmt_UI IN AppServerHandle (INPUT avtalvar, OUTPUT sterad,OUTPUT slerad,OUTPUT flik1,OUTPUT flik2 , OUTPUT frist , OUTPUT frisl , OUTPUT oversatt137 , INPUT ExelKommando).
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
      efrihj = 1.
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EFRI":
         ASSIGN
         AvtalKodertt.BENAMNING = AvtalKodertt.BENAMNING + " ( " + AvtalKodertt.ARBKOD + STRING(AvtalKodertt.LOPNR) + " )"
         AvtalKodertt.ARBKOD = "FRI"
         AvtalKodertt.LOPNR = efrihj.
         efrihj = efrihj + 1.
         /*ASSIGN
         AvtalKodertt.ARBKOD = "FRI"
         AvtalKodertt.LOPNR = AvtalKodertt.LOPNR - 97.*/
         /* skall ligga    FRI03-FRI09*/
         /* lena 20120216
         AvtalKodertt.LOPNR = AvtalKodertt.LOPNR - 60.      */
      END.
      egenhj = efrihj.
      /*numrera om EGEN-koderna så att de får plats i mallen  benämning = + (EGEN10)*/   
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN" BY AvtalKodertt.LOPNR:
         IF AvtalKodertt.TOTKOST = 0 THEN DELETE AvtalKodertt.
         ELSE DO:
            AvtalKodertt.BENAMNING = AvtalKodertt.BENAMNING + " ( " + AvtalKodertt.ARBKOD + STRING(AvtalKodertt.LOPNR) + " )".
            AvtalKodertt.ARBKOD = "FRI".
            AvtalKodertt.LOPNR = egenhj .
                        
            egenhj = egenhj + 1.
         END.   
      END.
      
           
   END METHOD.
   METHOD PUBLIC VOID VattenAvtal(INPUT-OUTPUT sterad AS INTEGER,INPUT-OUTPUT slerad AS INTEGER,OUTPUT sterad2 AS INTEGER,INPUT-OUTPUT slerad2 AS INTEGER,INPUT-OUTPUT sterad3 AS INTEGER,
                                INPUT-OUTPUT slerad3 AS INTEGER,INPUT-OUTPUT flik1 AS INTEGER,INPUT-OUTPUT flik2 AS INTEGER,
                                INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER ,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,
                                INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):
                                   
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      RUN vattmallhmt_UI IN AppServerHandle (OUTPUT sterad,OUTPUT slerad,OUTPUT sterad2,OUTPUT slerad2,OUTPUT flik1,OUTPUT flik2 ). 
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN":
          AvtalKodertt.ARBKOD = "FRI".
      END.     
   END METHOD.
   METHOD PUBLIC VOID VattenEonAvtal(INPUT-OUTPUT sterad AS INTEGER,INPUT-OUTPUT slerad AS INTEGER,OUTPUT sterad2 AS INTEGER,INPUT-OUTPUT slerad2 AS INTEGER,INPUT-OUTPUT sterad3 AS INTEGER,
                                INPUT-OUTPUT slerad3 AS INTEGER,INPUT-OUTPUT flik1 AS INTEGER,INPUT-OUTPUT flik2 AS INTEGER,
                                INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER ,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,
                                INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):
                                   
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      RUN vattmalleonhmt_UI IN AppServerHandle (OUTPUT sterad,OUTPUT slerad,OUTPUT sterad2,OUTPUT slerad2,OUTPUT flik1,OUTPUT flik2 ). 
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN":
          AvtalKodertt.ARBKOD = "FRI".
      END.     
   END METHOD.
   
   METHOD PUBLIC VOID VattenEllevioAvtal(INPUT-OUTPUT sterad AS INTEGER,INPUT-OUTPUT slerad AS INTEGER,OUTPUT sterad2 AS INTEGER,INPUT-OUTPUT slerad2 AS INTEGER,INPUT-OUTPUT sterad3 AS INTEGER,
                                INPUT-OUTPUT slerad3 AS INTEGER,INPUT-OUTPUT flik1 AS INTEGER,INPUT-OUTPUT flik2 AS INTEGER,
                                INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER ,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,
                                INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):
                                   
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      RUN vattmallelleviohmt_UI IN AppServerHandle (OUTPUT sterad,OUTPUT slerad,OUTPUT sterad2,OUTPUT slerad2,OUTPUT flik1,OUTPUT flik2 ). 
      /*FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN":
          AvtalKodertt.ARBKOD = "FRI".
      END.*/     
   END METHOD.
   
   
   METHOD PUBLIC VOID AkeaAvtal(INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):          
      
     
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
     
      
   END METHOD.
   
   METHOD PUBLIC VOID InfraAvtal(INPUT-OUTPUT avtalvar AS CHARACTER ,INPUT-OUTPUT sterad1 AS INTEGER,INPUT-OUTPUT slerad1 AS INTEGER,OUTPUT sterad2 AS INTEGER,INPUT-OUTPUT slerad2 AS INTEGER,INPUT-OUTPUT sterad3 AS INTEGER,
                                INPUT-OUTPUT slerad3 AS INTEGER,INPUT-OUTPUT frist AS INTEGER,INPUT-OUTPUT flik1 AS INTEGER,INPUT-OUTPUT flik2 AS INTEGER,
                                INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER ,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,
                                INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER, INPUT-OUTPUT oversatt137 AS LOGICAL , INPUT-OUTPUT ExelKommando AS CHARACTER):          
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      
      RUN inframallhmt_UI IN AppServerHandle (INPUT avtalvar, OUTPUT sterad1,OUTPUT slerad1,OUTPUT sterad2,OUTPUT slerad2,OUTPUT sterad3,OUTPUT slerad3,OUTPUT frist,OUTPUT flik1,OUTPUT flik2, OUTPUT oversatt137, 
      INPUT ExelKommando, INPUT Guru.GlobalaVariabler:KalkylSomStartiBer  ).

      /*FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN":
         AvtalKodertt.ARBKOD = "FRI".
      END.*/
      
      egenhj = 1.
      IF Guru.Konstanter:globforetag = "fors" THEN egenhj = 21.
      
      
      /*numrera om EGEN-koderna så att de får plats i mallen  benämning = + (EGEN10)*/   
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN" BY AvtalKodertt.LOPNR:
         IF AvtalKodertt.TOTKOST = 0 THEN DELETE AvtalKodertt.
         ELSE DO:
            AvtalKodertt.BENAMNING = AvtalKodertt.BENAMNING + " ( " + AvtalKodertt.ARBKOD + STRING(AvtalKodertt.LOPNR) + " )".
            AvtalKodertt.ARBKOD = "FRI".
            AvtalKodertt.LOPNR = egenhj .
                        
            egenhj = egenhj + 1.
         END.   
      END.
      
      /*one efri-koder*/
     
      FOR EACH BerAvtalKodertt WHERE BerAvtalKodertt.ARBKOD = "EFRI" AND BerAvtalKodertt.BERNUM > 0 :
         IF BerAvtalKodertt.TOTKOST = 0 THEN DELETE BerAvtalKodertt.
         ELSE DO:
            ASSIGN
            BerAvtalKodertt.BENAMNING = BerAvtalKodertt.BENAMNING + " ( " + BerAvtalKodertt.ARBKOD + STRING(BerAvtalKodertt.LOPNR) + " )".
            BerAvtalKodertt.ARBKOD = "FRI".
            IF BerAvtalKodertt.LOPNR  = 100 THEN BerAvtalKodertt.LOPNR = 1.
            ELSE IF BerAvtalKodertt.LOPNR  = 101 THEN BerAvtalKodertt.LOPNR = 2.
            ELSE IF BerAvtalKodertt.LOPNR  = 102 THEN BerAvtalKodertt.LOPNR = 3.
            ELSE IF BerAvtalKodertt.LOPNR  = 103 THEN BerAvtalKodertt.LOPNR = 4.
            ELSE IF BerAvtalKodertt.LOPNR  = 104 THEN BerAvtalKodertt.LOPNR = 5.
            ELSE IF BerAvtalKodertt.LOPNR  = 105 THEN BerAvtalKodertt.LOPNR = 6.
            ELSE IF BerAvtalKodertt.LOPNR  = 106 THEN BerAvtalKodertt.LOPNR = 7. 
         END.            
               
      END.
      FIND FIRST BerAvtalKodertt WHERE BerAvtalKodertt.arbkod = "EFRI" NO-LOCK NO-ERROR.  
      IF AVAILABLE BerAvtalKodertt THEN DO:
         /*RÄKNA BARA UPPOM DET FINNS EFRI*/
         egenhj = egenhj + 7.
      END.
      /*numrera om EGEN-koderna så att de får plats i mallen  benämning = + (EGEN10)*/   
      FOR EACH BerAvtalKodertt WHERE BerAvtalKodertt.ARBKOD = "EGEN" AND BerAvtalKodertt.BERNUM > 0  BY BerAvtalKodertt.LOPNR:
         IF BerAvtalKodertt.TOTKOST = 0 THEN DELETE BerAvtalKodertt.
         ELSE DO:
            BerAvtalKodertt.BENAMNING = BerAvtalKodertt.BENAMNING + " ( " + BerAvtalKodertt.ARBKOD + STRING(BerAvtalKodertt.LOPNR) + " )".
            BerAvtalKodertt.ARBKOD = "FRI".
            BerAvtalKodertt.LOPNR = egenhj .
                        
            egenhj = egenhj + 1.
         END.   
      END.
      FOR EACH SchAvtalKodertt WHERE SchAvtalKodertt.ARBKOD = "EGEN" AND SchAvtalKodertt.SID > 0 BY SchAvtalKodertt.LOPNR:
         IF SchAvtalKodertt.TOTKOST = 0 THEN DELETE SchAvtalKodertt.
         ELSE DO:
            SchAvtalKodertt.BENAMNING = SchAvtalKodertt.BENAMNING + " ( " + SchAvtalKodertt.ARBKOD + STRING(SchAvtalKodertt.LOPNR) + " )".
            SchAvtalKodertt.ARBKOD = "FRI".
            SchAvtalKodertt.LOPNR = egenhj .
                        
            egenhj = egenhj + 1.
         END.   
      END.
      
      
   END METHOD.
   
   METHOD PUBLIC VOID C4ElAvtal(INPUT-OUTPUT avtalvar AS CHARACTER ,INPUT-OUTPUT sterad1 AS INTEGER,INPUT-OUTPUT slerad1 AS INTEGER,OUTPUT sterad2 AS INTEGER,INPUT-OUTPUT slerad2 AS INTEGER,INPUT-OUTPUT sterad3 AS INTEGER,
                                INPUT-OUTPUT slerad3 AS INTEGER,INPUT-OUTPUT frist AS INTEGER,INPUT-OUTPUT flik1 AS INTEGER,INPUT-OUTPUT flik2 AS INTEGER,
                                INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER ,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,
                                INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER, INPUT-OUTPUT oversatt137 AS LOGICAL , INPUT-OUTPUT ExelKommando AS CHARACTER):          
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      
      RUN C4mallhmt_UI IN AppServerHandle (INPUT avtalvar, OUTPUT sterad1,OUTPUT slerad1,OUTPUT sterad2,OUTPUT slerad2,OUTPUT sterad3,OUTPUT slerad3,OUTPUT frist,OUTPUT flik1,OUTPUT flik2, OUTPUT oversatt137, 
      INPUT ExelKommando, INPUT Guru.GlobalaVariabler:KalkylSomStartiBer  ).

      /*1-3 REDAN UPPTAGNA*/
      egenhj = 4.
      /*IF Guru.Konstanter:globforetag = "fors" THEN egenhj = 21.*/
      
      
      /*numrera om EGEN-koderna så att de får plats i mallen  benämning = + (EGEN10)*/   
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN" BY AvtalKodertt.LOPNR:
         IF AvtalKodertt.TOTKOST = 0 THEN DELETE AvtalKodertt.
         ELSE DO:
            AvtalKodertt.BENAMNING = AvtalKodertt.BENAMNING + " ( " + AvtalKodertt.ARBKOD + STRING(AvtalKodertt.LOPNR) + " )".
            AvtalKodertt.ARBKOD = "FRI".
            AvtalKodertt.LOPNR = egenhj .
                        
            egenhj = egenhj + 1.
         END.   
      END.
      
      /*one efri-koder*/
      
      /*FOR EACH BerAvtalKodertt WHERE BerAvtalKodertt.ARBKOD = "EFRI" AND BerAvtalKodertt.BERNUM > 0 :
         IF BerAvtalKodertt.TOTKOST = 0 THEN DELETE BerAvtalKodertt.
         ELSE DO:
            ASSIGN
            BerAvtalKodertt.BENAMNING = BerAvtalKodertt.BENAMNING + " ( " + BerAvtalKodertt.ARBKOD + STRING(BerAvtalKodertt.LOPNR) + " )".
            BerAvtalKodertt.ARBKOD = "FRI".
            IF BerAvtalKodertt.LOPNR  = 100 THEN BerAvtalKodertt.LOPNR = 1.
            ELSE IF BerAvtalKodertt.LOPNR  = 101 THEN BerAvtalKodertt.LOPNR = 2.
            ELSE IF BerAvtalKodertt.LOPNR  = 102 THEN BerAvtalKodertt.LOPNR = 3.
            ELSE IF BerAvtalKodertt.LOPNR  = 103 THEN BerAvtalKodertt.LOPNR = 4.
            ELSE IF BerAvtalKodertt.LOPNR  = 104 THEN BerAvtalKodertt.LOPNR = 5.
            ELSE IF BerAvtalKodertt.LOPNR  = 105 THEN BerAvtalKodertt.LOPNR = 6.
            ELSE IF BerAvtalKodertt.LOPNR  = 106 THEN BerAvtalKodertt.LOPNR = 7. 
         END.            
               
      END.
      egenhj = egenhj + 7.*/
      
      /*numrera om EGEN-koderna så att de får plats i mallen  benämning = + (EGEN10)*/   
      FOR EACH BerAvtalKodertt WHERE BerAvtalKodertt.ARBKOD = "EGEN" AND BerAvtalKodertt.BERNUM > 0  BY BerAvtalKodertt.LOPNR:
         IF BerAvtalKodertt.TOTKOST = 0 THEN DELETE BerAvtalKodertt.
         ELSE DO:
            BerAvtalKodertt.BENAMNING = BerAvtalKodertt.BENAMNING + " ( " + BerAvtalKodertt.ARBKOD + STRING(BerAvtalKodertt.LOPNR) + " )".
            BerAvtalKodertt.ARBKOD = "FRI".
            BerAvtalKodertt.LOPNR = egenhj .
                        
            egenhj = egenhj + 1.
         END.   
      END.
      FOR EACH SchAvtalKodertt WHERE SchAvtalKodertt.ARBKOD = "EGEN" AND SchAvtalKodertt.SID > 0 BY SchAvtalKodertt.LOPNR:
         IF SchAvtalKodertt.TOTKOST = 0 THEN DELETE SchAvtalKodertt.
         ELSE DO:
            SchAvtalKodertt.BENAMNING = SchAvtalKodertt.BENAMNING + " ( " + SchAvtalKodertt.ARBKOD + STRING(SchAvtalKodertt.LOPNR) + " )".
            SchAvtalKodertt.ARBKOD = "FRI".
            SchAvtalKodertt.LOPNR = egenhj .
                        
            egenhj = egenhj + 1.
         END.   
      END.
      
      
   END METHOD.
   
   
   METHOD PUBLIC VOID KringAvtal(INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):          
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      
   END METHOD.
   METHOD PUBLIC VOID LappAvtal(INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):          
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      
   END METHOD.
   METHOD PUBLIC VOID SwecoAvtal(INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):          
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      
   END METHOD.
   METHOD PUBLIC VOID LimoAvtal(INPUT-OUTPUT beredarvar AS CHARACTER,INPUT-OUTPUT utfardatvar AS CHARACTER,INPUT-OUTPUT refvar AS CHARACTER,INPUT-OUTPUT ortnamn AS CHARACTER,INPUT-OUTPUT kontakt AS CHARACTER,INPUT-OUTPUT arbannsv AS CHARACTER):          
      RUN AvtalKalkAo_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).
      FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN":
         AvtalKodertt.ARBKOD = "FRI".
      END.
   END METHOD.
   
   METHOD PUBLIC VOID ReferensUE(OUTPUT btimUE AS DECIMAL, OUTPUT mtimUE AS DECIMAL, OUTPUT kabtimUE AS DECIMAL,OUTPUT ledtimUE AS DECIMAL,OUTPUT mtrlUE AS DECIMAL,OUTPUT ovrUE AS DECIMAL, OUTPUT uenr AS INTEGER ):                 
      RUN RefUE_UI IN AppServerHandle (INPUT  Guru.GlobalaVariabler:plusaonr,INPUT Guru.GlobalaVariabler:plusdnr,OUTPUT btimUE,OUTPUT mtimUE,OUTPUT kabtimUE,OUTPUT ledtimUE,OUTPUT mtrlUE,OUTPUT ovrUE, OUTPUT uenr).
      
      /*FOR EACH AvtalKodertt WHERE AvtalKodertt.ARBKOD = "EGEN":
         AvtalKodertt.ARBKOD = "FRI".
      END.*/
   END METHOD.
   
   METHOD PUBLIC VOID BeredningKop():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE nyber AS LOGICAL NO-UNDO.
      DEFINE VARIABLE orgomr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE omrnamn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE orgbernr AS INTEGER NO-UNDO.
      DEFINE VARIABLE kopbernr AS INTEGER NO-UNDO.
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN RUN BeredningMtrl IN AppServerExtraHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar, OUTPUT nyber, OUTPUT orgomr, OUTPUT orgbernr ).
      IF nyber = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(233)," ",
         System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).  
         IF rrr:ToString() EQ "Yes" THEN DO:
            RUN ejvalaonr_UI IN AppServerExtraHandle (INPUT orgomr,INPUT "",OUTPUT omrnamn,OUTPUT kopbernr).
            RUN reg_UI IN AppServerExtraHandle (INPUT orgomr,INPUT orgbernr,INPUT orgomr,INPUT kopbernr).
            
            IF Guru.Konstanter:appcon  THEN DO: 
                                         
               RUN KOPIBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT orgbernr, INPUT orgomr, INPUT STRING(kopbernr), INPUT orgomr).
            END.
            ELSE DO:
               RUN KOPIBER.P
               (INPUT orgbernr, INPUT orgomr, INPUT STRING(kopbernr), INPUT orgomr).
            END.
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(234)  + Guru.Konstanter:gomrl + " :" + orgomr + "Bernr :" + STRING(kopbernr) ," ",
            System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Information).    
         END.    
         ELSE DO:              
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(235) ," ",
            System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Warning).    
            RETURN.
         END.
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(232) ," ",
         System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Warning).            
      END.
   END METHOD.
   METHOD PUBLIC VOID BeredningMtrl():          
      DEFINE VARIABLE nyber AS LOGICAL NO-UNDO.
      DEFINE VARIABLE beromr AS CHARACTER NO-UNDO.
      DEFINE VARIABLE bernr AS INTEGER NO-UNDO.
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN RUN BeredningMtrl IN AppServerExtraHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar, OUTPUT nyber, OUTPUT beromr, OUTPUT bernr ).
      RUN STARTKALKBER.P (INPUT nyber,INPUT ControlShell:Omradevar,INPUT ControlShell:KalkNrvar,INPUT beromr,INPUT bernr,INPUT  STRING(THIS-OBJECT:ControlShell:KalkHuvudControl:HuvudBenamning:GuruText:Text)). 
   END METHOD.
   
   METHOD PUBLIC DECIMAL BerMtrlPris():
      DEFINE VARIABLE bertotpris AS DECIMAL NO-UNDO.
      RUN BerMtrlPris_UI IN AppServerHandle (INPUT ControlShell:KalkNrvar, OUTPUT bertotpris).
      RETURN bertotpris.
   END METHOD.
   METHOD PUBLIC VOID KalkyAonrBerFinns():
      DEFINE VARIABLE finns AS LOGICAL NO-UNDO.
      IF Guru.Konstanter:globforetag = "SNAT" THEN RETURN.
      RUN KalkyAonrBerFinns_UI IN AppServerHandle (INPUT ControlShell:KalkNrvar, OUTPUT finns).
      IF finns THEN DO:
         IF listitemVisaPristyp > - 1 THEN.
         ELSE listitemVisaPristyp = THIS-OBJECT:ControlShell:RibbonToolVisPriser:ValueList:ValueListItems:Add(NEW Infragistics.Win.ValueListItem("Mtrl. Pris från Beredning")).
      END. 
      
   END METHOD.
   /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
        DVS KALKYLEN HAR EN BEREDNING FÖR OPTO
        */
   METHOD PUBLIC LOGICAL BeredningMtrlfinns():   
      DEFINE VARIABLE nyber AS LOGICAL NO-UNDO.
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN DO:
         IF AVAILABLE kalkhuvtt THEN DO:
            RUN BeredningMtrlfinns IN AppServerExtraHandle (INPUT kalkhuvtt.KALKNR,INPUT kalkhuvtt.OMRADE, OUTPUT nyber).
         END.
         ELSE IF ControlShell NE ? THEN DO:
            RUN BeredningMtrlfinns IN AppServerExtraHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar, OUTPUT nyber).
         END.      
      END.     
      RETURN nyber.     
   END METHOD.
   
   /*Anders Olsson Elpool i Umeå AB  30 jun 2015 09:42:05 
        Bara för Sundsvall! Får mtrkostnadsrad i Kalkylen! man gör en beredning från kalkylen 
        DVS KALKYLEN HAR EN BEREDNING FÖR OPTO
        */
   METHOD PUBLIC VOID BeredningMtrlHmt(OUTPUT totbermtrl AS DECIMAL): 
      IF VALID-HANDLE(THIS-OBJECT:AppServerExtraHandle) THEN DO:
         IF AVAILABLE kalkhuvtt THEN DO:
            RUN BeredningMtrlHmt IN AppServerExtraHandle (INPUT kalkhuvtt.KALKNR,INPUT kalkhuvtt.OMRADE,OUTPUT totbermtrl,OUTPUT TABLE berkalkmtrltt).
         END.
         ELSE DO:
            RUN BeredningMtrlHmt IN AppServerExtraHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar,OUTPUT totbermtrl,OUTPUT TABLE berkalkmtrltt).
         END.       
      END.   
   END METHOD.
   /*Anders Olsson Elpool i Umeå AB  25 apr 2016 09:04:37 
   mtrl från bredning vid uppföljning om valet är satt. 
   */
   METHOD PUBLIC VOID BeredningMtrlHmtUppf():
      RUN BeredningMtrlHmtUppf IN AppServerExtraHandle (INPUT kalkhuvtt.KALKNR,INPUT kalkhuvtt.OMRADE,OUTPUT TABLE kalktmtrlTT).
   END METHOD.
   
   
   
   
   METHOD PUBLIC VOID Utsmetkost():
      
      ASSIGN 
      utsmetkost = 0 
      projledkost = 0
      monttottimusmet = 0
      totkostusmet = 0.      
      FIND FIRST kalkkostnad  WHERE kalkkostnad.FRITOTKOST > 0 NO-LOCK NO-ERROR.
      IF AVAILABLE kalkkostnad THEN DO:
         FOR EACH kalkkostnad WHERE kalkkostnad.ARBKOD = "Å":                           
            projledkost = projledkost + kalkkostnad.FRITOTKOST.                              
         END.
         FOR EACH kalkkostnad WHERE kalkkostnad.ARBKOD = "Ä":                           
            utsmetkost = utsmetkost + kalkkostnad.FRITOTKOST.                              
         END.
         FOR EACH kalkkostnad  :
            IF kalkkostnad.ARBKOD = "Å" THEN.
            ELSE IF kalkkostnad.ARBKOD = "Ä" THEN.
            ELSE totkostusmet = totkostusmet + kalkkostnad.FRITOTKOST.
         END.         
      END.
      ELSE DO:
         FOR EACH kalkkostnad WHERE kalkkostnad.ARBKOD = "Å":                           
            projledkost = projledkost + kalkkostnad.TOTKOST.                              
         END.
         FOR EACH kalkkostnad WHERE kalkkostnad.ARBKOD = "Ä":                     
            utsmetkost = utsmetkost + kalkkostnad.TOTKOST.               
         END.
         FOR EACH kalkkostnad  :
            IF kalkkostnad.ARBKOD = "Å" THEN.
            ELSE IF kalkkostnad.ARBKOD = "Ä" THEN.
            ELSE totkostusmet = totkostusmet + kalkkostnad.TOTKOST.
         END.                  
      END.          
      FIND FIRST kalkantal  WHERE kalkantal.FRISUMMA > 0 NO-LOCK NO-ERROR.
      IF AVAILABLE kalkantal THEN DO:     
         FOR EACH kalkantal WHERE kalkantal.KVID = 2:
            IF kalkantal.ARBKOD = "Å" THEN.                           
            ELSE monttottimusmet = monttottimusmet + kalkantal.FRISUMMA.
         END. 
      END.
      ELSE DO:
         FOR EACH kalkantal WHERE kalkantal.KVID = 2:
            IF kalkantal.ARBKOD = "Å" THEN.                            
            ELSE monttottimusmet = monttottimusmet + kalkantal.SUMMA.
         END.
      END.
      
      FOR EACH kalkkostnad,
      EACH kalknumtt WHERE kalknumtt.NUM = kalkkostnad.NUM:        
         ASSIGN   
         kalkkostnad.ST =  kalknumtt.ANTAL.        
         kalkkostnad.STTOTKOST = kalkkostnad.TOTKOST /  kalknumtt.ANTAL.
         kalkkostnad.STFRITOTKOST = kalkkostnad.FRITOTKOST /  kalknumtt.ANTAL.
      END.
      
      ASSIGN
      allapris = 0
      allaprisfri = 0.
      FOR EACH kalkkostnad WHERE kalkkostnad.KVID = 2:
         FIND FIRST kalkantal  WHERE  kalkantal.NUM = kalkkostnad.NUM AND kalkantal.KVID = 2 NO-LOCK NO-ERROR.
         IF AVAILABLE kalkantal  THEN DO:
            kalkkostnad.STFRIPROJKOST = ( kalkantal.FRISUMMA * projledkost / monttottimusmet )  / kalkkostnad.ST .
            kalkkostnad.STPROJKOST = ( kalkantal.SUMMA * projledkost / monttottimusmet )  / kalkkostnad.ST .
            IF kalkkostnad.ARBKOD = "Ä" THEN DO:
                utsmetkost = utsmetkost + ( kalkkostnad.STFRIPROJKOST * kalkkostnad.ST ).
            END.   
            ELSE IF kalkkostnad.ARBKOD = "Å" OR kalkkostnad.ARBKOD = "Ö" THEN.
            ELSE DO:
               totkostusmet = totkostusmet + ( kalkkostnad.STFRIPROJKOST * kalkkostnad.ST ).
               /*allapris = allapris + kalkkostnad.TOTKOST + ( kalkkostnad.STPROJKOST * kalkkostnad.ST ).
               allaprisfri = allaprisfri + kalkkostnad.FRITOTKOST + ( kalkkostnad.STFRIPROJKOST * kalkkostnad.ST ).*/
            END.                 
         END.   
      END.  
       
      IF THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisFaktorer") = "yes" OR        
      THIS-OBJECT:Root:DatabaseManager:Global:GetActualValues("Kalkyl",Guru.GlobalaVariabler:GuruDefaultAnv,"VisEgnaPriser") = "yes" THEN DO:         
         FOR EACH kalkkostnad :
            /*FIND FIRST kalknumtt  WHERE kalknumtt.NUM = kalkkostnad.NUM NO-LOCK NO-ERROR.*/
            kalkkostnad.STUTSMET = ( kalkkostnad.TOTKOST /  totkostusmet ) * utsmetkost / kalkkostnad.ST.
            kalkkostnad.STFRIUTSMET = ( kalkkostnad.FRITOTKOST /  totkostusmet ) * utsmetkost / kalkkostnad.ST. 
         END.
      END.  
      FOR EACH kalkkostnad NO-LOCK:
         kalkkostnad.STTOTKOST = kalkkostnad.STTOTKOST + kalkkostnad.STPROJKOST + kalkkostnad.STUTSMET.
         kalkkostnad.STFRITOTKOST = kalkkostnad.STFRITOTKOST + kalkkostnad.STFRIPROJKOST + kalkkostnad.STFRIUTSMET.
       
      END.       
        
   END METHOD.
   
   
   METHOD PUBLIC VOID KonverteraKalkyl(INPUT nykattyp AS INTEGER,INPUT nyomrvar AS CHARACTER,OUTPUT felmed AS LOGICAL): 
      DEFINE VARIABLE nykalknr AS INTEGER NO-UNDO.
      RUN KonverteraKalkyl_UI IN AppServerHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar,INPUT nykattyp,INPUT nyomrvar,OUTPUT nykalknr,
      OUTPUT TABLE kalknumtt,OUTPUT TABLE tidut).
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
      THIS-OBJECT:KalkylimportTTh = TEMP-TABLE KalkylimportTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ControlShell:LoadKalkyl(INPUT nykalknr, INPUT nyomrvar, INPUT FALSE). 
      THIS-OBJECT:KopKonvBenamning().
   END METHOD.
   
   METHOD PUBLIC VOID KopKonvBenamning():
      THIS-OBJECT:ControlShell:KalkHuvudControl:HuvudBenamning:GuruText:Text = THIS-OBJECT:ControlShell:KalkKopKonvControl:HuvudBenamning:GuruText:Text.
      kalkhuvtt.BENAMNING = THIS-OBJECT:ControlShell:KalkKopKonvControl:HuvudBenamning:GuruText:Text.
   END METHOD.
   
   METHOD PUBLIC VOID CreateImport():
      /*körs inte*/
      THIS-OBJECT:KalkylimportTTh:FIND-FIRST("WHERE MATRIS = " + STRING(kalknumtt.MATRIS) + " AND ARBKOD = '" + kalknumtt.ARBKOD + "' AND LOPNR = " + STRING(kalknumtt.LOPNR) ,NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:KalkylimportTTh:AVAILABLE THEN .
      ELSE DO:
         THIS-OBJECT:KalkylimportTTh:BUFFER-CREATE().
         THIS-OBJECT:KalkylimportTTh:BUFFER-COPY(BUFFER kalknumtt:HANDLE,"ANTAL").
         THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:KalkylimportTTh:RECID.
      END.   
      THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE + kalknumtt.ANTAL.
     
   END METHOD.
   METHOD PUBLIC VOID Kopierakalkyl(INPUT nykatidvar AS INTEGER,INPUT nyomrvar AS CHARACTER,OUTPUT felmed AS LOGICAL): 
      DEFINE VARIABLE nykalknr AS INTEGER NO-UNDO.
      DEFINE VARIABLE dResult AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE frikkop AS LOGICAL NO-UNDO.
      IF kalkhuvtt.KLOGID =  nykatidvar THEN DO: 
         dResult = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(231),"",System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF dResult:ToString() EQ "YES" THEN DO:
            frikkop = TRUE.
            RUN kopieraFRI_UI IN AppServerHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar,INPUT nykatidvar,INPUT nyomrvar,OUTPUT nykalknr,OUTPUT TABLE tidut).
            FIND FIRST tidut WHERE NO-LOCK NO-ERROR.
            IF AVAILABLE tidut THEN DO:
               THIS-OBJECT:ControlShell:FelMeddCheck().
               RETURN. 
            END.   
            THIS-OBJECT:anvkalkyl(INPUT nykalknr,INPUT nyomrvar).
         END.   
      END.
      IF frikkop = FALSE THEN DO: 
         RUN kopierakalkyl_UI IN AppServerHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar,INPUT nykatidvar,INPUT nyomrvar,OUTPUT nykalknr,OUTPUT TABLE tidut).
         FIND FIRST tidut WHERE NO-LOCK NO-ERROR.
         IF AVAILABLE tidut THEN DO:
            THIS-OBJECT:ControlShell:FelMeddCheck().
            RETURN. 
         END.   
         THIS-OBJECT:anvkalkyl(INPUT nykalknr,INPUT nyomrvar).  
         
         FOR EACH kalknumtt WHERE NO-LOCK:
            IF kalknumtt.ARBKOD = "EGEN" THEN.
            ELSE DO:
               CREATE KalkylimportTT.
               BUFFER-COPY kalknumtt TO KalkylimportTT.
               KalkylimportTT.TTRECID = RECID(KalkylimportTT).
            END.   
         END.
         RUN oversatt_UI IN AppServerHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar,INPUT nykatidvar,INPUT-OUTPUT TABLE KalkylimportTT).
         THIS-OBJECT:KalkylimportTTh = TEMP-TABLE KalkylimportTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      END.   
      THIS-OBJECT:ControlShell:LoadKalkyl(INPUT nykalknr, INPUT nyomrvar, INPUT FALSE).
      THIS-OBJECT:KopKonvBenamning().         
   END METHOD.
   METHOD PUBLIC VOID FlyttaKalkyl(INPUT nykatidvar AS INTEGER,INPUT nyomrvar AS CHARACTER,OUTPUT felmed AS LOGICAL): 
      DEFINE VARIABLE nykalknr AS INTEGER NO-UNDO.
      RUN FlyttaKalkylNykat_UI IN AppServerHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar,INPUT nykatidvar,OUTPUT TABLE tidut).
      FIND FIRST tidut WHERE NO-LOCK NO-ERROR.
      IF AVAILABLE tidut THEN DO:
         THIS-OBJECT:ControlShell:FelMeddCheck().
         RETURN. 
      END.   
      THIS-OBJECT:anvkalkyl(INPUT nykalknr,INPUT nyomrvar).  
      FOR EACH kalknumtt WHERE NO-LOCK:
         IF kalknumtt.ARBKOD = "EGEN" THEN.
         ELSE DO:
            CREATE KalkylimportTT.
            BUFFER-COPY kalknumtt TO KalkylimportTT.
            KalkylimportTT.TTRECID = RECID(KalkylimportTT).
         END.   
      END.
      
      THIS-OBJECT:KalkylimportTTh = TEMP-TABLE KalkylimportTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ControlShell:LoadKalkyl(INPUT ControlShell:KalkNrvar, INPUT ControlShell:Omradevar, INPUT FALSE).         
   END METHOD.
   METHOD PUBLIC VOID SlaIhopKalkyler(INPUT slaihoph AS HANDLE): 
      DEFINE VARIABLE slaihoptth AS HANDLE NO-UNDO.
      CREATE TEMP-TABLE slaihoptth.
      slaihoptth:CREATE-LIKE(slaihoph).
      slaihoptth:TEMP-TABLE-PREPARE("ttkopia"). 
      slaihoptth:COPY-TEMP-TABLE(slaihoph). 
      RUN SlaIhopKalkyler IN AppServerHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar,INPUT TABLE-HANDLE slaihoptth,OUTPUT TABLE KalkylimportTT).
      THIS-OBJECT:anvkalkyl(INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar).  
     
      THIS-OBJECT:ControlShell:LoadKalkyl(INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar, INPUT FALSE).         
   END METHOD.
   
   METHOD PUBLIC VOID JmfKalkyler(INPUT jmfcath AS HANDLE): 
      DEFINE VARIABLE jmftth AS HANDLE NO-UNDO.
      CREATE TEMP-TABLE jmftth.
      jmftth:CREATE-LIKE(jmfcath).
      jmftth:TEMP-TABLE-PREPARE("ttkopia"). 
      jmftth:COPY-TEMP-TABLE(jmfcath). 
      RUN JmfKalkyler IN AppServerHandle (INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar,INPUT TABLE-HANDLE jmftth,OUTPUT TABLE KalkylimportTT).
       THIS-OBJECT:anvkalkyl(INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar).  
     /*egen kod*/
      THIS-OBJECT:ControlShell:LoadKalkyl(INPUT ControlShell:KalkNrvar,INPUT ControlShell:Omradevar, INPUT FALSE).         
   END METHOD.
   METHOD PUBLIC VOID InkopKoll(numvar AS INTEGER):
      IF THIS-OBJECT:ControlShell:RibbonToolKoderAndMtrlmBer:CHECKED = FALSE  THEN RETURN. 
      FIND FIRST sumbernum_mtrl WHERE sumbernum_mtrl.NUM = numvar      
      NO-LOCK NO-ERROR.
      IF AVAILABLE sumbernum_mtrl THEN DO:
         THIS-OBJECT:ControlShell:KalkControl:GridBerMtrl:GuruReadOnly(TRUE,"ANTAL").
         THIS-OBJECT:ControlShell:KalkControl:AddButton:Visible  = FALSE.
         THIS-OBJECT:ControlShell:KalkControl:RemoveButton:Visible = FALSE. 
      END.
      ELSE DO:
         THIS-OBJECT:ControlShell:KalkControl:GridBerMtrl:GuruReadOnly(FALSE,"ANTAL").
         THIS-OBJECT:ControlShell:KalkControl:AddButton:Visible  = TRUE.
         THIS-OBJECT:ControlShell:KalkControl:RemoveButton:Visible = TRUE. 
         THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("BERKALKAND"):BUFFER-VALUE = TRUE.
        
      END.
      THIS-OBJECT:ControlShell:KalkControl:GridBerMtrl:GuruReopen(). 
     
   END METHOD.
   
   METHOD PUBLIC VOID SparaMtrl():
      DEFINE VARIABLE brow AS ROWID NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE q2H       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar2 AS CHARACTER NO-UNDO.  
      
      IF NOT VALID-HANDLE(Bervallbuffh) THEN RETURN.
      IF THIS-OBJECT:Bervallbuffh:AVAILABLE THEN.
      ELSE RETURN.
      IF THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("BERKALKAND"):BUFFER-VALUE = FALSE THEN RETURN.
            
      
      brow = Bervallbuffh:ROWID.
      
      EMPTY TEMP-TABLE berkalkmtrlandTT NO-ERROR.
      queryvar = "FOR EACH " + THIS-OBJECT:Bervallbuffh:TABLE + " WHERE BERKALKAND = TRUE".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Bervallbuffh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         
         queryvar2 = "FOR EACH " + THIS-OBJECT:Bermtrlbuffh:TABLE + " WHERE NUM = " + STRING(THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE).
         
         q2H = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Bermtrlbuffh,queryvar2).
         q2H:GET-FIRST().
         DO WHILE q2H:QUERY-OFF-END = FALSE:
            CREATE berkalkmtrlandTT.
            BUFFER berkalkmtrlandTT:HANDLE:BUFFER-COPY(THIS-OBJECT:Bermtrlbuffh).
            q2H:GET-NEXT().            
         END.
         THIS-OBJECT:Bervallbuffh:BUFFER-FIELD("BERKALKAND"):BUFFER-VALUE = FALSE.
         qH:GET-NEXT().  
      END.
      
      IF Guru.Konstanter:appcon THEN DO:
         RUN BERKALKMTRLAND.p ON  Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT BernrVar, INPUT BerOmrVar, INPUT TABLE berkalkmtrlandTT).
       
      END.
      ELSE DO:
         RUN BERKALKMTRLAND.p (INPUT BernrVar, INPUT BerOmrVar, INPUT TABLE berkalkmtrlandTT).      
      END. 
 
  
  
           /*
      FOR EACH berkalkmtrlandTT WHERE NO-LOCK:
         MESSAGE BernrVar   BerOmrVar skip
          berkalkmtrlandTT.NUM 
   berkalkmtrlandTT.SKAPNUM skip
   berkalkmtrlandTT.ENR  
   berkalkmtrlandTT.LEVKOD 
   berkalkmtrlandTT.ANTAL 
         
         
         VIEW-AS ALERT-BOX.
      END.
      */  
   END METHOD.
   
   METHOD PUBLIC CHARACTER BortTagenPersonal(INPUT pkodNamnvar AS CHARACTER):
      RUN BortTagenPersonal_UI IN AppServerHandle (INPUT-OUTPUT pkodNamnvar).
      RETURN pkodNamnvar.
   END METHOD.
   METHOD PUBLIC VOID AnvKalkyl (INPUT nykalknr AS INTEGER, INPUT nyomrvar AS CHARACTER):       
      RUN anvkalkyl_UI IN AppServerHandle (INPUT nykalknr,INPUT nyomrvar).      
   END METHOD.
   
   METHOD PUBLIC VOID AnvKalkylHmt(OUTPUT TABLE anvkalkyltt):       
       RUN anvkalkylhmt_UI IN AppServerHandle (OUTPUT TABLE anvkalkyltt).    
   END METHOD.
END CLASS.
