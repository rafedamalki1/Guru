 /*------------------------------------------------------------------------
    File        : KalkylAdmDb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jan 07 14:13:22 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Modules.Beredning.KabelSkapDb: 
   DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO 
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC  PROPERTY valber AS CHARACTER NO-UNDO 
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC  PROPERTY valomr AS CHARACTER NO-UNDO 
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC  PROPERTY BigNumvar AS INTEGER NO-UNDO 
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC  PROPERTY gruppkod AS INTEGER NO-UNDO 
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO. 
   DEFINE PUBLIC VARIABLE ControllKabelSkap                  AS Modules.Beredning.KabelSkap NO-UNDO. 
   
   DEFINE PUBLIC VARIABLE berstarth AS HANDLE NO-UNDO.
   DEFINE VARIABLE inkberh AS HANDLE NO-UNDO.
   
   DEFINE VARIABLE kbenvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kundvar AS LOGICAL NO-UNDO.
   DEFINE PUBLIC VARIABLE b1var AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE b2var AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE b3var AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE b4var AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE b5var AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE b6var AS CHARACTER NO-UNDO. 
   
    
   &Scoped-define PUBLIC PUBLIC
   {KONVALTEMP.I}
   {KONSTRMTRL.I}
   {BBENAMNTEMP.I}
   {BEREDTT.i}
   {BEREDTH.i}  
   
   {INKSUMBERTEMP.I}
   
   CONSTRUCTOR PUBLIC KabelSkapDb(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
     
   END CONSTRUCTOR.
   METHOD PUBLIC VOID Connect(berstarthin AS HANDLE ):
      
      berstarth = berstarthin.
      KonstvaltempTTh = TEMP-TABLE konstvaltempTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      andkonstvaltemp2TTh = TEMP-TABLE andkonstvaltemp2TT:HANDLE:DEFAULT-BUFFER-HANDLE.
      andkonstvaltemp3TTh = TEMP-TABLE andkonstvaltemp3TT:HANDLE:DEFAULT-BUFFER-HANDLE.
      andkonstvaltemp4TTh = TEMP-TABLE andkonstvaltemp4TT:HANDLE:DEFAULT-BUFFER-HANDLE.
      andkonstvaltemp5TTh = TEMP-TABLE andkonstvaltemp5TT:HANDLE:DEFAULT-BUFFER-HANDLE.
      andkonstvaltemp6TTh = TEMP-TABLE andkonstvaltemp6TT:HANDLE:DEFAULT-BUFFER-HANDLE.
     
      RUN gruppkod_UI IN berstarth (OUTPUT gruppkod).
      RUN Kabelandtth_UI IN berstarth (OUTPUT valber, OUTPUT valomr,OUTPUT KonstTTh,OUTPUT bbenamntempTTh,OUTPUT TABLE KonstvaltempTT,OUTPUT KskKon_valTTh,OUTPUT Fri_tempTTh, OUTPUT List_mtrlTTh, OUTPUT Markgrupp_tempTTh).
      
      THIS-OBJECT:BbenamntempTTh:FIND-FIRST("WHERE  ") NO-ERROR.
      b1var =  THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B1"):BUFFER-VALUE.
      b2var =  THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B2"):BUFFER-VALUE.
      b3var =  THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B3"):BUFFER-VALUE.
      b4var =  THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B4"):BUFFER-VALUE.
      b5var =  THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B5"):BUFFER-VALUE.
      b6var =  THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B6"):BUFFER-VALUE.
      IF Guru.Konstanter:appcon THEN DO:
         RUN KABELANDBER.P PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
      END.
      ELSE DO:
         RUN KABELANDBER.P PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globanv).
      END.   
      IF Guru.Konstanter:appcon THEN DO:
         RUN INKBERAPP.P PERSISTENT SET inkberh ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
      END.                             
      ELSE DO:
         RUN INKBERAPP.P PERSISTENT SET inkberh.   
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KskKon_valTTh:TABLE + " BY SKAPNUM".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KskKon_valTTh,queryvar).
      qH:GET-FIRST().
      THIS-OBJECT:BigNumvar = THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE.
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:Fri_tempTTh:FIND-FIRST("WHERE Fri_temp.NUM = " + STRING(THIS-OBJECT:BigNumvar) + 
            " AND Fri_temp.KABNR = " + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE) NO-ERROR.
         IF THIS-OBJECT:Fri_tempTTh:AVAILABLE THEN DO:
            THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("KABELADRESS"):BUFFER-VALUE = THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("KABADR"):BUFFER-VALUE.
         END.
         qH:GET-NEXT(). 
      END.
   
      queryvar =  "FOR EACH " + THIS-OBJECT:List_mtrlTTh:TABLE + " WHERE NUM = " + STRING(BigNumvar) + " AND KUND = FALSE". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:List_mtrlTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         RUN enrhmt_UI IN AppServerHandle (INPUT THIS-OBJECT:List_mtrlTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE,INPUT THIS-OBJECT:List_mtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE, OUTPUT kundvar).
         THIS-OBJECT:List_mtrlTTh:BUFFER-FIELD("KUND"):BUFFER-VALUE = kundvar.
         qH:GET-NEXT().
      END.
      
      queryvar =  "FOR EACH " + THIS-OBJECT:KonstvaltempTTh:TABLE + " WHERE BB = '" + THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B2"):BUFFER-VALUE + "'". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KonstvaltempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:AndKonstVal(2).
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KonstvaltempTTh:TABLE + " WHERE BB = '" + THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B3"):BUFFER-VALUE + "'". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KonstvaltempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:AndKonstVal(3).
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KonstvaltempTTh:TABLE + " WHERE BB = '" + THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B4"):BUFFER-VALUE + "'". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KonstvaltempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:AndKonstVal(4).
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KonstvaltempTTh:TABLE + " WHERE BB = '" + THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B5"):BUFFER-VALUE + "'". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KonstvaltempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:AndKonstVal(5).
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KonstvaltempTTh:TABLE + " WHERE BB = '" + THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B6"):BUFFER-VALUE + "'". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KonstvaltempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:AndKonstVal(6).
         qH:GET-NEXT().
      END.
     
   END METHOD.
   METHOD PUBLIC VOID InitializeTTs(gruppkodIN AS INTEGER):
      gruppkod = gruppkodIN.
       {BEREDKABANDTTTH.i}
   END METHOD.
   
   METHOD PUBLIC VOID UppMtrl():
      RUN uppmtrl_UI IN AppServerHandle (INPUT valber,INPUT valomr,INPUT THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE,INPUT THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE ,OUTPUT TABLE upp_mtrlTT,OUTPUT TABLE ber_mtrltt).
      
      IF  Guru.GlobalaVariabler:plusaonr = ? THEN.
      ELSE RUN beredningskontroll_UI IN inkberh (INPUT   Guru.GlobalaVariabler:plusaonr,INPUT  Guru.GlobalaVariabler:plusdnr,OUTPUT TABLE sumbernum_mtrl,OUTPUT TABLE sumberskapnum_mtrl).
    
   END METHOD.
   METHOD PUBLIC LOGICAL InkopKoll():
      FIND FIRST sumberskapnum_mtrl WHERE sumberskapnum_mtrl.NUM = THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE AND 
      sumberskapnum_mtrl.SKAPNUM = THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE             
      NO-LOCK NO-ERROR.  
        
      IF AVAILABLE sumberskapnum_mtrl THEN RETURN TRUE.
      ELSE RETURN FALSE.
   END METHOD.
   METHOD PUBLIC VOID NyKabel():
      DEFINE VARIABLE tillskapnum AS INTEGER NO-UNDO.
      THIS-OBJECT:KskKon_valTTh:FIND-LAST("WHERE NUM = " + STRING(BigNumvar) + " AND SKAPNUM < " + STRING(99) + " USE-INDEX NUM2") NO-ERROR.
      IF THIS-OBJECT:KskKon_valTTh:AVAILABLE THEN DO:
         tillskapnum = THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE + 1.
      END.
      ELSE tillskapnum = 1.
      IF tillskapnum >= 99 THEN DO:
         THIS-OBJECT:KskKon_valTTh:FIND-LAST("WHERE NUM = " + STRING(BigNumvar) + " USE-INDEX NUM2") NO-ERROR.
      END.   
      THIS-OBJECT:Fri_tempTTh:BUFFER-CREATE(). 
      THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE = BigNumvar.
      THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("KABNR"):BUFFER-VALUE = tillskapnum.
      THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("VARSKAPAD"):BUFFER-VALUE = TRUE.
      THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE = YEAR(TODAY).  
      
      THIS-OBJECT:KskKon_valTTh:FIND-FIRST("WHERE NUM = " + STRING(0) + " AND SKAPNUM = " + STRING(0)) NO-ERROR.
      IF THIS-OBJECT:KskKon_valTTh:AVAILABLE THEN DO:
         ASSIGN
         THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("TYP"):BUFFER-VALUE = "1"
         THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("GRUPP"):BUFFER-VALUE = gruppkod
         THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE = BigNumvar
         THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE = tillskapnum.
         THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("KSKAP"):BUFFER-VALUE = TRUE.
         
         IF THIS-OBJECT:ControllKabelSkap:GridB1:ActiveRow NE ? THEN THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F1"):BUFFER-VALUE = THIS-OBJECT:ControllKabelSkap:GridB1:GridFaltToString("KTYPKOD").
         IF THIS-OBJECT:ControllKabelSkap:GridB2:ActiveRow NE ? THEN THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F2"):BUFFER-VALUE = THIS-OBJECT:ControllKabelSkap:GridB2:GridFaltToString("KVALKOD").
         IF THIS-OBJECT:ControllKabelSkap:GridB3:ActiveRow NE ? THEN THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F3"):BUFFER-VALUE = THIS-OBJECT:ControllKabelSkap:GridB3:GridFaltToString("KVALKOD").
         IF THIS-OBJECT:ControllKabelSkap:GridB4:ActiveRow NE ? THEN THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F4"):BUFFER-VALUE = THIS-OBJECT:ControllKabelSkap:GridB4:GridFaltToString("KVALKOD").
         IF THIS-OBJECT:ControllKabelSkap:GridB5:ActiveRow NE ? THEN THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F5"):BUFFER-VALUE = THIS-OBJECT:ControllKabelSkap:GridB5:GridFaltToString("KVALKOD").
         IF THIS-OBJECT:ControllKabelSkap:GridB6:ActiveRow NE ? THEN THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F6"):BUFFER-VALUE = THIS-OBJECT:ControllKabelSkap:GridB6:GridFaltToString("KVALKOD").
     
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F2"):BUFFER-VALUE NE "" THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F1 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F2"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:AddMtrl(tillskapnum).
         END.
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F3"):BUFFER-VALUE NE "" THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F3"):BUFFER-VALUE +
              "' AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:AddMtrl(tillskapnum).
         END.
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F4"):BUFFER-VALUE NE "" THEN DO:   
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F4"):BUFFER-VALUE +
              "' AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:AddMtrl(tillskapnum).
         END.
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F5"):BUFFER-VALUE NE "" THEN DO:   
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE +  
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F5"):BUFFER-VALUE +
              "' AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:AddMtrl(tillskapnum).
         END.
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F6"):BUFFER-VALUE NE "" THEN DO:   
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE +  
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F6"):BUFFER-VALUE + "'".
            THIS-OBJECT:AddMtrl(tillskapnum).
         END.
      END.         
   END METHOD.
   METHOD PUBLIC VOID AddMtrl(tillskapnum AS INTEGER):
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Upp_mtrlTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:List_mtrlTTh:BUFFER-CREATE().
         THIS-OBJECT:List_mtrlTTh:BUFFER-COPY(Upp_mtrlTTh).
         THIS-OBJECT:List_mtrlTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE = BigNumvar.
         THIS-OBJECT:List_mtrlTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE = tillskapnum.
         qH:GET-NEXT().
      END.
   END METHOD.
   METHOD PUBLIC VOID BortMtrl():
      queryvar =  "FOR EACH " + THIS-OBJECT:List_mtrlTTh:TABLE + " WHERE NUM = " + STRING(BigNumvar) + 
      " AND SKAPNUM = " + STRING(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:List_mtrlTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:List_mtrlTTh:BUFFER-DELETE(). 
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:Fri_tempTTh:TABLE + " WHERE NUM = " + STRING(BigNumvar) + 
      " AND KABNR = " + STRING(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Fri_tempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:Fri_tempTTh:BUFFER-DELETE(). 
         qH:GET-NEXT().
      END.
     
   END METHOD.
   METHOD PUBLIC VOID BytKabelDelFran():
      
      IF THIS-OBJECT:ControllKabelSkap:beforekolnamn = "F2" THEN DO:
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F2"):BUFFER-VALUE NE "" THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F1 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F2"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:BytMtrlBort().
         END.   
      END.
      IF THIS-OBJECT:ControllKabelSkap:beforekolnamn = "F3" THEN DO:
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F3"):BUFFER-VALUE NE "" THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F3"):BUFFER-VALUE +
              "' AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:BytMtrlBort().
         END.
      END.
      IF THIS-OBJECT:ControllKabelSkap:beforekolnamn = "F4" THEN DO:   
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F4"):BUFFER-VALUE NE "" THEN DO:   
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE +  
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F4"):BUFFER-VALUE +
              "' AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:BytMtrlBort().
         END.
      END.
      IF THIS-OBJECT:ControllKabelSkap:beforekolnamn = "F5" THEN DO:   
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F5"):BUFFER-VALUE NE "" THEN DO:   
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE +  
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F5"):BUFFER-VALUE +
              "' AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:BytMtrlBort().
         END.
      END.
      IF THIS-OBJECT:ControllKabelSkap:beforekolnamn = "F6" THEN DO:
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F6"):BUFFER-VALUE NE "" THEN DO:   
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE +  
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F6"):BUFFER-VALUE + "'".
            THIS-OBJECT:BytMtrlBort().
         END.
      END.   
   END METHOD.
   METHOD PUBLIC VOID BytMtrlBort():
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Upp_mtrlTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:List_mtrlTTh:FIND-FIRST("WHERE  NUM = " + STRING(BigNumvar) + " AND SKAPNUM = " + STRING(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE) + " AND ENR = '" + THIS-OBJECT:Upp_mtrlTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE + "'") NO-ERROR.
         IF THIS-OBJECT:List_mtrlTTh:AVAILABLE THEN DO:
            THIS-OBJECT:List_mtrlTTh:BUFFER-DELETE(). 
         END.
         qH:GET-NEXT().
      END.
   END METHOD.
   METHOD PUBLIC VOID BytKabelDelTill():
      
      IF THIS-OBJECT:ControllKabelSkap:beforekolnamn = "F2" THEN DO:
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F2"):BUFFER-VALUE NE "" THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F1 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F2"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:AddMtrl(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE ).
         END.
      END.   
      IF THIS-OBJECT:ControllKabelSkap:beforekolnamn = "F3" THEN DO:
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F3"):BUFFER-VALUE NE "" THEN DO:
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F3"):BUFFER-VALUE +
              "' AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:AddMtrl(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE ).
         END.
      END.   
      IF THIS-OBJECT:ControllKabelSkap:beforekolnamn = "F4" THEN DO:   
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F4"):BUFFER-VALUE NE "" THEN DO:   
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F4"):BUFFER-VALUE +
              "' AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:AddMtrl(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE ).
         END.
      END.   
      IF THIS-OBJECT:ControllKabelSkap:beforekolnamn = "F5" THEN DO:   
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F5"):BUFFER-VALUE NE "" THEN DO:   
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F5"):BUFFER-VALUE +
              "' AND upp_mtrlTT.F5 = ''".
            THIS-OBJECT:AddMtrl(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE ).
         END.
      END.   
      IF THIS-OBJECT:ControllKabelSkap:beforekolnamn = "F6" THEN DO:   
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F6"):BUFFER-VALUE NE "" THEN DO:   
            queryvar =  "FOR EACH " + THIS-OBJECT:Upp_mtrlTTh:TABLE + " WHERE upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE +  
              "' AND upp_mtrlTT.F1 = ''  
                 AND upp_mtrlTT.F2 = ''
                 AND upp_mtrlTT.F3 = ''
                 AND upp_mtrlTT.F4 = ''
                 AND upp_mtrlTT.F5 = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F6"):BUFFER-VALUE + "'".
            THIS-OBJECT:AddMtrl(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE ).
         END.
      END.   
   END METHOD.
   DESTRUCTOR KabelSkapDb():
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DELETE PROCEDURE THIS-OBJECT:AppServerHandle NO-ERROR.
      IF VALID-HANDLE(inkberh) THEN DELETE PROCEDURE inkberh NO-ERROR.
   END DESTRUCTOR.
  /*special--------------------------------*/
   METHOD PUBLIC VOID andbrwb2(INPUT kvalkodvar AS CHARACTER):
      DEFINE VARIABLE barea AS CHARACTER NO-UNDO.
      DEFINE VARIABLE karea AS INTEGER NO-UNDO.
      DEFINE VARIABLE ansld AS CHARACTER NO-UNDO.
      ansld = TRIM(kvalkodvar).  
      /*
      FIND FIRST Konstvaltemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Konstvaltemp THEN DO:
         RUN KonstvaltempHmt_UI IN berstarth (OUTPUT TABLE Konstvaltemp).
      END.
      */
      IF gruppkod = 0 THEN DO:               
         barea = THIS-OBJECT:goromDec (INPUT kvalkodvar).
         /*ansl*/    
         THIS-OBJECT:andkonstvaltemp3TTh:EMPTY-TEMP-TABLE() NO-ERROR.     
         karea = INTEGER(barea) NO-ERROR.
         THIS-OBJECT:bortkabk(INPUT karea, INPUT 3, INPUT "lsp").
      END.      
      ELSE DO:
         
         THIS-OBJECT:stationsakrh(ansld).                          
      END.
   END METHOD.
   
   METHOD PUBLIC VOID stationsakrh(ansld AS CHARACTER):
      THIS-OBJECT:andkonstvaltemp4TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:bortsakh(INPUT ansld).
   END METHOD.
   METHOD PUBLIC VOID bortsakh(ansld AS CHARACTER):
      DEFINE VARIABLE sakkopp AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE hjbenvar AS CHARACTER NO-UNDO.   
      hjbenvar = THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B4"):BUFFER-VALUE.
      IF ansld = "NALF+JK" OR ansld = "SÄK.APP" OR ansld = "SÄKH" OR ansld = "Trafofack" THEN DO:
         
         FOR EACH konstvaltempTT WHERE konstvaltempTT.KONSKOD = gruppkod AND konstvaltempTT.KTYPKOD =  
         THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         konstvaltempTT.KOPP = TRUE AND konstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(konstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "00/" THEN.
            ELSE IF sakkopp BEGINS "000/" THEN.
            ELSE IF sakkopp BEGINS "C 00/" THEN.
            ELSE IF sakkopp BEGINS "1/" THEN.
            ELSE IF sakkopp BEGINS "2/" THEN.
            ELSE IF sakkopp BEGINS "3/" THEN.
            ELSE IF sakkopp BEGINS "DII/" THEN.
            ELSE IF sakkopp BEGINS "DIII/" THEN.
            ELSE THIS-OBJECT:AndKonstVal(4).         
         END.
      END.   
      ELSE DO:
         FOR EACH konstvaltempTT WHERE konstvaltempTT.KONSKOD = gruppkod AND konstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         konstvaltempTT.KOPP = TRUE AND konstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(konstvaltempTT.KVALKOD).
         END.
      END.

   END METHOD.
   METHOD PUBLIC VOID AndKonstVal(INPUT andkonstval AS INTEGER):
       IF andkonstval = 2 THEN DO:
          andkonstvaltemp2TTh:BUFFER-CREATE().
          andkonstvaltemp2TTh:BUFFER-COPY(KonstvaltempTTh).
          andkonstvaltemp2TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = andkonstvaltemp2TTh:RECID.
       END.
       IF andkonstval = 3 THEN DO:
          andkonstvaltemp3TTh:BUFFER-CREATE().
          andkonstvaltemp3TTh:BUFFER-COPY(KonstvaltempTTh).
          andkonstvaltemp3TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = andkonstvaltemp3TTh:RECID.
       END.
       IF andkonstval = 4 THEN DO:
          andkonstvaltemp4TTh:BUFFER-CREATE().
          andkonstvaltemp4TTh:BUFFER-COPY(KonstvaltempTTh).
          andkonstvaltemp4TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = andkonstvaltemp4TTh:RECID.
       END.
       IF andkonstval = 5 THEN DO:
          andkonstvaltemp5TTh:BUFFER-CREATE().
          andkonstvaltemp5TTh:BUFFER-COPY(KonstvaltempTTh).
          andkonstvaltemp5TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = andkonstvaltemp5TTh:RECID.
       END.
       IF andkonstval = 6 THEN DO:
          andkonstvaltemp6TTh:BUFFER-CREATE().
          andkonstvaltemp6TTh:BUFFER-COPY(KonstvaltempTTh).
          andkonstvaltemp6TTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = andkonstvaltemp6TTh:RECID.
       END.
            
   END METHOD.
   
   METHOD PUBLIC CHARACTER goromDec(INPUT kvarde AS CHARACTER):
      DEFINE VARIABLE barea AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.   
      DEFINE VARIABLE vardearray AS CHARACTER EXTENT 50 NO-UNDO.
      DEFINE VARIABLE vlangd AS INTEGER NO-UNDO.
      DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
      DEFINE VARIABLE hjarea AS CHARACTER NO-UNDO.
      ivar = 1.
      vlangd = LENGTH(kvarde).
      DO WHILE ivar <= (vlangd):
         vardearray[ivar] = SUBSTRING(kvarde,1, 1).
         kvarde = SUBSTRING(kvarde,2).
         ivar = ivar + 1.
      END.
      ASSIGN
      kvarde = ""  barea = "" hjarea = "" ivar = 1.
      DO WHILE ivar <= vlangd:
         ascivarde = ASC(vardearray[ivar]).      
         /*siffror*/
         IF ascivarde >= 48 AND ascivarde <= 57 THEN barea = barea  + vardearray[ivar].      
         ELSE barea = "".      
         ivar = ivar + 1.   
         IF barea NE "" THEN hjarea = barea.   
      END.      
      IF barea = "" THEN barea = hjarea.
      IF LENGTH(barea) = 4 THEN barea = substring(barea,2).
      IF LENGTH(barea) = 5 THEN barea = substring(barea,3).
      RETURN barea.
   END METHOD.
   
   METHOD PUBLIC VOID bortkabk(INPUT karea AS INTEGER,INPUT valdbrw AS INTEGER,INPUT lsphsp AS CHARACTER):
      DEFINE VARIABLE anslkopp AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE hjbenvar AS CHARACTER NO-UNDO.
      IF valdbrw = 2 THEN DO:
         IF gruppkod = 0 THEN hjbenvar = THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B2"):BUFFER-VALUE.
         ELSE hjbenvar = THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B2"):BUFFER-VALUE.
      END.
      IF valdbrw = 3 THEN DO:
         IF gruppkod = 0 THEN hjbenvar = THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B3"):BUFFER-VALUE.
         ELSE hjbenvar = THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B3"):BUFFER-VALUE.
      END.
      IF valdbrw = 4 THEN DO:
         IF gruppkod = 0 THEN hjbenvar = THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B4"):BUFFER-VALUE.
         ELSE hjbenvar = THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B4"):BUFFER-VALUE.
      END.  
      IF valdbrw = 3 AND lsphsp = "hsp"  THEN DO:
         
      END. 
      ELSE IF valdbrw = 2 AND lsphsp = "hsp"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            THIS-OBJECT:AndKonstVal(valdbrw).
         END.
      END.
      ELSE IF valdbrw = 2 AND lsphsp = "lsp"  THEN DO:
      
      END.
      
      ELSE DO:   
         IF karea = 0 THEN.
         ELSE IF /*karea GE 6 AND*/ karea LE 25 THEN DO:     
            FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE AND 
            KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
               anslkopp = TRIM(KonstvaltempTT.KVALKOD).
               IF anslkopp BEGINS "AD95" OR anslkopp = "AD 95"  OR anslkopp = "KA95 4" OR anslkopp = "KF-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "sld000" OR anslkopp = "KG4" THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "sld00" OR anslkopp = "JM00-4" OR anslkopp = "KS00 4" THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "AKZD95" OR anslkopp = "KA 95" OR anslkopp = "AKZD 95"   THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "LDL63/3" OR anslkopp = "CEWE 3gr"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "XLBM00" OR anslkopp = "CEWE 00" OR anslkopp = "CEWE GB00K"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "GB00K-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "LDL63/2"  THEN THIS-OBJECT:AndKonstVal(valdbrw).            
               ELSE IF anslkopp BEGINS "FS160" OR anslkopp = "KS 00-4" OR anslkopp = "FS 160"   THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "DIN00-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "Bef.DZ"  THEN THIS-OBJECT:AndKonstVal(valdbrw).           
            END.
         END.
         ELSE IF karea > 25 AND karea < 50 THEN DO:
            FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
            KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
               anslkopp = TRIM(KonstvaltempTT.KVALKOD).
               IF anslkopp BEGINS "AD95" OR anslkopp = "AD 95"  OR anslkopp = "KA95 4" OR anslkopp = "KF-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "sld000" OR anslkopp = "KG4" THEN THIS-OBJECT:AndKonstVal(valdbrw).           
               ELSE IF anslkopp BEGINS "sld00" OR anslkopp = "JM00-4" OR anslkopp = "KS00 4" THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "AKZD95" OR anslkopp = "KA 95" OR anslkopp = "AKZD 95"   THEN THIS-OBJECT:AndKonstVal(valdbrw).        
               ELSE IF anslkopp BEGINS "XLBM00" OR anslkopp = "CEWE 00" OR anslkopp = "CEWE GB00K"  THEN THIS-OBJECT:AndKonstVal(valdbrw).  
               ELSE IF anslkopp = "GB00K-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "FS160"  OR anslkopp = "KS 00-4" OR anslkopp = "FS 160"   THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "JM2-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "JM3P-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "DIN00-4" OR anslkopp = "DIN1-4" OR anslkopp = "DIN2-4" OR anslkopp = "DIN3-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
            END.
         END.
         ELSE IF karea GE 50 AND karea LE 95 THEN DO:
            FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
            KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
               anslkopp = TRIM(KonstvaltempTT.KVALKOD).                     
               IF anslkopp BEGINS "LDL63/3" OR anslkopp BEGINS "LDL63/2" OR anslkopp = "Bef.DZ" THEN.         
               ELSE THIS-OBJECT:AndKonstVal(valdbrw).

            END.
         END.
         ELSE IF karea > 95 AND karea LE 300 THEN DO:
            FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
            KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
               anslkopp = TRIM(KonstvaltempTT.KVALKOD).
               IF anslkopp BEGINS "AD300" OR anslkopp = "AD 300" OR anslkopp = "KA240 4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "FD3300" OR anslkopp = "KF 4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "sld1" OR anslkopp = "KS 0-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "sld2" OR anslkopp = "KS2 4" OR anslkopp = "KS  2-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).         
               ELSE IF anslkopp BEGINS "AKKZ300" OR anslkopp = "KA 240-4" OR anslkopp = "AKKZ 300"   THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "XLBM1" OR anslkopp = "KS 2_4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "XLBM2" OR anslkopp = "KS 2-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "XLBM3"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp BEGINS "GLB2"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "JM2-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "JM3P-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).            
               ELSE IF anslkopp = "Bef.1"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "Bef.2"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "BTVC"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
               ELSE IF anslkopp = "Bef.00"  THEN THIS-OBJECT:AndKonstVal(valdbrw).            
               ELSE IF anslkopp = "DIN1-4" OR anslkopp = "DIN2-4" OR anslkopp = "DIN3-4"  THEN THIS-OBJECT:AndKonstVal(valdbrw).
            END.
         END.     
         ELSE IF  karea > 300 THEN DO:
            FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
            KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
          
            END.
         END.
      END.
   END METHOD.
   METHOD PUBLIC VOID andbrwb3(INPUT kvalkodvar AS CHARACTER):
      DEFINE VARIABLE ansld AS CHARACTER NO-UNDO.
      ansld = trim(kvalkodvar).
      IF gruppkod = 0 THEN DO:      
         THIS-OBJECT:andkonstvaltemp5TTh:EMPTY-TEMP-TABLE() NO-ERROR.
         THIS-OBJECT:bortsak(INPUT ansld).
      END. 
      ELSE DO:
         THIS-OBJECT:stationsakr(ansld).                          
      END.
   
   END METHOD.
   METHOD PUBLIC VOID stationsakr(ansld AS CHARACTER):
        
      EMPTY TEMP-TABLE ekonstvaltemp NO-ERROR.
      THIS-OBJECT:andkonstvaltemp4TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:bortsak(INPUT ansld).
   END METHOD.
   METHOD PUBLIC VOID bortsak(INPUT ansld AS CHARACTER):
      DEFINE VARIABLE valdbrw AS INTEGER NO-UNDO.
      DEFINE VARIABLE sakkopp AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE hjbenvar AS CHARACTER NO-UNDO.
      IF gruppkod = 0 THEN DO:
         hjbenvar = THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B5"):BUFFER-VALUE.
         valdbrw = 5.
      END.   
      ELSE DO:
         hjbenvar = THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B4"):BUFFER-VALUE.
         valdbrw = 4.
      END.   
      IF ansld BEGINS "sld000" OR ansld = "KG4" THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "00/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
            ELSE IF sakkopp BEGINS "000/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
            ELSE IF sakkopp BEGINS "C 00/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
                     
         END.
      END.   
      ELSE IF ansld BEGINS "sld00" OR ansld = "JM00-4" OR ansld = "Bef.00" OR ansld = "KS00 4" THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "00/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
            ELSE IF sakkopp BEGINS "C 00/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
         END.
      END.
      ELSE IF ansld BEGINS "sld1" OR ansld = "KS 0-4" OR ansld = "Bef.1"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "1/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
         END.
      END.
      ELSE IF ansld BEGINS "sld2" OR ansld = "KS2 4" OR ansld = "KS  2-4" OR ansld = "Bef.2" OR ansld = "BTVC" THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "2/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
         END.
      END.   
      ELSE IF  ansld BEGINS "GLB2"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "1/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
            ELSE IF sakkopp BEGINS "2/" THEN THIS-OBJECT:AndKonstVal(valdbrw).               
         END.
      END.
      ELSE IF ansld BEGINS "AD300" OR ansld = "AD 300" OR ansld = "KA240 4" OR ansld = "KF-4" THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
                     
         END.
      END.
      ELSE IF ansld BEGINS "AD95" OR ansld = "AD 95" OR ansld = "KA95 4"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
         END.
      END.
      ELSE IF ansld BEGINS "FD3300" OR ansld = "KF 4"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
         END.
      END.
      ELSE IF ansld BEGINS "AKKZ300" OR ansld = "KA 240-4" OR ansld = "AKKZ 300"    THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
        END.
      END.
      ELSE IF ansld BEGINS "AKZD95" OR ansld = "KA 95" OR ansld = "AKZD 95"    THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
        END.
      END.
      ELSE IF ansld BEGINS "LDL63/3" OR ansld BEGINS "LDL63/2" OR ansld BEGINS "Bef.DZ" OR ansld = "CEWE 3gr" OR ansld =  "CEWE 63/2K"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "DII/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
            ELSE IF sakkopp BEGINS "DIII/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
                     
        END.
      END.   
      ELSE IF ansld BEGINS "FS160" OR ansld = "KS 00-4" OR ansld = "FS 160"  OR ansld = "DIN00-4"     THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "00/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
            ELSE IF sakkopp BEGINS "C 00/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
                     
        END.
      END.
      ELSE IF ansld BEGINS "XLBM00" OR ansld = "GB00K-4" OR ansld = "CEWE 00" OR ansld = "CEWE GB00K"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "00/" THEN THIS-OBJECT:AndKonstVal(valdbrw). 
            ELSE IF sakkopp BEGINS "C 00/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
                     
        END.
      END.
      ELSE IF ansld BEGINS "XLBM1" OR ansld = "KS 2_4"  OR ansld = "DIN1-4"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "1/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
                     
        END.
      END.
      ELSE IF ansld BEGINS "XLBM2" OR ansld = "KS 2-4"  OR ansld = "DIN2-4"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "2/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
                     
        END.
      END.
      ELSE IF ansld BEGINS "XLBM3"  OR ansld = "DIN3-4"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "3/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
                     
        END.
      END.   
      ELSE IF ansld = "JM2-4"   THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "2/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
                     
        END.
      END.
      ELSE IF ansld = "JM3P-4"  THEN DO:
         FOR EACH KonstvaltempTT WHERE KonstvaltempTT.KONSKOD = gruppkod AND KonstvaltempTT.KTYPKOD =  THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE  AND 
         KonstvaltempTT.KOPP = TRUE AND KonstvaltempTT.BB =  hjbenvar  NO-LOCK:      
            sakkopp = TRIM(KonstvaltempTT.KVALKOD).
            IF sakkopp BEGINS "3/" THEN THIS-OBJECT:AndKonstVal(valdbrw).
                     
        END.
      END. 
   END METHOD.
   METHOD PUBLIC VOID andbrwb5(INPUT kvalkodvar AS CHARACTER):
      
      THIS-OBJECT:stationkab(kvalkodvar).                                      
   
   END METHOD.
   METHOD PUBLIC VOID stationkab(INPUT kvalkodvar AS CHARACTER):
      DEFINE VARIABLE lsphsp AS CHARACTER NO-UNDO.
      DEFINE VARIABLE barea AS CHARACTER NO-UNDO.
      DEFINE VARIABLE karea AS INTEGER NO-UNDO.
      /*test kabel lena cccc*/
      RUN stationkab_UI IN berstarth (INPUT kvalkodvar, OUTPUT lsphsp).
      barea = THIS-OBJECT:goromDec(INPUT kvalkodvar).  
      THIS-OBJECT:andkonstvaltemp2TTh:EMPTY-TEMP-TABLE() NO-ERROR.
      THIS-OBJECT:andkonstvaltemp3TTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      
      karea = INTEGER(barea) NO-ERROR.   
      THIS-OBJECT:bortkabk(INPUT karea, INPUT 2,INPUT lsphsp).
      THIS-OBJECT:bortkabk(INPUT karea, INPUT 3,INPUT lsphsp).
      
   END METHOD.
END CLASS.
