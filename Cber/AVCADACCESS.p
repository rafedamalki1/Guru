/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: AVCADACCESS.p
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2007.12.03 14:04 ELPAO   
     Modified: 2007.12.27 10:48 ELPAO    
     Modified: 2008.03.13 11:54 ELPAO    
     Modified: 2008.10.08 15:48 ELPAO    
     Modified: 2009.03.19 16:11 ELPAO    
     Modified: 2009.03.23 10:29 ELPAO    
     Modified: 2009.10.16 15:57 ELPAO    
     Modified: 2009.12.04 13:14 ELPAO    
     Modified: 2010.06.15 11:08 ELPAO    
     Modified: 

ORDER
OrderName     ID
Token         13
OrderName	Comment	Id	CreatedCad
NT7621 L02	X       109	-1
SURVEY 
ID            OrderID   SurveyNamn
1             13        Token1
9             13        Token2

POLE
ID SurveyID   StructID   Totlength  PoleNR PoleClass
55    9       EÄB21      10         1       G
56    9       ERB21      11         2       N


STRUCT
ID   Namn     TYPE   Class
78   EÄB21    3
79   ERB21    0

SEGMENTS
ID         SurveyID  CabelID Fran Till Lina       Area
11          9        -1      21   39   BLL 24 KV    99
12          9        -1      20   21   ALUS        380

CABLE
ID     Namn       TYPE
3      Alus         2
13     BLL 24 KV    1

Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\SIBSQL\AvCad\DBF\AvCad.mdb;Persist Security Info=False
*/
{BERPUNKTTEMP.I}
DEFINE INPUT  PARAMETER globanv AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER globforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE valdminarea AS INTEGER NO-UNDO.

DEF VAR hAccess       AS COM-HANDLE NO-UNDO.
DEF VAR hCurrdb       AS COM-HANDLE NO-UNDO.
DEF VAR hTable        AS COM-HANDLE NO-UNDO.
DEF VAR hTable2        AS COM-HANDLE NO-UNDO.
DEF VAR hfield        AS COM-HANDLE NO-UNDO.
DEF VAR l-database    AS CHAR       NO-UNDO INIT "C:\Program\SIBSQL\AvCad\Dbf\AvCadx.mdb".
DEF VAR avcdatabase    AS CHAR       NO-UNDO INIT "C:\Program\SIBSQL\AvCad\Dbf\AvCadx.mdb".
DEF VAR l-textformat  AS INT        NO-UNDO INIT 10.
DEF VAR l-fieldLength AS INT        NO-UNDO INIT 40.
DEF VAR l-Result      AS LOG        NO-UNDO.



DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valbernr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE katvar AS INTEGER NO-UNDO.

DEFINE VARIABLE num AS INTEGER NO-UNDO.
DEFINE VARIABLE ordernummer AS INTEGER NO-UNDO.
DEFINE VARIABLE surint AS INTEGER NO-UNDO.
DEFINE VARIABLE com AS CHARACTER NO-UNDO.
DEFINE VARIABLE com2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE st AS INTEGER NO-UNDO.
DEFINE VARIABLE sl AS INTEGER NO-UNDO.
DEFINE VARIABLE path AS CHARACTER.
DEFINE VARIABLE idnum AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER NO-UNDO.
DEFINE VARIABLE counter AS INTEGER NO-UNDO.
DEFINE VARIABLE odbch AS HANDLE NO-UNDO.
&Scoped-define NEW   
{ODBCTEMP.I}
DEFINE TEMP-TABLE feltemp NO-UNDO
   FIELD FELTEXT AS CHARACTER.
&Scoped-define SHARED 
{BEREDNINGTEMP.I}
{KONVALTEMP.I}

DEFINE TEMP-TABLE avcadtemp
   FIELD ID AS INTEGER LABEL "Id"
   FIELD BENAMNING AS CHARACTER LABEL "Benämning"
   FIELD COMMENT AS CHARACTER LABEL "Kommentar"
   FIELD AVVAGNING AS INTEGER LABEL "Avvägning"
   INDEX ID IS PRIMARY ID.
                          
/*
DEFINE TEMP-TABLE tt_order 
   FIELD ORDERNUM    AS INTEGER
   FIELD NAMN        AS CHARACTER
   INDEX ORDERNUM ORDERNUM.
*/
{AVCADTEMP.I}
DEFINE VARIABLE avcadversion AS CHARACTER NO-UNDO.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
DEFINE VARIABLE odbcvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
PROCEDURE load_UI :
   DEFINE INPUT  PARAMETER pathAVCaddb AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER avcadok AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER avdatabase AS CHARACTER NO-UNDO.
  
   RUN close_UI.
   IF pathAVCaddb NE "" THEN DO:
      l-database = pathAVCaddb.
      path = SEARCH(l-database).
   END.   
   ELSE IF Guru.Konstanter:globforetag = "vast" THEN DO:
      l-database = "C:\Users\" + globanv + "\AppData\Local\VirtualStore\Program Files (x86)\SIBSQL\AvCad\Dbf\AvCad.mdb".
      path = SEARCH(l-database).
   END. 
   IF path = "" OR  path = ? THEN DO:
      avcadversion = "AvCad 6".
      LOAD "Software\SIB Elkraft & Data AB\" + avcadversion BASE-KEY "HKEY_CURRENT_USER" NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES = 0 THEN.
      ELSE DO:
         avcadversion = "AvCad 5".
         LOAD "Software\SIB Elkraft & Data AB\" + avcadversion BASE-KEY "HKEY_CURRENT_USER" NO-ERROR.
      END.
      IF ERROR-STATUS:NUM-MESSAGES = 0 THEN.
      ELSE DO:  
         avcadversion = "AvCad 4".
         LOAD "Software\SIB Elkraft & Data AB\" + avcadversion BASE-KEY "HKEY_CURRENT_USER" NO-ERROR.
      END.   
     
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         MESSAGE 
         "AvCad är inte installerad så som programmet förutsätter." SKIP 
         "Det går ej att ansluta AvCAD automatiskt." SKIP
         "Välj AvCAD databas." SKIP 
         "Kontakta Elpool tel 090/184540." SKIP
         SKIP
         "Vill du se fler felmeddelanden?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel!"
         UPDATE view-errs AS LOGICAL .       
         IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
            MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
            ERROR-STATUS:GET-MESSAGE(ivar)
            VIEW-AS ALERT-BOX.
         END.
         RETURN.
      END.
      /*access*/
      LOAD "Software\SIB Elkraft & Data AB" BASE-KEY "HKEY_CURRENT_USER" NO-ERROR.
      USE "Software\SIB Elkraft & Data AB".
      RUN AvCadversion_UI.
      
      UNLOAD "Software\SIB Elkraft & Data AB".
      /*
   \\it.pitea.se\users\extern\pikab\pe\PE84001\My Documents\SIB Elkraft o Data AB\AvCad5\DBF\AvCad.mdb
   \\it.pitea.se\users\extern\pikab\pe\PE84001\My Documents\SIB Elkraft o Data AB\AvCad5\DBF\AvCad.mdb
   */  
      IF l-database = ? OR  avcdatabase = ? THEN DO:
         MESSAGE 
         "AvCad är inte installerad så som programmet förutsätter." SKIP 
         "Det går ej att ansluta AvCAD automatiskt." SKIP
         "Välj AvCAD databas." SKIP 
         "Kontakta Elpool tel 090/184540." SKIP
         VIEW-AS ALERT-BOX.
         RETURN.
      END.   
     
      IF Guru.Konstanter:globforetag = "PITE" THEN l-database = avcdatabase.
      ELSE DO:
         st = INDEX(l-database,"c:\",1).
         IF st = 0 THEN  st = INDEX(l-database,"d:\",1).
         l-database = SUBSTRING(l-database,st,LENGTH(l-database)).
         sl = INDEX(l-database,"AvCad.mdb",1).
         IF sl = 0 THEN DO:
            sl = INDEX(l-database,"AvCadE.mdb",1).
            sl = sl + 1.
         END.   
         sl = sl + 8.
         l-database = SUBSTRING(l-database,1,sl).
      END.
      path = SEARCH(l-database).
      IF path = ? THEN DO:
         path = SEARCH(avcdatabase).
         l-database = avcdatabase.
      END.   
   END. 
   /*
   path = "C:\Program Files\SIBSQL\AvCad\Dbf\AVCADSQL.mdf".
    MESSAGE l-database path  VIEW-AS ALERT-BOX.
   */
  
   IF path NE ? THEN DO:
      CREATE "Access.Application" hAccess CONNECT TO l-database NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         RUN odbc_UI (OUTPUT avcadok).
         IF avcadok = FALSE THEN DO:
            MESSAGE "Kunde inte ladda databasen, kontakta Elpool på 090-184540. Ange:" l-database VIEW-AS ALERT-BOX.
            avdatabase = path.
            RETURN.
         END.
         avcadok = TRUE.
      END.
      ELSE DO:
         hAccess:VISIBLE = NO.
         hCurrdb    = hAccess:CurrentDb.
         IF NOT VALID-HANDLE(hCurrdb) THEN DO:
            RUN odbc_UI (OUTPUT avcadok).
            IF avcadok = FALSE THEN DO:
               MESSAGE "Kunde inte ladda databasen, kontakta Elpool på 090-184540. Ange:" l-database VIEW-AS ALERT-BOX.
               avdatabase = path.
               RETURN.
            END.
            avcadok = TRUE.

         END.
         avcadok = TRUE.
      END.
      avdatabase = path.
      RETURN.
   END.
   ELSE DO:
      RUN odbc_UI (OUTPUT avcadok).
      IF avcadok = FALSE THEN DO:
         LOAD "Software\SIB Elkraft & Data AB" BASE-KEY "HKEY_CURRENT_USER".
         USE "Software\SIB Elkraft & Data AB".
         RUN AvCadversion_UI.
         UNLOAD "Software\SIB Elkraft & Data AB".
         st = INDEX(l-database,"c:\",1).
         l-database = SUBSTRING(l-database,st,LENGTH(l-database)).
         sl = INDEX(l-database,"AVCADSQL.mdf",1).
         sl = sl + 11.
         l-database = SUBSTRING(l-database,1,sl).
         path = SEARCH(l-database).
         MESSAGE "Du kör en SQL databas! Kontakta Elpool på 090-184540. Ange:" l-database VIEW-AS ALERT-BOX.
         avdatabase = path.
         RETURN.
      END.
   END.
   
END PROCEDURE.
PROCEDURE AvCadversion_UI :
   IF avcadversion = "AvCad 4" THEN DO:
      GET-KEY-VALUE SECTION avcadversion KEY "Databas" VALUE l-database.
      GET-KEY-VALUE SECTION avcadversion KEY "Access Fil" VALUE avcdatabase.    
   END.
   ELSE DO:   
      GET-KEY-VALUE SECTION avcadversion KEY "CONNECTION STRING" VALUE l-database.
      GET-KEY-VALUE SECTION avcadversion KEY "DBFPATH" VALUE avcdatabase.
   END.
   IF l-database = ? THEN l-database = avcdatabase.
END PROCEDURE.

PROCEDURE odbc_UI :
   /*obs 32bit*/
   DEFINE OUTPUT PARAMETER avcadok AS LOGICAL NO-UNDO.
  
   RUN ODBCACESS.P PERSISTENT SET odbch (INPUT "SIB",
                                 INPUT "localhost",
                                 INPUT "",
                                 INPUT "",
                                 OUTPUT hAccess,
                                 OUTPUT TABLE feltemp).
   FIND FIRST feltemp WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE feltemp THEN DO:
      MESSAGE feltemp.FELTEXT VIEW-AS ALERT-BOX.
      DELETE PROCEDURE odbch NO-ERROR.
      avcadok = FALSE.
      RETURN.
   END.
   avcadok = TRUE.
   odbcvar = TRUE.
END PROCEDURE.
PROCEDURE avcad_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR avcadtemp.
   DEFINE VARIABLE htableS AS COM-HANDLE NO-UNDO.
   EMPTY TEMP-TABLE avcadtemp NO-ERROR.
   IF odbcvar = TRUE THEN DO:
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "ORDERS"
      sqltab.FALTANT = 1
      sqltab.FALT[1] = "Ordername"
      sqltab.FALT[2] = "Comment"
      sqltab.FALT[3] = "ID".
      kommando = "ORDERS". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE sqldat).
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "SURVEY"
      sqltab.FALTANT = 3
      sqltab.FALT[1] = "Idnumber"
      sqltab.FALT[2] = "OrderID"
      sqltab.FALT[3] = "SurveyName".
      kommando = "SURVEY".
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE sqldat APPEND).         
      FOR EACH sqldat WHERE sqldat.TABNAMN = "SURVEY":
         CREATE avcadtemp.
         ASSIGN
         avcadtemp.ID = INTEGER(sqldat.DATAFALT[2])
         avcadtemp.BENAMNING = "      --Avvägning  " + sqldat.DATAFALT[3]
         avcadtemp.AVVAGNING = INTEGER(sqldat.DATAFALT[1]).                         
      END.
      FOR EACH avcadtemp,
      EACH sqldat WHERE sqldat.TABNAMN = "ORDERS" AND sqldat.DATAFALT[3] = STRING(avcadtemp.ID):
         ASSIGN
         avcadtemp.BENAMNING = sqldat.DATAFALT[1] + avcadtemp.BENAMNING
         avcadtemp.COMMENT = sqldat.DATAFALT[2]. 
      END.                
      RETURN.
   END.
   IF VALID-HANDLE(hCurrdb) THEN DO:
      htable = hCurrdb:openrecordset("ORDERS",2,,).
      DO WHILE NOT htable:eof():
         idnum = htable:FIELDS("Id"):VALUE.
         /** Connect to SURVEY Table  **/               
         htableS = hCurrdb:openrecordset("SURVEY",2,,).
         com = "OrderID = " + STRING(idnum).
         htableS:findfirst(com).
         IF htableS:nomatch THEN DO:
            RELEASE OBJECT htableS NO-ERROR.
            htableS = ?.            
         END.          
         ELSE DO:
            REPEAT:                        
               CREATE avcadtemp.
               ASSIGN
               avcadtemp.ID = htable:FIELDS("Id"):VALUE
               avcadtemp.BENAMNING = htable:FIELDS("Ordername"):VALUE + "      --Avvägning  " + htableS:FIELDS("SurveyName"):VALUE
               avcadtemp.COMMENT = htable:FIELDS("Comment"):VALUE 
               avcadtemp.AVVAGNING = htableS:FIELDS("Idnumber"):VALUE.
               htableS:findnext(com).
               IF htableS:nomatch THEN DO:
                  RELEASE OBJECT htableS NO-ERROR.
                  htableS = ?.
                  LEAVE.
               END.                
            END.                          
         END.
         htable:movenext().   
      END.
      RELEASE OBJECT htable NO-ERROR.
      htable = ?.       
   END.
   ELSE DO:
      RUN close_UI.
   END.
   
END.

PROCEDURE ok_UI :
   DEFINE INPUT  PARAMETER valdminareain AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER idnum2 AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER avvagvar AS INTEGER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR beredningtemp.
   valdminarea = valdminareain. 
   idnum = idnum2.
   IF odbcvar = TRUE THEN DO:
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "ORDERS"
      sqltab.FALTANT = 1
      sqltab.FALT[1] = "Ordername"
      sqltab.FALT[2] = "Comment"
      sqltab.FALT[3] = "ID".
      kommando = "ORDERS WHERE ORDERS.ID = " + STRING(idnum).
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE sqldat).
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      FIND FIRST sqldat WHERE sqldat.TABNAMN = "ORDERS" NO-ERROR.
   
      FIND FIRST beredningtemp WHERE beredningtemp.BERNR = valbernr AND
      beredningtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
      IF AVAILABLE beredningtemp THEN DO:
         IF beredningtemp.BENAMNING = "" THEN DO:
            beredningtemp.BENAMNING = sqldat.DATAFALT[1].
         END.
      END.
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "SURVEY"
      sqltab.FALTANT = 3
      sqltab.FALT[1] = "Idnumber"
      sqltab.FALT[2] = "OrderID"
      sqltab.FALT[3] = "SurveyName".
      kommando = "SURVEY WHERE SURVEY.OrderID = " + STRING(idnum).
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE sqldat). 
      FOR EACH sqldat WHERE sqldat.TABNAMN = "SURVEY":
         surint = INTEGER(sqldat.DATAFALT[1]).
         IF avvagvar = surint THEN DO:
            RUN pole_UI.
            RUN strukt_UI.
            RUN segment_UI.     
         END.         
      END.
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "CABLE"
      sqltab.FALTANT = 3
      sqltab.FALT[1] = "Id"	
      sqltab.FALT[2] = "name"	
      sqltab.FALT[3] = "type".
      IF AVAILABLE tt_segment THEN DO:	
         str = tt_segment.LIN.
      END.   
      kommando = "CABLE". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE sqldat).
      FOR EACH sqldat WHERE sqldat.TABNAMN = "CABLE":
         CREATE tt_cab.
         ASSIGN
         tt_cab.CABID = INTEGER(sqldat.DATAFALT[1])
         tt_cab.CABNAME = sqldat.DATAFALT[2]
         tt_cab.CABTYPE = INTEGER(sqldat.DATAFALT[3]).
      END.
      /*
      FOR EACH tt_segment:
         RUN cab_UI.
      END.
      */
      RUN konstr_UI.
      RUN accapp_UI.
      RUN close_UI.
   END.
   IF VALID-HANDLE(hCurrdb) THEN DO:
       /*beredningshuvud*/
      htable = hCurrdb:openrecordset("ORDERS",2,,).
      com = "Id = " + STRING(idnum).
      htable:findfirst(com).
      IF htable:nomatch THEN DO:
         RELEASE OBJECT htable NO-ERROR.
         htable = ?.
         RETURN.
      END.
      ELSE DO:
         FIND FIRST beredningtemp WHERE beredningtemp.BERNR = valbernr AND
         beredningtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
         IF AVAILABLE beredningtemp THEN DO:
            IF beredningtemp.BENAMNING = "" THEN DO:
               beredningtemp.BENAMNING = htable:FIELDS("Ordername"):VALUE.
            END.
         END.
         RELEASE OBJECT htable NO-ERROR.
         htable = ?.
         
         /** Connect to SURVEY Table  **/               
         htable = hCurrdb:openrecordset("SURVEY",2,,).
         com = "OrderID = " + STRING(idnum).
         htable:findfirst(com).
         
         IF htable:nomatch THEN DO:
            RELEASE OBJECT htable NO-ERROR.
            htable = ?.
            RETURN.
         END.
         ELSE DO:
            REPEAT:
               ASSIGN
               surint = htable:FIELDS("Idnumber"):VALUE.
               IF avvagvar = surint THEN DO:
                  RUN pole_UI.
                  RUN strukt_UI.
                  RUN segment_UI.
               END.
               htable:findnext(com).
               IF htable:nomatch THEN DO:
                  RELEASE OBJECT htable NO-ERROR.
                  htable = ?.
                  LEAVE.
               END.                
            END.
            RELEASE OBJECT htable NO-ERROR.
            htable = ?.
            FOR EACH tt_segment:
               RUN cab_UI.
            END.
           
                        
                      /*
         tt_pole.POLECLAS = htable2:FIELDS("PoleClass"):VALUE.
         tt_pole.STOLPLENGTH = htable2:FIELDS("Totlength"):VALUE.
         */
            RUN konstr_UI.
           
         END.
         RUN accapp_UI.
      END.
      RUN close_UI.
   END.
END PROCEDURE.
PROCEDURE konstr_UI:  
DEBUGGER:SET-BREAK().
   DEFINE VARIABLE valdminareah AS DECIMAL NO-UNDO.
   counter = 1.
   FOR EACH tt_pole:
      CREATE kon_val.
      IF INDEX(tt_pole.POLECLAS,"+") > 0 THEN DO:
         IF tt_pole.POLECLAS BEGINS "N" THEN DO:
            kon_val.ANMARK = "Stolp dim:" + "G" + STRING(tt_pole.STOLPLENGTH).
         END. 
         ELSE IF tt_pole.POLECLAS BEGINS "G" THEN DO:
            kon_val.ANMARK = "Stolp dim:" + "E" + STRING(tt_pole.STOLPLENGTH).
         END.
         ELSE IF tt_pole.POLECLAS BEGINS "E" THEN DO:
            kon_val.ANMARK = "Stolp dim:" + "S" + STRING(tt_pole.STOLPLENGTH).
         END.    
         ELSE kon_val.ANMARK = "Stolp dim:" + tt_pole.POLECLAS + " " + STRING(tt_pole.STOLPLENGTH).
         IF INDEX(kon_val.ANMARK,"+1") > 0 THEN kon_val.ANMARK = REPLACE(kon_val.ANMARK,"+1","+2").
         IF INDEX(kon_val.ANMARK,"+ 1") > 0 THEN kon_val.ANMARK = REPLACE(kon_val.ANMARK,"+ 1","+ 2"). 
      END.   
      ELSE IF tt_pole.STOLPLENGTH > 9 THEN kon_val.ANMARK = "Stolp dim:" + tt_pole.POLECLAS + STRING(tt_pole.STOLPLENGTH).
      ELSE kon_val.ANMARK = "Stolp dim:" + tt_pole.POLECLAS + " " + STRING(tt_pole.STOLPLENGTH).
      kon_val.F4 = STRING(tt_pole.DJUP) + "$".
      FIND FIRST tt_polecomp WHERE tt_polecomp.POLEID = tt_pole.POLEID NO-LOCK NO-ERROR.
      IF AVAILABLE tt_polecomp THEN DO:
         valdminareah = 0.
         IF tt_polecomp.Earea > 0 OR tt_polecomp.Varea > 0 THEN DO:
            IF tt_polecomp.Earea > 0 THEN valdminareah = tt_polecomp.Earea.
            ELSE IF tt_polecomp.Varea > 0 THEN valdminareah = tt_polecomp.Varea.
            IF valdminareah < valdminarea THEN valdminareah = valdminarea.
            kon_val.ANMARK = kon_val.ANMARK + " Stag area:" + STRING(ROUND(valdminareah,0)).
            kon_val.F4 = kon_val.F4 + STRING(ROUND(valdminareah,0)).
         END.            
      END.
      
      ASSIGN
      kon_val.ID2 = STRING(tt_pole.POLENAMN)  /*POLENAMN ?*/
      kon_val.BERAONR   = valaonr   
      kon_val.OMRADE    = valomrade 
      kon_val.F1        = tt_pole.STRUID    
      kon_val.NUM       = counter
      kon_val.ORD       =  counter.   
      CREATE berpunkttemp.
      ASSIGN
      berpunkttemp.AONR                = valaonr   
      berpunkttemp.OMRADE              = valomrade 
      berpunkttemp.KONSTRUKTIONUPPLAG  = TRUE   
      berpunkttemp.NUM                 = counter
      berpunkttemp.NERGRAVDJUP         = tt_pole.DJUP
      berpunkttemp.XKORDCH             = STRING(tt_pole.XCORD)
      berpunkttemp.YKORDCH             = STRING(tt_pole.YCORD)
      berpunkttemp.ZKORDCH             = STRING(tt_pole.ZCORD).  
      
/* cccv     HDPUNKT  */               
      FIND FIRST tt_segment WHERE tt_segment.SURID = tt_pole.SURID AND
      tt_segment.FR <= tt_pole.POLNR AND tt_segment.TI + 1 >= tt_pole.POLNR  
      NO-LOCK NO-ERROR.
      IF AVAILABLE tt_segment THEN DO:
         kon_val.F2        = tt_segment.LIN + "$" + tt_segment.AREA.
         /* För area sök:
         om Feal sök på area.
         annar ta de trE första bokstäverna och sedan area*/                                               
      END.
       
      counter = counter + 1.
   END.
END PROCEDURE.
PROCEDURE accapp_UI:
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN AVCADACCESSAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT valaonr,INPUT valomrade,INPUT valdelnr,INPUT Guru.Konstanter:globanv,INPUT TABLE kon_val,INPUT TABLE berpunkttemp).
   END.
   ELSE DO:
      RUN AVCADACCESSAPP.P 
      (INPUT valaonr,INPUT valomrade,INPUT valdelnr,INPUT Guru.Konstanter:globanv,INPUT TABLE kon_val,INPUT TABLE berpunkttemp).
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN IMPBERKALKSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:varforetypchar[48], INPUT valaonr,INPUT valomrade).
   END.
   ELSE DO:
      RUN IMPBERKALKSKAP.P (INPUT Guru.Konstanter:varforetypchar[48],INPUT valaonr,INPUT valomrade).
   END.
END PROCEDURE.
PROCEDURE pole_UI:
   /** Connect to POLE Table  **/
   DEBUGGER:SET-BREAK().
   IF odbcvar = TRUE THEN DO:
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "POLE"
      sqltab.FALTANT = 42
      sqltab.FALT[1] = "Id"	
      sqltab.FALT[2] = "SurveyId"	
      sqltab.FALT[3] = "PoleName"	
      sqltab.FALT[4] = "StructId"	
      sqltab.FALT[5] = "Totlength"	
      sqltab.FALT[6] = "Depth"	
      sqltab.FALT[7] = "Kors"	
      sqltab.FALT[8] = "GroundPole"	
      sqltab.FALT[9] = "TWeight"	
      sqltab.FALT[10] = "THeight"	
      sqltab.FALT[11] = "FskWeight"	
      sqltab.FALT[12] = "FskHeight"	
      sqltab.FALT[13] = "OWeight"	
      sqltab.FALT[14] = "Pk"	
      sqltab.FALT[15] = "Momentum"	
      sqltab.FALT[16] = "D2Break"	
      sqltab.FALT[17] = "PoleClass"	
      sqltab.FALT[18] = "StayDirection"	
      sqltab.FALT[19] = "ResForce"	
      sqltab.FALT[20] = "SteelStead"	
      sqltab.FALT[21] = "TSteadAngle"	
      sqltab.FALT[22] = "TSteadDepth"	
      sqltab.FALT[23] = "TSteadLevel"	
      sqltab.FALT[24] = "TSteadDist"	
      sqltab.FALT[25] = "TSteadLength"	
      sqltab.FALT[26] = "TSteadClass"	
      sqltab.FALT[27] = "D2Moment"	
      sqltab.FALT[28] = "FlagByte"	
      sqltab.FALT[29] = "PoleNr"	
      sqltab.FALT[30] = "Direction"	
      sqltab.FALT[31] = "X"	
      sqltab.FALT[32] = "geoX"	
      sqltab.FALT[33] = "Y"	
      sqltab.FALT[34] = "S"	
      sqltab.FALT[35] = "Z"	
      sqltab.FALT[36] = "D"	
      sqltab.FALT[37] = "Comment"	
      sqltab.FALT[38] = "Codes"	
      sqltab.FALT[39] = "CalcClass"	
      sqltab.FALT[40] = "DecayDamage"	
      sqltab.FALT[41] = "Text"	
      sqltab.FALT[42] = "TextID".
      
/*
Depth Float Nedgrävningsdjup
sqltab.FALT[6] = "Depth"
tt_pole.DJUP =  htable2:FIELDS("Depth"):VALUE

geoX  Float Stolpens geografiska X-koordinat
sqltab.FALT[32] = "geoX"  
tt_pole.XCORD  = htable2:FIELDS("geoX"):VALUE

Y  Float Stolpens geografiska Y-koordinat
sqltab.FALT[33] = "Y"
tt_pole.YCORD  = htable2:FIELDS("Y"):VALUE

Z  Float Stolpens geografiska höjdläge
sqltab.FALT[35] = "Z"
tt_pole.ZCORD  = htable2:FIELDS("Z"):VALUE
*/


      kommando = "POLE WHERE POLE.SurveyId = " + STRING(surint).
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat). 
      FIND FIRST esqldat NO-ERROR.
      IF NOT AVAILABLE esqldat THEN DO:
         BELL.
         MESSAGE "Det finns inga stolpar!" VIEW-AS ALERT-BOX ERROR.
      END.
      FOR EACH esqldat WHERE esqldat.TABNAMN = "POLE":
         CREATE tt_pole.
         ASSIGN
         tt_pole.SURID = INTEGER(esqldat.DATAFALT[2])
         tt_pole.POLENAMN = esqldat.DATAFALT[3]
         tt_pole.POLEID = INTEGER(esqldat.DATAFALT[1])
         tt_pole.STRUID = esqldat.DATAFALT[4]
         tt_pole.TEXID = esqldat.DATAFALT[42]
         tt_pole.POLECLAS = esqldat.DATAFALT[17]
         tt_pole.POLNR = INTEGER(esqldat.DATAFALT[29])
         tt_pole.DJUP = DECIMAL(esqldat.DATAFALT[6])
         tt_pole.XCORD  = DECIMAL(esqldat.DATAFALT[32])
         tt_pole.YCORD  = DECIMAL(esqldat.DATAFALT[33])
         tt_pole.ZCORD  = DECIMAL(esqldat.DATAFALT[35])
         tt_pole.STOLPLENGTH = INTEGER(esqldat.DATAFALT[5]).
      END.
      EMPTY TEMP-TABLE esqldat NO-ERROR.
      
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "POLECOMP"
      sqltab.FALTANT = 21
      sqltab.FALT[1] =  "PoleId"    
      sqltab.FALT[2] =  "CrossarmNr"
      sqltab.FALT[3] =  "NrRStay"   
      sqltab.FALT[4] =  "RSpread"   
      sqltab.FALT[5] =  "ROutStr"   
      sqltab.FALT[6] =  "rarea"     
      sqltab.FALT[7] =  "rmaxload"  
      sqltab.FALT[8] =  "nrestay"     
      sqltab.FALT[9] =  "espread"   
      sqltab.FALT[10] = "eoutstr"   
      sqltab.FALT[11] = "earea"     
      sqltab.FALT[12] = "emaxload"  
      sqltab.FALT[13] = "nrvstay"   
      sqltab.FALT[14] = "vspread"   
      sqltab.FALT[15] = "voutstr"   
      sqltab.FALT[16] = "varea"     
      sqltab.FALT[17] = "vmaxload"  
      sqltab.FALT[18] = "jord"   
      sqltab.FALT[19] = "angle"     
      sqltab.FALT[20] = "Id"          
      sqltab.FALT[21] = "VDIR". 
      FOR EACH tt_pole WHERE NO-LOCK:
          kommando = "POLECOMP WHERE POLECOMP.PoleId = " + STRING(tt_pole.POLEID).
          RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat APPEND).
      END.      
      
      FIND FIRST esqldat NO-ERROR.
      IF NOT AVAILABLE esqldat THEN DO:
         BELL.
         MESSAGE "Det finns inga stolpar!" VIEW-AS ALERT-BOX ERROR.
      END.
      FOR EACH esqldat WHERE esqldat.TABNAMN = "POLECOMP":
         CREATE tt_polecomp.
         ASSIGN
         tt_polecomp.POLEID = INTEGER(esqldat.DATAFALT[1])
         tt_polecomp.Earea = DECIMAL(DATAFALT[11])
         tt_polecomp.Varea = DECIMAL(DATAFALT[16]).
         
      END.
      EMPTY TEMP-TABLE esqldat NO-ERROR.
      RETURN.
   END.
   htable2 = hCurrdb:openrecordset("POLE",2,,).
   com2 = "SurveyId = " + STRING(surint).
   htable2:findfirst(com2).
   IF htable2:nomatch THEN DO:
      BELL.
      MESSAGE "Det finns inga stolpar!" VIEW-AS ALERT-BOX ERROR.
      RELEASE OBJECT htable2 NO-ERROR.
      htable2 = ?.
      RETURN.
   END.
   ELSE DO:
      REPEAT:
         CREATE tt_pole.
         ASSIGN
         tt_pole.SURID = htable2:FIELDS("SurveyId"):VALUE
         tt_pole.POLENAMN = htable2:FIELDS("PoleName"):VALUE
         tt_pole.POLEID = htable2:FIELDS("Id"):VALUE
         tt_pole.STRUID = htable2:FIELDS("StructId"):VALUE
         tt_pole.TEXID = htable2:FIELDS("TextID"):VALUE
         tt_pole.POLECLAS = htable2:FIELDS("PoleClass"):VALUE
         tt_pole.POLNR = htable2:FIELDS("PoleNR"):VALUE
         tt_pole.DJUP =  htable2:FIELDS("Depth"):VALUE
         tt_pole.XCORD  = htable2:FIELDS("geoX"):VALUE
         tt_pole.YCORD  = htable2:FIELDS("Y"):VALUE
         tt_pole.ZCORD  = htable2:FIELDS("Z"):VALUE
         tt_pole.STOLPLENGTH = htable2:FIELDS("Totlength"):VALUE.
         htable2:findnext(com2).
         IF htable2:nomatch THEN DO:
            RELEASE OBJECT htable2 NO-ERROR.
            htable2 = ?.
            LEAVE.
         END.          
      END.      
   END.
   htable2 = hCurrdb:openrecordset("POLECOMP",2,,).
   
   FOR EACH tt_pole NO-LOCK:
       com2 = "PoleId = " + STRING(tt_pole.POLEID).
      htable2:findfirst(com2).
      IF htable2:nomatch THEN DO:
      
      END.
      ELSE DO:     
         CREATE tt_polecomp.
         ASSIGN
         tt_polecomp.POLEID = htable2:FIELDS("POLEID"):VALUE
         tt_polecomp.Earea = htable2:FIELDS("Earea"):VALUE
         tt_polecomp.Varea = htable2:FIELDS("Varea"):VALUE.                  
      END.      
   END.
   RELEASE OBJECT htable2 NO-ERROR.
   htable2 = ?.
END PROCEDURE.

PROCEDURE strukt_UI:
   /** Connect to STRUCT Table  **/  
   IF odbcvar = TRUE THEN DO:
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "STRUCT"
      sqltab.FALTANT = 5
      sqltab.FALT[1] = "Id"	
      sqltab.FALT[2] = "namn"	
      sqltab.FALT[3] = "type"	
      sqltab.FALT[4] = "nrpoles"	
      sqltab.FALT[5] = "class".
      kommando = "STRUCT".
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE sqldat). 
      /*
      FIND FIRST sqldat WHERE sqldat.TABNAMN = "STRUCT" NO-ERROR.
      */
      FOR EACH tt_pole,
      EACH sqldat WHERE sqldat.TABNAMN = "STRUCT" AND sqldat.DATAFALT[2] = tt_pole.STRUID:
         str = tt_pole.STRUID.
         /*
         kommando = "STRUCT WHERE STRUCT.NAMN = " + "'" + str + "'".
         RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE sqldat). 
         FIND FIRST sqldat WHERE sqldat.TABNAMN = "STRUCT" NO-ERROR.
         */
         IF AVAILABLE sqldat THEN DO:
            CREATE tt_struct.
            ASSIGN
            tt_struct.STRUID = INTEGER(sqldat.DATAFALT[1])
            tt_struct.STRUNAMN = sqldat.DATAFALT[2]
            tt_struct.STRUTYPE = INTEGER(sqldat.DATAFALT[3])
            tt_struct.STRUCLASS = INTEGER(sqldat.DATAFALT[3]).
            IF tt_struct.STRUTYPE = 0 THEN tt_struct.STOLPTYP = "Raklinjestolpe".
            ELSE IF tt_struct.STRUTYPE = 1 THEN tt_struct.STOLPTYP = "Vinkelstolpe".     
            ELSE IF tt_struct.STRUTYPE = 2 THEN tt_struct.STOLPTYP = "Avgreningsstolpe". 
            ELSE IF tt_struct.STRUTYPE = 3 THEN tt_struct.STOLPTYP = "Ändstolpe".        
            ELSE IF tt_struct.STRUTYPE = 4 THEN tt_struct.STOLPTYP = "Avgreningsstolpe".                        
         END.
      END.
      RETURN.
   END.
   htable2 = hCurrdb:openrecordset("STRUCT",2,,).
   FOR EACH tt_pole:
      str = tt_pole.STRUID.
      com2 = "namn = " + "'" + str + "'".
      htable2:findfirst(com2).
      IF htable2:nomatch THEN DO:
         RELEASE OBJECT htable2 NO-ERROR.
         htable2 = ?.
         RETURN.
      END.
      ELSE DO:
         CREATE tt_struct.
         ASSIGN
         tt_struct.STRUID = htable2:FIELDS("Id"):VALUE
         tt_struct.STRUNAMN = htable2:FIELDS("namn"):VALUE
         tt_struct.STRUTYPE = htable2:FIELDS("type"):VALUE
         tt_struct.STRUCLASS = htable2:FIELDS("class"):VALUE.
         IF tt_struct.STRUTYPE = 0 THEN tt_struct.STOLPTYP = "Raklinjestolpe".
         ELSE IF tt_struct.STRUTYPE = 1 THEN tt_struct.STOLPTYP = "Vinkelstolpe".     
         ELSE IF tt_struct.STRUTYPE = 2 THEN tt_struct.STOLPTYP = "Avgreningsstolpe". 
         ELSE IF tt_struct.STRUTYPE = 3 THEN tt_struct.STOLPTYP = "Ändstolpe".        
         ELSE IF tt_struct.STRUTYPE = 4 THEN tt_struct.STOLPTYP = "Avgreningsstolpe".                        
      END.
   END.
   RELEASE OBJECT htable2 NO-ERROR.
   htable2 = ?.

END PROCEDURE.

PROCEDURE segment_UI:
   /** Connect to SEGMENTS Table  **/  
   IF odbcvar = TRUE THEN DO:
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "SEGMENTS"
      sqltab.FALTANT = 15
      sqltab.FALT[1] = "Id"	
      sqltab.FALT[2] = "SurveyId"	
      sqltab.FALT[3] = "OldId"	
      sqltab.FALT[4] = "CableId"	
      sqltab.FALT[5] = "Fran"	
      sqltab.FALT[6] = "Till"	
      sqltab.FALT[7] = "Fsek"	
      sqltab.FALT[8] = "Tsek"	
      sqltab.FALT[9] = "Mall1"	
      sqltab.FALT[10] = "Mall2"	
      sqltab.FALT[11] = "Mall3"	
      sqltab.FALT[12] = "Mall4"	
      sqltab.FALT[13] = "Linnr"	
      sqltab.FALT[14] = "Lina"	
      sqltab.FALT[15] = "Area".	
      kommando = "SEGMENTS WHERE SEGMENTS.SurveyId = " + STRING(surint).
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE sqldat).
      FIND FIRST sqldat WHERE sqldat.TABNAMN = "SEGMENTS" NO-ERROR.
      IF AVAILABLE sqldat THEN DO:
         FOR EACH sqldat WHERE sqldat.TABNAMN = "SEGMENTS":
            CREATE tt_segment.
            ASSIGN
            tt_segment.SURID    = INTEGER(sqldat.DATAFALT[2])
            tt_segment.CABID    = INTEGER(sqldat.DATAFALT[4]) 
            tt_segment.FR       = INTEGER(sqldat.DATAFALT[5])      /*Första stolpen*/     
            tt_segment.TI       = INTEGER(sqldat.DATAFALT[6])      /*Sista stolpen*/ 
            tt_segment.LIN      = sqldat.DATAFALT[14]     
            tt_segment.SEGTEXT  = ""      
            tt_segment.AREA     = sqldat.DATAFALT[15].
         END.
      END.
      ELSE DO:
         BELL.
         MESSAGE "Det finns inga linor!" VIEW-AS ALERT-BOX ERROR.
      END.
      RETURN.
   END.
   htable2 = hCurrdb:openrecordset("SEGMENTS",2,,).
   com2 = "SurveyId = " + STRING(surint).
   htable2:findfirst(com2).
   IF htable2:nomatch THEN DO:
      BELL.
      MESSAGE "Det finns inga linor!" VIEW-AS ALERT-BOX ERROR.
      RELEASE OBJECT htable2 NO-ERROR.
      htable2 = ?.
      RETURN.
   END.
   ELSE DO:
      REPEAT:
         CREATE tt_segment.
         ASSIGN
         tt_segment.SURID    = htable2:FIELDS("SurveyId"):VALUE
         tt_segment.CABID    = htable2:FIELDS("CableId"):VALUE 
         tt_segment.FR       = htable2:FIELDS("Fran"):VALUE      /*Första stolpen*/     
         tt_segment.TI       = htable2:FIELDS("Till"):VALUE      /*Sista stolpen*/ 
         tt_segment.LIN      = htable2:FIELDS("Lina"):VALUE      
         tt_segment.SEGTEXT  = htable2:FIELDS("Text"):VALUE      
         tt_segment.AREA     = htable2:FIELDS("Area"):VALUE.
         htable2:findnext(com2).
         IF htable2:nomatch THEN DO:
            RELEASE OBJECT htable2 NO-ERROR.
            htable2 = ?.
            LEAVE.
         END.          
      END.      
   END.
   RELEASE OBJECT htable2 NO-ERROR.
   htable2 = ?.

END PROCEDURE.

PROCEDURE cab_UI:
   DEFINE VARIABLE str AS CHARACTER NO-UNDO.
   /** Connect to CABLE Table  **/
   IF odbcvar = TRUE THEN DO:
      IF odbcvar = odbcvar THEN RETURN.
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "CABLE"
      sqltab.FALTANT = 3
      sqltab.FALT[1] = "Id"	
      sqltab.FALT[2] = "name"	
      sqltab.FALT[3] = "type".	
      str = tt_segment.LIN.
      kommando = "CABLE WHERE CABLE.name = " + "'" + str + "'". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE sqldat).
      FIND FIRST sqldat WHERE sqldat.TABNAMN = "CABLE" NO-ERROR.
      IF AVAILABLE sqldat THEN DO:
         CREATE tt_cab.
         ASSIGN
         tt_cab.CABID = htable2:FIELDS("Id"):VALUE.
         tt_cab.CABNAME = htable2:FIELDS("name"):VALUE.
         tt_cab.CABTYPE = htable2:FIELDS("type"):VALUE.
      END.
      RETURN.
   END.
   htable2 = hCurrdb:openrecordset("CABLE",2,,).
   IF AVAILABLE tt_segment THEN DO:
      
      str = tt_segment.LIN.
      com2 = "name = " + "'" + str + "'".
      htable2:findfirst(com2).
      IF htable2:nomatch THEN DO:
         BELL.
         MESSAGE "No structs found" VIEW-AS ALERT-BOX ERROR.
         RELEASE OBJECT htable2 NO-ERROR.
         htable2 = ?.
         RETURN.
      END.
      ELSE DO:
         REPEAT:
            CREATE tt_cab.
            ASSIGN
            tt_cab.CABID = htable2:FIELDS("Id"):VALUE.
            tt_cab.CABNAME = htable2:FIELDS("name"):VALUE.
            tt_cab.CABTYPE = htable2:FIELDS("type"):VALUE.
            htable2:findnext(com2).
            IF htable2:nomatch THEN DO:
               RELEASE OBJECT htable2 NO-ERROR.
               htable2 = ?.
               LEAVE.
            END.          
         END.      
      END.
   END.
   RELEASE OBJECT htable2 NO-ERROR.
   htable2 = ?.

END PROCEDURE.


PROCEDURE close_UI :
   IF odbcvar = TRUE THEN DO:
      IF VALID-HANDLE(odbch) THEN RUN avslut_UI IN odbch.
      DELETE PROCEDURE odbch NO-ERROR.
      RELEASE OBJECT hCurrdb NO-ERROR.
      odbcvar = FALSE.
      RETURN.
   END.
   IF VALID-HANDLE(hAccess) THEN DO:
      hAccess:CloseCurrentDatabase().
      RELEASE OBJECT hfield NO-ERROR.
      hfield = ?.
      RELEASE OBJECT htable NO-ERROR.
      htable = ?.
      RELEASE OBJECT htable2 NO-ERROR.
      htable2 = ?.
      RELEASE OBJECT hCurrdb NO-ERROR.
      hCurrdb = ?.
      hAccess:QUIT(2).
      RELEASE OBJECT hAccess NO-ERROR.
      hAccess = ?.
   END.
   
END PROCEDURE.

 /*
ORDER
OrderName	Comment	Id	CreatedCad
NT7621 L02	X       109	-1
SURVEY 
Idnumber OrderId SurveyName	                Company	         Signature Datum      Voltage FlagByte Comment Range Dim3d Distribution Fork Line	       Workorder Locked	CreatedCad Hskala Lskala DXF
201      109	 Avg Vojmsjödammen 20081219	PICAB Produktion PN	       2007-11-01 24	  2	        _	   0	 0	   _	        _	 Vojmsjödammen _	     0	    -1	       500	  2000	

POLE

Id	  SurveyId PoleName	StructId Totlength Depth Kors GroundPole TWeight THeight FskWeight FskHeight OWeight Pk	   Momentum	D2Break	PoleClass StayDirection	ResForce SteelStead	TSteadAngle	TSteadDepth	TSteadLevel	TSteadDist TSteadLength	TSteadClass	D2Moment FlagByte PoleNr Direction X       geoX       Y          S       Z                D   Comment Codes CalcClass DecayDamage Text TextID
24643 201 	   1        HKÄ 21   11        2     0   -1          0       0       0         0         0       52071 4083     237     N         0             0        0          0           0           0           0          0                        229      0        1      0         0       1544242,51 7197523,84 0       428,54           0   X       S1R   4         0           -1   39612284337                                                                                                                                                                   
24644 201      2        HKR 21   13        2     0   -1          0       0       0         0         0       7197  20250    237     N         0             0        0          0           0           0           0          0                        191      0        2      0         87,15   1544172,97 7197471,32 87,15   429,38601537476  0   X       S1R   4         0           -1   39612284512                                                                                                                                  
24645 201      3        HKR 21   11        2     0   -1          0       0       0         0         0       6239  15561    204     N         0             0        0          0           0           0           0          0                        174      0        3      0         162,29  1544113,01 7197426,02 162,29  429,47481545961  0   X       S1R   4         0           -1   39612285276                                                                                                                                  
24646 201      4        HKR 21   11        2     0   -1          0       0       0         0         0       6740  14745    207     N         0             0        0          0           0           0           0          0                        171      -1       4      0         246,43  1544045,87 7197375,31 246,43  431,122401143402 0   X       S1R   4         0           -1   39612316600                                                                                                                          
24647 201      5        HKV 21   12        2     0   -1          0       0       0         0         0       22312 5436     216     N+1       346,5         0        0          0           0           0           0          0                        253      10       5      107       309,39  1544219,35 7197215,32 309,39  431,21           107 X       S1R   4         0           -1   39612285398                                                                                                                                      
24648 201      6        HKV 21   13        2     0   -1          0       0       0         0         0       8156  833      243     N         305,5         0        0          0           0           0           0          0                        190      10       6      68        353     1544216,08 7197171,83 353     429,67           68  X       S1R   4         0           -1   39612285450                                                                                                                                              
24649 201      7        HKR 21   12        2     0   -1          0       0       0         0         0       7369  18976    225     N         0             0        0          0           0           0           0          0                        187      -1       7      0         441,69  1544209,45 7197083,39 441,69  428,807100591716 0   X       S1R   4         0           -1   39612285569                                                                                                                          
24650 201      8        HKR 21   11        2     0   -1          0       0       0         0         0       6802  16513    208     N         0             0        0          0           0           0           0          0                        178      -1       8      0         529     1544202,91 7196996,33 529     427,68625        0   X       S1R   4         0           -1   39612316856                                                                                                                                          
24651 201      9        HKR 21   12        2     0   -1          0       0       0         0         0       7621  19277    226     N         0             0        0          0           0           0           0          0                        188      0        9      0         616,31  1544112,08 7197054,02 616,31  428,1498125      0   X       S1R   4         0           -1   39612286451                                                                                                                                  
24652 201      10       HKR 21   12        2     0   -1          0       0       0         0         0       6299  17620    218     N         0             0        0          0           0           0           0          0                        182      0        10     0         709,45  1544145,32 7196967,02 709,45  427,574285714286 0   X       S1R   4         0           -1   39612286645                                                                                                                          
24653 201      11       HKR 21   11        2     0   -1          0       0       0         0         0       6115  13521    203     N         0             0        0          0           0           0           0          0                        166      0        11     0         774,49  1544168,53 7196906,26 774,49  428,626991869919 0   X       S1R   4         0           -1   39612287073                                                                                                                          
24654 201      12       HKV 21   10        2     0   -1          0       0       0         0         0       14016 3176     225     N         325           0        0          0           0           0           0          0                        207      -1       12     50        838,52  1544191,38 7196846,44 838,52  428,51852739726  50  X       S1R   4         0           -1   39612287217                                                                                                                              
24655 201      13       HKR 21   10        2     0   -1          0       0       0         0         0       3572  9813     170     N         0             0        0          0           0           0           0          0                        149      0        13     0         887,73  1544235,95 7196826,06 887,73  429,49778597786  0   X       S1R   4         0           -1   39612287305                                                                                                                              
24656 201      14       HKR 21   10        2     0   -1          0       0       0         0         0       7526  11877    198     N         0             0        0          0           0           0           0          0                        159      0        14     0         939,06  1544282,84 7196805,16 939,06  433,340646387833 0   X       S1R   4         0           -1   39612287420                                                                                                                          
24657 201      15       HKR 21   13        2     0   -1          0       0       0         0         0       8211  21242    244     N         0             0        0          0           0           0           0          0                        194      0        15     0         1022,14 1544358,72 7196771,34 1022,14 432,430802919708 0   X       S1R   4         0           -1   39612287827                                                                                                                          
24658 201      16       HKV 21   12        2     0   -1          0       0       0         0         0       11999 2161     248     N         316           0        0          0           0           0           0          0                        212      0        16     32        1112,1  1544440,89 7196734,72 1112,1  427,94037037037  32  X       S1R   4         0           -1   39612287978                                                                                                                              
24659 201      17       HKV 21   12        2     0   -1          0       0       0         0         0       8889  383      234     N         301,5         0        0          0           0           0           0          0                        161      0        17     3         1183,54 1544484,52 7196678,23 1183,54 428,91785202864  3   X       S1R   4         0           -1   39612288258                                                                                                                              
24660 201      18       HKV 21   12        2     0   -1          0       0       0         0         0       9796  459      238     N         302           0        0          0           0           0           0          0                        165      -1       18     4         1290,96 1544546,02 7196590,16 1290,96 429,47           4   X       S1R   4         0           -1   39612288376                                                                                                                                      
24661 201      19       HKV 21   11        2     0   -1          0       0       0         0         0       9543  1423     222     N         310           0        0          0           0           0           0          0                        187      0        19     20        1372,98 1544597,48 7196526,29 1372,98 427,821290322581 20  X       S1R   4         0           -1   39612288593                                                                                                                          
24662 201      20       HKV 21   13        2     0   -1          0       0       0         0         0       9317  580      250     N         303           0        0          0           0           0           0          0                        181      0        20     6         1451,82 1544626,04 7196452,82 1451,82 431,80223880597  6   X       S1R   4         0           -1   39612288681                                                                                                                              
24663 201      21       HKR 21   12        2     0   -1          0       0       0         0         0       7621  17620    226     N         0             0        0          0           0           0           0          0                        182      0        21     0         1559,77 1544655,28 7196348,91 1559,77 439,099523809524 0   X       S1R   4         0           -1   39612289629                                                                                                                          
24664 201      22       HKR 21   12        2     0   -1          0       0       0         0         0       6929  16717    222     N         0             0        0          0           0           0           0          0                        179      0        22     0         1609,52 1544668,73 7196301,02 1609,52 440,433806734993 0   X       S1R   4         0           -1   39612289898                                                                                                                          
24665 201      23       HKR 21   12        2     0   -1          0       0       0         0         0       7746  19428    227     N         0             0        0          0           0           0           0          0                        188      0        23     0         1704,95 1544694,54 7196209,14 1704,95 440,358986975398 0   X       S1R   4         0           -1   39612292431                                                                                                                          
24666 201      24       HKR 21   11        2     0   -1          0       0       0         0         0       7177  15697    210     N         0             0        0          0           0           0           0          0                        175      0        24     0         1792,26 1544718,15 7196125,08 1792,26 439,142709883103 0   X       S1R   4         0           -1   39612293753                                                                                                                          
24667 201      25       HKR 21   10        2     0   -1          0       0       0         0         0       6660  12969    193     N         0             0        0          0           0           0           0          0                        164      0        25     0         1867,93 1544738,61 7196052,23 1867,93 436,366666666667 0   X       S1R   4         0           -1   39612293874                                                                                                                          
24668 201      26       HKV 21   10        2     0   -1          0       0       0         0         0       20839 3538     246     G         328           0        0          0           0           0           0          0                        212      -1       26     56        1944,66 1544759,36 7195978,36 1944,66 432,579269746647 56  X       S1R   4         0           -1   39612293961                                                                                                                          
24669 201      27       HKV 21   12        2     0   -1          0       0       0         0         0       12330 1897     250     N+1       314,5         0        0          0           0           0           0          0                        207      0        27     29        1998,64 1544728,9  7195933,92 1998,64 430,524303405573 29  X       S1R   4         0           -1   39612297222                                                                                                                          
24670 201      28       HKR 21   12        2     0   -1          0       0       0         0         0       3119  10842    191     N         0             0        0          0           0           0           0          0                        154      0        28     0         2033,03 1544698,94 7195917,04 2033,03 430,371890080429 0   X       S1R   4         0           -1   39612297307                                                                                                                          
24671 201      29       HKV 21   10        2     0   -1          0       0       0         0         0       8777  860      204     N         306           0        0          0           0           0           0          0                        161      0        29     12        2066,85 1544669,48 7195900,43 2066,85 431,626037735849 12  X       S1R   4         0           -1   39612299056                                                                                                                          
24672 201      30       HKV 21   10        2     0   -1          0       0       0         0         0       11041 900      214     N         306           0        0          0           0           0           0          0                        162      0        30     12        2132,56 1544618,9  7195858,49 2132,56 433,743602941176 12  X       S1R   4         0           -1   39612299464                                                                                                                          
24673 201      31       HKV 21   11        2     0   -1          0       0       0         0         0       9125  482      220     N         302,5         0        0          0           0           0           0          0                        156      0        31     5         2203,47 1544574,18 7195803,47 2203,47 432,320935412027 5   X       S1R   4         0           -1   39612299557                                                                                                                          
24674 201      32       HKR 21   11        2     0   -1          0       0       0         0         0       5487  13521    199     N         0             0        0          0           0           0           0          0                        166      0        32     0         2287,08 1544526,52 7195734,77 2287,08 429,620407470289 0   X       S1R   4         0           -1   39612299688                                                                                                                          
24675 201      33       HKV 21   11        2     0   -1          0       0       0         0         0       14137 3236     242     N+1       325,5         0        0          0           0           0           0          0                        218      -1       33     51        2334,17 1544499,69 7195696,07 2334,17 428,587064220183 51  X       S1R   4         0           -1   39612299786                                                                                                                          
24676 201      34       HKÄ 21   11        2     0   -1          0       0       0         0         0       35902 4064     221     N         0             0        0          0           0           0           0          0                        229      -1       34     0         2404,02 0          0          2404,02 429,699609756098 0   X       S1R   4         0           -1   39612299869                                                                                                                                          

STRUCT
Id	 namn         type	  nrpoles class voltage angstay endstay rockstay smaxarea gc2pt	            OldStructure Picture
1429 HKA 1        2       1       5     1       1       2       3        52       1,70000004768372  0            h18                  
1430 HKA 11       2       1       5     1       0       2       2        52       0,300000011920929 0            h18    
1431 HKA 12       2       1       5     1       0       2       1        52       0,300000011920929 0            h19    
1432 HKA 2        2       1       5     1       0       2       3        68       1,70000004768372  0            h17    
1433 HKA 21       2       1       5     1       0       2       2        52       0,300000011920929 0            h18    
1434 HKA 22       2       1       5     1       0       2       1        52       0,300000011920929 0            h19    
1435 HKR 1        0       1       5     1       0       0       3        52       0,300000011920929 0            h01    
1436 HKR 1/KK-0.3 0       1       5     1       0       0       3        52       0,300000011920929 0            s01    
1437 HKR 11       0       1       5     1       0       0       3        52       0,300000011920929 0            h03    
1438 HKR 12       0       1       5     1       0       0       3        52       0,300000011920929 0            h04    
1439 HKR 21       0       1       5     1       0       0       3        52       0,300000011920929 0            h03    
1440 HKR 22       0       1       5     1       0       0       3        52       0,300000011920929 0            h04    
1441 HKV 1/KK-0.3 1       1       5     1       2       0       3        52       0,300000011920929 0            s02    
1442 HKV 11       1       1       5     1       1       0       3        52       0,300000011920929 0            h09    
1443 HKV 12       1       1       5     1       2       0       3        52       0,300000011920929 0            h10    
1444 HKV 13       1       1       5     1       2       0       3        52       0,300000011920929 0            h11    
1445 HKV 14       1       1       5     1       2       0       3        52       0,300000011920929 0            h12    
1446 HKV 2        1       1       5     1       2       0       3        68       1,70000004768372  0            h06    
1447 HKV 21       1       1       5     1       1       0       3        52       0,300000011920929 0            h09    
1448 HKV 22       1       1       5     1       2       0       3        52       0,300000011920929 0            h10    
1449 HKV 23       1       1       5     1       2       0       3        52       0,300000011920929 0            h11    
1450 HKV 24       1       1       5     1       2       0       3        52       0,300000011920929 0            h12    
1451 HKV 4-0.9    1       1       5     1       2       0       3        68       2,40000009536743  0            h08    
1452 HKÄ 1        3       1       5     1       0       2       2        68       1,5               0            h13                           
1453 HKÄ 1/KK-0.3 3       1       5     1       0       2       2        68       1,70000004768372  0            s03    
1454 HKÄ 11       3       1       5     1       0       2       2        52       0,300000011920929 0            h14    
1455 HKÄ 12       3       1       5     1       0       2       2        52       0,300000011920929 0            h15    
1456 HKÄ 2        3       1       5     1       0       2       2        68       1,70000004768372  0            h20    
1457 HKÄ 21       3       1       5     1       0       2       2        52       0,300000011920929 0            h14    
1458 HKÄ 22       3       1       5     1       0       2       2        52       0,300000011920929 0            h15    

SEGMENTS
Id	SurveyId OldId CableId Fran	Till Fsek Tsek Mall1 Mall2 Mall3 Mall4 Linnr Lina	    Area Diam qe   uppsp eik   ep	 tp	    V	     klass Mallnr1 Mallnr2 Mallnr3 Mallnr4 Force1 Force2 Force3	Force4 Downhang1         Downhang2	      Downhang3	       Downhang4 An Color Text
726	201	     -1	   33	   0	33	           6	 8	   9	 -1	   0	 AXCES 24kV	220	 45	  17,5 46	 55000 64000 0,0007	0,000023 2	   496	   1608	   1608	   0	   17609  5440	 5440	0	   0,790480137099752 2,55879797382543 2,55879797382543 0	     80 -1    39612284335

CABLE
Id name       type Material
7  AXCES 24kV 2	   6

CABLEDAT
Id CableId CLASS AREACLASS AREA	DIAM QE	  STRAIN S1	S2 S3 S4 S5	TEMP CableMatId
33 7	     3x70/16   220	45	    17,5 46	 -1	-1 -1 -1 -1	0	 17

CABELMAT 
Id Name	            EIK	  EP	Tp	 V	      SLimit1 SLimit2 EFactor V400 V004	V410
17 AXCES 3X70 24 kV	55000 64000	0,00 0,000023 123	  123	  0			

REGLAR
CROSSARM 
Id	 StructId crossarmnr crossarm    avst fp2ruk top2ruk as2ruk	es2ruk rs2ruk weight passing fixed fork	articleno phases ConnectionType	MaxAngle MinAngle Class	linedistV MaxloadH MaxloadV	MaxloadHV
1522 1429     0          0172        0    0      0,2     0,1     0     2      5      -1      0     0              1      5              35       0        2  
1523 1429     1          E06 480 15  0    0      0,2     0       0,1   2      12     0       -1    -1             1      5              200      0        2 
1524 1430     0   	     0168-G      0    0      0,2     0,15    0,15  2      2      -1      0     0              1      4              400      0        2 
1525 1430     1   	     0168-A      0    0      0,3     0,15    0,15  2      2      0       -1    -1             1      4              200      0        2 
1526 1431     0   	     0168-G      0    0      0,2     0,15    0,15  2      2      -1      0     0              1      4              200      0        2 
1527 1431     1   	     0168-A      0    0      0,3     0,15    0,15  2      2      0       -1    -1             1      4              200      0        2 
1528 1432     0          Avsp E0648  0    0      0,2     0,1     0     2      5      -1      0     0              1      5              200      0        2 
1529 1432     1          Avsp. E064  0    0      0,2     0       0,1   2      5      0       -1    -1             1      5              200      0        2 
1530 1433     0   	     0168-G      0    0      0,2     0,15    0,15  2      2      -1      0     0              1      4              200      0        2 
1531 1433     1   	     0168-A      0    0      0,3     0,15    0,15  2      2      0       -1    -1             1      5              200      0        2 
1532 1434     0   	     0168/F      0    0      0,2     0,15    0,15  2      2      -1      0     0              1      4              200      0        2 
1533 1434     1   	     0168-A      0    0      0,3     0,15    0,15  2      2      0       -1    -1             1      5              200      0        2 
1534 1435     0          0170        0    0      0,2     0       0     2      2      -1      0     0              1      4              2        0        2 
1535 1436     0          0170-1      0    0      0,2     0       0     2      2      -1      0     0              1      4              3        0        2 
1536 1436     1          0170-2      0    0      0,5     0       0     2      2      -1      0     0              1      4              3        0        2 
1537 1437     0          0170        0    0      0,2     0       0     2      2      -1      0     0              1      4              2        0        2 
1538 1438     0          0171        0    0      0,2     0       0     2      2      -1      0     0              2      4              2        0        2 
1539 1439     0          0170        0    0      0,2     0       0     2      2      -1      0     0              1      4              2        0        2 
1540 1440     0          0171        0    0      0,2     0       0     2      2      -1      0     0              2      4              2        0        2 
1541 1441     0          0173-1      0    0      0,2     0,15    0     2      2      -1      0     0              1      4              100      0        2 
1542 1441     1          0173-2      0    0      0,5     0,15    0     2      2      -1      0     0              1      4              100      0        2 
1543 1442     0          0170        0    0      0,2     0,15    0     2      2      -1      0     0              1      4              35       0        2 
1544 1443     0          0173        0    0      0,2     0,15    0     2      2      -1      0     0              2      4              100      0        2 
1545 1444     0          SOT 73      0    0      0,2     0,15    0     2      2      -1      0     0              1      4              70       0        2 
1546 1445     0   	     2 x SOT 73  0    0      0,2     0,15    0     2      2      -1      0     0              2      4              70       0        2 
1547 1446     0   	     Avsp. E064  0    0      0,2     0,15    0     2      10     -1      0     0              1      5              100      0        2 
1548 1447     0          0170        0    0      0,2     0,15    0     2      2      -1      0     0              1      4              100      0        2 
1549 1448     0          0173        0    0      0,2     0,15    0     2      2      -1      0     0              2      4              100      0        2 
1550 1449     0          0170        0    0      0,2     0,15    0     2      2      -1      0     0              1      4              100      0        2 
1551 1450     0          0173        0    0      0,2     0,15    0     2      2      -1      0     0              2      4              100      0        2 
1552 1451     0          0203        0    0      0,3     0,15    0     2,7    10     -1      0     0              2      5              100      0        2 
1553 1451     1          0203        0    0      1,2     0,15    0     2      10     -1      0     0              1      4              100      0        2 
1554 1452     0          0203        0    0      0,2     0       0,01  2      10     0       -1    0              1      5              0        0        2 
1555 1453     0          0203-1      0    0      0,2     0       0,15  2      10     0       -1    0              1      5              0        0        2 
1556 1453     1          0170-2      0    0      0,5     0       0,15  2      10     0       -1    0              1      5              0        0        2 
1557 1454     0          0170        0    0      0,2     0       0,15  2      2      0       -1    0              1      4              0        0        2 
1558 1455     0          0174        0    0      0,2     0       0,15  2      2      0       -1    0              2      4              0        0        2 
1559 1456     0          0203        0    0      0,2     0       0,01  2      10     0       -1    0              2      5              0        0        2 
1560 1457     0          0170        0    0      0,2     0       0,15  2      2      0       -1    0              1      4              0        0        2 
1561 1458     0          0174        0    0      0,2     0       0,15  2      2      0       -1    0              2      4              0        0        2 
    
    INST ?                                                                                                          

*/




/*    /** Connect to STRUCT Table - innehåller namn på konstruktionen **/            */
/*    htable = hCurrdb:openrecordset("STRUCT",2,,).                                  */
/*    num = 1.                                                                       */
/*    DO WHILE NOT htable:eof():                                                     */
/*       char1 = htable:FIELDS("namn"):VALUE.                                        */
/*       IF char1 = keychar1 THEN DO:                                                */
/*          ASSIGN                                                                   */
/*          int1 = htable:FIELDS("Id"):VALUE                                         */
/*          int2 = htable:FIELDS("type"):VALUE                                       */
/*          char1 = htable:FIELDS("namn"):VALUE                                      */
/*             .                                                                     */
/*          IF int2 = 0 THEN char2 = "Raklinjestolpe".                               */
/*          ELSE IF int2 = 1 THEN char2 = "Vinkelstolpe".                            */
/*          ELSE IF int2 = 2 THEN char2 = "Avgreningsstolpe".                        */
/*          ELSE IF int2 = 3 THEN char2 = "Ändstolpe".                               */
/*          ELSE IF int2 = 4 THEN char2 = "Avgreningsstolpe".                        */
/*                                                                                   */
/*                                                                                   */
/*          DISPLAY int1 int2 char1 char2  WITH DOWN WIDTH 60 TITLE "STRUCT".        */
/*          DOWN.                                                                    */
/*       END.                                                                        */
/*       htable:movenext().                                                          */
/*       num = num + 1.                                                              */
/*    END.                                                                           */
/*    RELEASE OBJECT htable NO-ERROR.                                                */
/*    htable = ?.                                                                    */
/*                                                                                   */
/*                                                                                   */
/*    /** Connect to SEGMENTS Table  **/                                             */
/*    htable = hCurrdb:openrecordset("SEGMENTS",2,,).                                */
/*    num = 1.                                                                       */
/*    DO WHILE NOT htable:eof():                                                     */
/*       int1 = htable:FIELDS("SurveyId"):VALUE.                                     */
/*       IF int1 = keyint1 THEN DO:                                                  */
/*          ASSIGN                                                                   */
/*          keyint3 = htable:FIELDS("CableId"):VALUE                                 */
/*          int1 = htable:FIELDS("SurveyId"):VALUE                                   */
/*          int2 = htable:FIELDS("Fran"):VALUE                                       */
/*          int3 = htable:FIELDS("Till"):VALUE                                       */
/*          char1 = htable:FIELDS("Lina"):VALUE                                      */
/*          char2 = htable:FIELDS("Text"):VALUE                                      */
/*          .                                                                        */
/*          DISPLAY int1 int2 int3 char1 char2  WITH DOWN WIDTH 60 TITLE "SEGMENTS". */
/*          DOWN.                                                                    */
/*       END.                                                                        */
/*       htable:movenext().                                                          */
/*       num = num + 1.                                                              */
/*    END.                                                                           */
/*    RELEASE OBJECT htable NO-ERROR.                                                */
/*    htable = ?.                                                                    */
/*                                                                                   */
/*                                                                                   */
/*    /*    /** Connect to CABLEDAT Table **/                                     */ */
/*    htable = hCurrdb:openrecordset("CABLEDAT",2,,).                                */
/*    num = 1.                                                                       */
/*    DO WHILE NOT htable:eof():                                                     */
/*       int1 = htable:FIELDS("CableId"):VALUE.                                      */
/*       IF int1 = keyint3 THEN DO:                                                  */
/*          ASSIGN                                                                   */
/*          keyint4 = htable:FIELDS("Id"):VALUE                                      */
/*          int1 = htable:FIELDS("CableId"):VALUE                                    */
/*          int2 = htable:FIELDS("Area"):VALUE                                       */
/*          char1 = htable:FIELDS("AREACLASS"):VALUE                                 */
/*          .                                                                        */
/*          DISPLAY int1 int2 char1  WITH DOWN WIDTH 90 TITLE "CABLEDAT".            */
/*          DOWN.                                                                    */
/*       END.                                                                        */
/*       htable:movenext().                                                          */
/*       num = num + 1.                                                              */
/*    END.                                                                           */
/*    RELEASE OBJECT htable NO-ERROR.                                                */
/*    htable = ?.                                                                    */
/*                                                                                   */
/*    /** Connect to CABLE Table - innehåller typ av lina **/                        */
/*    htable = hCurrdb:openrecordset("CABLE",2,,).                                   */
/*    num = 1.                                                                       */
/*    DO WHILE NOT htable:eof():                                                     */
/*       int1 = htable:FIELDS("Id"):VALUE.                                           */
/*       IF int1 = keyint4 THEN DO:                                                  */
/*          ASSIGN                                                                   */
/*          int1 = htable:FIELDS("Id"):VALUE                                         */
/*          int2 = htable:FIELDS("type"):VALUE                                       */
/*          char1 = htable:FIELDS("name"):VALUE                                      */
/*             .                                                                     */
/*          DISPLAY int1 int2 char1  WITH DOWN WIDTH 90 TITLE "CABLE".               */
/*          DOWN.                                                                    */
/*       END.                                                                        */
/*       htable:movenext().                                                          */
/*       num = num + 1.                                                              */
/*    END.                                                                           */
/*    RELEASE OBJECT htable NO-ERROR.                                                */
/*    htable = ?.                                                                    */


/*    /** Connect to CABLEMAT Table - innehåller kabelinfo**/     */
/*    htable = hCurrdb:openrecordset("CABLEMAT",2,,).             */
/*    num = 1.                                                    */
/*    DO WHILE NOT htable:eof() AND num < 100:                    */
/*       ASSIGN                                                   */
/*       int1 = htable:FIELDS("Id"):VALUE                         */
/*       char1 = htable:FIELDS("Name"):VALUE                      */
/*       .                                                        */
/*       DISPLAY int1 char1  WITH DOWN WIDTH 90 TITLE "CABLEMAT". */
/*       DOWN.                                                    */
/*       htable:movenext().                                       */
/*       num = num + 1.                                           */
/*    END.                                                        */
/*    RELEASE OBJECT htable NO-ERROR.                             */
/*    htable = ?.                                                 */
   

/***********************************************************************
Recordset objects have the following methods:
movefirst
movelast
movenext
moveprevious
findfirst
findlast
findnext
findprevious
addnew
edit
update
.... etc
************************************************************************/

/*********************************************************************** 
Reading records - Loop through first 10 records in the table 
Each field is an entry in the recordset's fields collection.
For some reason, this is very slow.
************************************************************************/
/* DO WHILE NOT htable:eof() AND li-cnt < 10:          */
/*    ASSIGN                                             */
/*    lc-custcode = htable:FIELDS("NR"):VALUE          */
/*    lc-company = htable:FIELDS("KLASS"):VALUE.       */
/*                                                       */
/*    DISPLAY lc-custcode lc-company WITH DOWN WIDTH 90. */
/*    DOWN.                                              */
/*                                                       */
/*    htable:movenext().                               */
/*    li-cnt = li-cnt + 1.                               */
/* END.                                                  */
/*
/***********************************************************************
Add a new record 
1. Add the record
2. Set the field values
3. Write the changes back 
************************************************************************/ 
htable:AddNew
ASSIGN
htable:FIELDS("OMRADE"):VALUE = "TMP" 
htable:FIELDS("BENAMNING"):VALUE = "Test Company".
htable:UPDATE(,).

/***********************************************************************
Find a record. 
The findfirst/last/next/previous methods all work on dynaset type
recordsets
only.
They each take a string parameter, which is an SQL where clause.
The nomatch propertie can be tested to see if find was successful.
************************************************************************/ 
htable:findfirst("OMRADE = 'TMP'").
IF htable:nomatch
THEN DO:
   BELL.
   MESSAGE "Customer not found" VIEW-AS ALERT-BOX ERROR.
END.

/***********************************************************************
Update a record.
1. Lock the record (using the Edit method)
2. Change the values of the fields
3. Write the changes back (using the Update method)
************************************************************************/
ELSE DO:
   htable:edit.
   htable:FIELDS("BENAMNING"):VALUE = "Fred Flinstone".
   htable:UPDATE(,).

/***********************************************************************
Finally, delete the record using the delete method.

************************************************************************/
   htable:DELETE.
END.

*/






   /*Kolla vadupp_UI i bervalsw.p som kör nykabstart_UI och i den BERHMTKONMTRL.P*/
