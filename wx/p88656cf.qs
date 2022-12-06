 _VERSION-NUMBER UIB_v9r12 GUI


 _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 

/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{VALDBDEF.I}
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{GLOBVAR2.I}
{EGENBVAR.I}
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
/* DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO. */
DEFINE VARIABLE utskriv AS LOGICAL NO-UNDO.
DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
DEFINE VARIABLE frandb AS CHARACTER NO-UNDO.
DEFINE VARIABLE franforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgdb AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgfieldh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgqh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempkommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE tillcon AS CHARACTER NO-UNDO.
DEFINE VARIABLE tilldb AS CHARACTER NO-UNDO.
DEFINE VARIABLE tillforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE compproch AS HANDLE NO-UNDO.        /* COMPTABLE.P */
DEFINE VARIABLE tabnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE klar AS LOGICAL NO-UNDO.
DEFINE VARIABLE stoppa AS LOGICAL NO-UNDO.
DEFINE VARIABLE counter AS INTEGER NO-UNDO.
DEFINE VARIABLE counter2 AS INTEGER NO-UNDO.
DEFINE VARIABLE asynch AS HANDLE NO-UNDO.
DEFINE VARIABLE fellog AS LOGICAL NO-UNDO.
DEFINE VARIABLE textmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE klarlog AS LOGICAL NO-UNDO.
DEFINE VARIABLE db1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE f1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE db2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE f2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE styrlog AS LOGICAL NO-UNDO.

/* DEFINE NEW SHARED VARIABLE gplk AS CHARACTER NO-UNDO.     /*PLANNR KORT*/   */
/* DEFINE NEW SHARED VARIABLE gaok AS CHARACTER NO-UNDO.       /*AONR KORT*/   */
/* DEFINE NEW SHARED VARIABLE gomrk AS CHARACTER NO-UNDO.      /*OMRÅDE KORT*/ */
/* DEFINE NEW SHARED VARIABLE varforetypval AS INTEGER EXTENT 50 NO-UNDO.      */

DEFINE TEMP-TABLE filetemp NO-UNDO
   FIELD FILENAME    AS CHARACTER   FORMAT "x(32)"
   FIELD FILENUMBER  AS INTEGER     FORMAT "->>>>9"
   FIELD NUMKEY      AS INTEGER     FORMAT ">>9"
   FIELD NUMKFLD     AS INTEGER     FORMAT ">>9"
   FIELD NUMKCOMP    AS INTEGER     FORMAT ">>9"
   FIELD NUMFLD      AS INTEGER     FORMAT ">>9"
   FIELD LASTCHANGED AS INTEGER     FORMAT "->>>>>>>>>9"
   FIELD FILEREC     AS RECID. 

DEFINE TEMP-TABLE fieldtemp NO-UNDO
   FIELD FIELDNAME   AS CHARACTER   FORMAT "x(32)"
   FIELD FILEREC     AS RECID.

DEFINE TEMP-TABLE dbcompresult NO-UNDO
   FIELD DBNAME1  AS CHARACTER   FORMAT "x(32)"
   FIELD DBNAME2  AS CHARACTER   FORMAT "x(32)"
   FIELD FVAL1    AS CHARACTER   FORMAT "x(32)"
   FIELD FVAL2    AS CHARACTER   FORMAT "x(32)". 


DEFINE TEMP-TABLE extravaldbtemp NO-UNDO LIKE valdbtemp.

DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(90)".


/* DEFINE TEMP-TABLE valfiletemp NO-UNDO LIKE filetemp. */

/* _UIB-CODE-BLOCK-END */

 _UIB-PREPROCESSOR-BLOCK 


/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_DB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES valdbtemp fieldtemp dbcompresult filetemp ~
extravaldbtemp

/* Definitions for BROWSE BRW_DB                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_DB valdbtemp.FORETAG valdbtemp.VALDB 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DB 
&Scoped-define QUERY-STRING-BRW_DB FOR EACH valdbtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_DB OPEN QUERY BRW_DB FOR EACH valdbtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_DB valdbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DB valdbtemp


/* Definitions for BROWSE BRW_FALT                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FALT fieldtemp.FIELDNAME 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FALT 
&Scoped-define QUERY-STRING-BRW_FALT FOR EACH fieldtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_FALT OPEN QUERY BRW_FALT FOR EACH fieldtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_FALT fieldtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FALT fieldtemp


/* Definitions for BROWSE BRW_RES                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_RES dbcompresult.DBNAME1 ~
dbcompresult.DBNAME2 dbcompresult.FVAL1 dbcompresult.FVAL2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_RES 
&Scoped-define QUERY-STRING-BRW_RES FOR EACH dbcompresult NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_RES OPEN QUERY BRW_RES FOR EACH dbcompresult NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_RES dbcompresult
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_RES dbcompresult


/* Definitions for BROWSE BRW_TAB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_TAB filetemp.FILENAME 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TAB 
&Scoped-define QUERY-STRING-BRW_TAB FOR EACH filetemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TAB OPEN QUERY BRW_TAB FOR EACH filetemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TAB filetemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TAB filetemp


/* Definitions for BROWSE BRW_VDB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VDB extravaldbtemp.FORETAG ~
extravaldbtemp.VALDB 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VDB 
&Scoped-define QUERY-STRING-BRW_VDB FOR EACH extravaldbtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_VDB OPEN QUERY BRW_VDB FOR EACH extravaldbtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_VDB extravaldbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VDB extravaldbtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_DB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_DB BRW_VDB BTN_OVER BTN_BACK BRW_TAB ~
BRW_FALT BTN_FORST FILL-IN_FORSTA BTN_ANDRA FILL-IN_ANDRA BRW_RES BTN_OK ~
BTN-SKRIV BTN_AVB RECT-2 RECT-5 RECT-6 
&Scoped-Define DISPLAYED-OBJECTS EDT-LOG FILL-IN_FORSTA FILL-IN_ANDRA 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */



DEFINE QUERY BRW_DB FOR 
      valdbtemp SCROLLING.

DEFINE QUERY BRW_FALT FOR 
      fieldtemp SCROLLING.

DEFINE QUERY BRW_RES FOR 
      dbcompresult SCROLLING.

DEFINE QUERY BRW_TAB FOR 
      filetemp SCROLLING.

DEFINE QUERY BRW_VDB FOR 
      extravaldbtemp SCROLLING.

 _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DB C-Win _STRUCTURED

  QUERY BRW_DB NO-LOCK DISPLAY
      valdbtemp.FORETAG FORMAT "X(5)":U
      valdbtemp.VALDB COLUMN-LABEL "Databas" FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FALT C-Win _STRUCTURED

  QUERY BRW_FALT NO-LOCK DISPLAY
      fieldtemp.FIELDNAME COLUMN-LABEL "Fältnamn" FORMAT "X(16)":U
/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_RES C-Win _STRUCTURED

  QUERY BRW_RES NO-LOCK DISPLAY
      dbcompresult.DBNAME1 COLUMN-LABEL "Databas 1" FORMAT "X(12)":U
      dbcompresult.DBNAME2 COLUMN-LABEL "Databas 2" FORMAT "X(12)":U
      dbcompresult.FVAL1 COLUMN-LABEL "Värde 1" FORMAT "X(12)":U
      dbcompresult.FVAL2 COLUMN-LABEL "Värde 2" FORMAT "X(12)":U
/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TAB C-Win _STRUCTURED

  QUERY BRW_TAB NO-LOCK DISPLAY
      filetemp.FILENAME COLUMN-LABEL "Tabellnamn" FORMAT "x(25)":U
/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VDB C-Win _STRUCTURED

  QUERY BRW_VDB NO-LOCK DISPLAY
      extravaldbtemp.FORETAG FORMAT "X(5)":U
      extravaldbtemp.VALDB COLUMN-LABEL "Databas" FORMAT "X(30)":U
            WIDTH 25
/* _UIB-CODE-BLOCK-END */

 _PROCEDURE-SETTINGS

/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: dbcompresult T "?" NO-UNDO temp-db dbcompresult
      TABLE: extravaldbtemp T "?" NO-UNDO temp-db extravaldbtemp
      TABLE: fieldtemp T "?" NO-UNDO temp-db fieldtemp
      TABLE: filetemp T "?" NO-UNDO temp-db filetemp
      TABLE: valdbtemp T "?" NO-UNDO temp-db valdbtemp
      TABLE: valfiletemp T "?" NO-UNDO temp-db valfiletemp
   END-TABLES.
 */
_END-PROCEDURE-SETTINGS
 _CREATE-WINDOW

IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kopiera tabell"
         HEIGHT             = 23.83
         WIDTH              = 122.75
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */

 _RUN-TIME-ATTRIBUTES

/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_DB 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_VDB BRW_DB DEFAULT-FRAME */
/* BROWSE-TAB BRW_TAB BTN_BACK DEFAULT-FRAME */
/* BROWSE-TAB BRW_FALT BRW_TAB DEFAULT-FRAME */
/* BROWSE-TAB BRW_RES FILL-IN_ANDRA DEFAULT-FRAME */
ASSIGN 
       BRW_DB:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_FALT:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_RES:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_TAB:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_VDB:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BUTTON BTN_STOP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDT-LOG IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDT-LOG:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN_ANDRA:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN_FORSTA:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */

 _QUERY-BLOCK BROWSE BRW_DB

/* Query rebuild information for BROWSE BRW_DB
     _TblList          = "Temp-Tables.valdbtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.valdbtemp.FORETAG
     _FldNameList[2]   > Temp-Tables.valdbtemp.VALDB
"valdbtemp.VALDB" "Databas" "X(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_DB */

 _QUERY-BLOCK BROWSE BRW_FALT

/* Query rebuild information for BROWSE BRW_FALT
     _TblList          = "Temp-Tables.fieldtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.fieldtemp.FIELDNAME
"fieldtemp.FIELDNAME" "Fältnamn" "X(16)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FALT */

 _QUERY-BLOCK BROWSE BRW_RES

/* Query rebuild information for BROWSE BRW_RES
     _TblList          = "Temp-Tables.dbcompresult"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.dbcompresult.DBNAME1
"dbcompresult.DBNAME1" "Databas 1" "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.dbcompresult.DBNAME2
"dbcompresult.DBNAME2" "Databas 2" "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.dbcompresult.FVAL1
"dbcompresult.FVAL1" "Värde 1" "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.dbcompresult.FVAL2
"dbcompresult.FVAL2" "Värde 2" "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_RES */

 _QUERY-BLOCK BROWSE BRW_TAB

/* Query rebuild information for BROWSE BRW_TAB
     _TblList          = "Temp-Tables.filetemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.filetemp.FILENAME
"filetemp.FILENAME" "Tabellnamn" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TAB */

 _QUERY-BLOCK BROWSE BRW_VDB

/* Query rebuild information for BROWSE BRW_VDB
     _TblList          = "Temp-Tables.extravaldbtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.extravaldbtemp.FORETAG
     _FldNameList[2]   > Temp-Tables.extravaldbtemp.VALDB
"extravaldbtemp.VALDB" "Databas" "X(30)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VDB */

 _UIB-CODE-BLOCK _CONTROL C-Win C-Win

ON END-ERROR OF C-Win /* Kopiera tabell */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL C-Win C-Win

ON WINDOW-CLOSE OF C-Win /* Kopiera tabell */
DO:
  /* This event will close the window and terminate the procedure.  */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BRW_DB C-Win

ON VALUE-CHANGED OF BRW_DB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
   /*    MESSAGE valdbtemp.ORDNING valdbtemp.FORETAG valdbtemp.GFORETAG. */
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BRW_FALT C-Win

ON VALUE-CHANGED OF BRW_FALT IN FRAME DEFAULT-FRAME /* Fält */
DO:
/*    IF AVAILABLE fieldtemp THEN DO:                                                                        */
/* /*       RUN setcolsortvar_UI IN brwproc[4] (INPUT "fieldtemp.FILEREC = " + STRING(filetemp.FILEREC)). */ */
/*       MESSAGE fieldtemp.FILEREC.                                                                          */
/*    END.                                                                                                   */
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BRW_TAB C-Win

ON VALUE-CHANGED OF BRW_TAB IN FRAME DEFAULT-FRAME /* Tabeller */
DO:
   IF AVAILABLE filetemp THEN DO:
      RUN setcolsortvar_UI IN brwproc[4] (INPUT "fieldtemp.FILEREC = " + STRING(filetemp.FILEREC)).
      RUN openbdynspec_UI IN brwproc[4].
      ASSIGN
      FILL-IN_ANDRA:SCREEN-VALUE = "?"
      FILL-IN_FORSTA:SCREEN-VALUE = "?".
      tempcolh = BRW_RES:GET-BROWSE-COLUMN(3) IN FRAME {&FRAME-NAME}.
      tempcolh:LABEL = "?".
      tempcolh = BRW_RES:GET-BROWSE-COLUMN(4) IN FRAME {&FRAME-NAME}.
      tempcolh:LABEL = "?".
   END.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win

ON VALUE-CHANGED OF BRW_VDB IN FRAME DEFAULT-FRAME /* Valda databaser */
DO:
/*    MESSAGE valdbtemp.ORDNING valdbtemp.FORETAG valdbtemp.GFORETAG. */
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BTN-SKRIV C-Win

ON CHOOSE OF BTN-SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:    
   RUN skrivut_UI.
/*    DEFINE VARIABLE firstdbname AS CHARACTER NO-UNDO.                                                             */
/*    DEFINE VARIABLE ftcommand AS CHARACTER NO-UNDO.                                                               */
/*    DEFINE VARIABLE stcommand AS CHARACTER NO-UNDO.                                                               */
/*    DEFINE VARIABLE ffieldcommand AS CHARACTER NO-UNDO.                                                           */
/*    DEFINE VARIABLE sfieldcommand AS CHARACTER NO-UNDO.                                                           */
/*    DEFINE VARIABLE openkommando AS CHARACTER NO-UNDO.                                                            */
/*    DEFINE VARIABLE tablename AS CHARACTER NO-UNDO.                                                               */
/*    DEFINE VARIABLE fieldnamefirst AS CHARACTER NO-UNDO.                                                          */
/*    DEFINE VARIABLE fieldnamesecond AS CHARACTER NO-UNDO.                                                         */
/*                                                                                                                  */
/*    ASSIGN                                                                                                        */
/*    tablename = filetemp.FILENAME                                                                                 */
/*    fieldnamefirst = FILL-IN_FORSTA:SCREEN-VALUE                                                                  */
/*    fieldnamesecond = FILL-IN_ANDRA:SCREEN-VALUE                                                                  */
/*    firstdbname = "RT9".                                                                                          */
/*    /* Skapar query-strängar */                                                                                   */
/*    ftcommand = firstdbname + "." + tablename.                                                                    */
/*    stcommand = extravaldbtemp.DBNAMN + "." + tablename.                                                          */
/*    ffieldcommand = ftcommand + "." + fieldnamefirst + " = " + stcommand + "." + fieldnamefirst.                  */
/*    IF fieldnamesecond = "?" THEN sfieldcommand = "".                                                             */
/*    ELSE sfieldcommand = " AND " + stcommand + "." + fieldnamesecond + " = " + stcommand + "." + fieldnamesecond. */
/*                                                                                                                  */
/*    openkommando = "FOR EACH " + ftcommand + ", EACH " + stcommand                                                */
/*                   + " WHERE " + ffieldcommand + sfieldcommand + " NO-LOCK.".                                     */
/*                                                                                                                  */
/*    MESSAGE openkommando VIEW-AS ALERT-BOX.                                                                       */
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BTN_ANDRA C-Win

ON CHOOSE OF BTN_ANDRA IN FRAME DEFAULT-FRAME
DO:
  /* andra knapp */
   BRW_FALT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF AVAILABLE fieldtemp THEN DO:
      IF FILL-IN_FORSTA:SCREEN-VALUE = fieldtemp.FIELDNAME THEN DO:
         MESSAGE "Första och andra kan ej ha samma värde!" VIEW-AS ALERT-BOX TITLE "Meddelande!".
      END.
      ELSE DO: 
         FILL-IN_ANDRA:SCREEN-VALUE = fieldtemp.FIELDNAME.
         tempcolh = BRW_RES:GET-BROWSE-COLUMN(4) IN FRAME {&FRAME-NAME}.
         tempcolh:LABEL = fieldtemp.FIELDNAME.
      END.
   END.           
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win

ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BTN_FORST C-Win

ON CHOOSE OF BTN_FORST IN FRAME DEFAULT-FRAME
DO:
  /* första knapp */
   BRW_FALT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF AVAILABLE fieldtemp THEN DO:
      IF FILL-IN_ANDRA:SCREEN-VALUE = fieldtemp.FIELDNAME THEN DO:
         MESSAGE "Första och andra kan ej ha samma värde!" VIEW-AS ALERT-BOX TITLE "Meddelande!".
      END.
      ELSE DO: 
         FILL-IN_FORSTA:SCREEN-VALUE = fieldtemp.FIELDNAME.
         tempcolh = BRW_RES:GET-BROWSE-COLUMN(3) IN FRAME {&FRAME-NAME}.
         tempcolh:LABEL = fieldtemp.FIELDNAME.         
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win

ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Jämför */
DO:
   RUN btnok_UI.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BTN_STOP C-Win

ON CHOOSE OF BTN_STOP IN FRAME DEFAULT-FRAME /* Stoppa */
DO:
   IF stoppa = FALSE THEN DO:
/*       MESSAGE "Är du säker på att du vill stoppa jämförningen?"         */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL. */
/*       IF svar = TRUE THEN stoppa = TRUE. */
      stoppa = TRUE.
      EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Håller på att avsluta jämförning!" + CHR(10).
   END.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 



/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF klar = FALSE THEN RETURN NO-APPLY.
   RUN disconnectdb_UI IN compproch.
   IF VALID-HANDLE(compproch) THEN DELETE PROCEDURE compproch.
   IF VALID-HANDLE(apphand) THEN DO:
      IF apphand:CONNECTED() = TRUE THEN apphand:DISCONNECT().
      DELETE OBJECT apphand.
   END.   
   {BORTBRWPROC.I}
   RUN disable_UI.
END.
    
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
/*    {WIN_M_START.I} */   
   {VALDBALL.I}  
   {ALLSTARTDYN.I}
   RUN enable_UI.
   RUN start_UI.
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {&WINDOW-NAME}:MOVE-TO-TOP ().
   ENABLE EDT-LOG WITH FRAME {&FRAME-NAME}.
/*    {WIN_M_SLUT.I} */
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 

PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/  
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_DB:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_VDB:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_TAB:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_FALT:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_RES:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN DYNARROW.P PERSISTENT SET brwproc[6]
      (INPUT BRW_DB:HANDLE, INPUT BRW_VDB:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT ?, INPUT ?, INPUT BTN_BACK:HANDLE).
   RUN mouseselclick_UI IN brwproc[3] (INPUT FALSE).   
/*    RUN settitlenum_UI IN brwproc[{&ARROWS}] (INPUT TRUE).                  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE btnok_UI C-Win 

PROCEDURE btnok_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE tempdb AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tempkoll AS LOGICAL NO-UNDO.
   DEFINE VARIABLE numdb AS INTEGER NO-UNDO.
   ASSIGN
   FILL-IN_FORSTA = INPUT FRAME {&FRAME-NAME} FILL-IN_FORSTA
   FILL-IN_ANDRA = INPUT FRAME {&FRAME-NAME} FILL-IN_ANDRA.

   IF FILL-IN_FORSTA = "?" THEN DO:
      MESSAGE "Första fältet får inte vara '?'" VIEW-AS ALERT-BOX TITLE "Meddelande!".
   END.
   ELSE DO:
      tempkoll = FALSE.
      OPEN QUERY oq FOR EACH extravaldbtemp NO-LOCK.
      GET FIRST oq NO-LOCK.
      IF AVAILABLE extravaldbtemp THEN DO: 
         ASSIGN
         numdb = 1.
         tempdb = extravaldbtemp.FORETAG.
         GET NEXT oq NO-LOCK.
         DO WHILE AVAILABLE extravaldbtemp:
            numdb = numdb + 1.
            IF tempdb NE extravaldbtemp.FORETAG THEN tempkoll = TRUE.
            GET NEXT oq NO-LOCK.
         END.
      END.
      CLOSE QUERY oq.      
      IF tempkoll = TRUE THEN MESSAGE "Databaserna måste ligga inom samma företag!" VIEW-AS ALERT-BOX.
      ELSE IF numdb < 2 THEN MESSAGE "Du måste välja minst två databaser!" VIEW-AS ALERT-BOX.
      ELSE DO:
         FIND FIRST extravaldbtemp NO-LOCK NO-ERROR.
         EDT-LOG:SCREEN-VALUE = "Connecting to " + extravaldbtemp.DBNAMN + CHR(10).
         RUN conappserver_UI (INPUT extravaldbtemp.APPCON, INPUT extravaldbtemp.GFORETAG).
         IF appcon = TRUE THEN DO:
            EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Connected!" + CHR(10).
            IF AVAILABLE filetemp THEN DO:
               RUN COMPTABLE.P PERSISTENT SET compproch ON apphand TRANSACTION DISTINCT
                  (INPUT TABLE extravaldbtemp, INPUT filetemp.FILENAME, 
                   INPUT FILL-IN_FORSTA, INPUT FILL-IN_ANDRA).                  
               klar = FALSE.
               RUN visagom_UI.
               counter = 0.
               FOR EACH dbcompresult:
                  DELETE dbcompresult.
               END.
               RUN openbdynspec_UI IN brwproc[5].
               RUN compdb_UI.
            END.
         END.
      END.
   END.

         /*    MESSAGE "ORDNING:" extravaldbtemp.ORDNING SKIP "FORETAG:" extravaldbtemp.FORETAG SKIP */
         /*       "VALDB:" extravaldbtemp.VALDB SKIP "DBNAMN:" extravaldbtemp.DBNAMN SKIP            */
         /*       "DBCON:" extravaldbtemp.DBCON SKIP "DBCACHE:" extravaldbtemp.DBCACHE SKIP          */
         /*       "DBPLATS:" extravaldbtemp.DBPLATS SKIP "APPCON:" extravaldbtemp.APPCON SKIP        */
         /*       "GFORETAG:" extravaldbtemp.GFORETAG VIEW-AS ALERT-BOX.                             */

   
/*       MESSAGE ORDNING FORETAG VALDB DBNAMN DBCON DBCACHE DBPLATS APPCON.                                         */
/*    ASSIGN                                                                                                        */
/*    FILL-FRAN = INPUT FRAME {&FRAME-NAME} FILL-FRAN                                                               */
/*    FILL-TILL = INPUT FRAME {&FRAME-NAME} FILL-TILL.                                                              */
/*    FIND FIRST valfiletemp NO-LOCK NO-ERROR.                                                                      */
/*    IF FILL-FRAN = "" OR FILL-TILL = "" THEN DO:                                                                  */
/*       MESSAGE "Du måste välja någon databas!" VIEW-AS ALERT-BOX.                                                 */
/*    END.                                                                                                          */
/*    ELSE IF NOT AVAILABLE valfiletemp THEN DO:                                                                    */
/*       MESSAGE "Du måste välja någon tabell!" VIEW-AS ALERT-BOX.                                                  */
/*    END.                                                                                                          */
/*    ELSE IF tillforetag = franforetag THEN DO:                                                                    */
/*       MESSAGE "Vill du kopiera data från databas " + FILL-FRAN +                                                 */
/*          " till databas " + FILL-TILL + "?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.    */
/*       IF svar = TRUE THEN DO:                                                                                    */
/*          MESSAGE "Vill du radera data från databas " + FILL-FRAN + "?"                                           */
/*             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar2 AS LOGICAL.                                   */
/*          RUN conappserver_UI.                                                                                    */
/*          IF appcon = TRUE THEN DO:                                                                               */
/*             RUN KOPTABDB.P PERSISTENT SET koptabproch ON apphand TRANSACTION DISTINCT                            */
/*             (INPUT frandb, INPUT tilldb, INPUT TABLE valfiletemp, INPUT tillcon,                                 */
/*              INPUT svar2, OUTPUT felmed, OUTPUT fellog).                                                         */
/*             EDT-LOG:SCREEN-VALUE = felmed.                                                                       */
/*             IF fellog = FALSE THEN DO:                                                                           */
/*                EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + CHR(10) + "Kopierar tabeller!" + CHR(10) + CHR(10). */
/*                EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Kop-tid: Tabellnamn:" + CHR(10).                   */
/*                klar = FALSE.                                                                                     */
/*                RUN visagom_UI.                                                                                   */
/*                counter = 0.                                                                                      */
/*                RUN kopieratab_UI.                                                                                */
/*             END.                                                                                                 */
/*          END.                                                                                                    */
/*       END.                                                                                                       */
/*    END.                                                                                                          */
/*    ELSE DO:                                                                                                      */
/*       MESSAGE "Båda databaserna måste ligga på samma server.".                                                   */
/*    END.                                                                                                          */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE compdb_UI C-Win 

PROCEDURE compdb_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   /*    counter2 = 0.       */

   RUN condb_UI IN compproch ASYNCHRONOUS SET asynch 
      EVENT-PROCEDURE "compevent_UI" IN THIS-PROCEDURE (OUTPUT textmedd, OUTPUT db1, OUTPUT db2).
   
   /*    RUN koptab_UI IN koptabproch ASYNCHRONOUS SET asynch EVENT-PROCEDURE "tabevent_UI" IN THIS-PROCEDURE (OUTPUT felmed). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE compevent_UI C-Win 

PROCEDURE compevent_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER textmedd AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER dbname1 AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER dbname2 AS CHARACTER NO-UNDO.
   ASSIGN
   db1 = dbname1
   db2 = dbname2.
   IF textmedd = "" THEN DO:
      EDT-LOG:SCREEN-VALUE IN FRAME {&FRAME-NAME} = EDT-LOG:SCREEN-VALUE + db1 + "-" + db2 + CHR(10).
      IF stoppa = FALSE THEN DO:
/*          RUN compdb_UI. */
         IF db1 NE "" THEN RUN compf_UI.
         ELSE RUN compdb_UI.
      END.
      ELSE DO:
         RUN disconnectdb_UI IN compproch.
         ASSIGN
         EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Jämförning avbruten!" + CHR(10)
         stoppa = FALSE
         klar = TRUE.
         RUN visagom_UI.
      END.
   END.
   ELSE DO:
      klar = TRUE.
      RUN visagom_UI.
      IF textmedd = "" THEN DO:
         EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Klar!!!!!!!" + CHR(10).
      END.
      ELSE DO:
         EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + textmedd + CHR(10).
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE compfevent_UI C-Win 

PROCEDURE compfevent_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER klarlog AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER f1 AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f2 AS CHARACTER NO-UNDO.
   IF klarlog = FALSE THEN DO:
      IF stoppa = FALSE THEN DO:
         EDT-LOG:SCREEN-VALUE IN FRAME {&FRAME-NAME} = EDT-LOG:SCREEN-VALUE + f1 + " *-* " + f2 + CHR(10).
         CREATE dbcompresult.
         ASSIGN
         dbcompresult.DBNAME1 = db1
         dbcompresult.DBNAME2 = db2
         dbcompresult.FVAL1 = f1
         dbcompresult.FVAL2 = f2.
         RUN compf_UI.         
      END.
      ELSE DO:
         RUN disconnectdb_UI IN compproch.
         ASSIGN
         EDT-LOG:SCREEN-VALUE IN FRAME {&FRAME-NAME} = EDT-LOG:SCREEN-VALUE + "Jämförning avbruten!" + CHR(10)
         stoppa = FALSE
         klar = TRUE.
         RUN openbdynspec_UI IN brwproc[5].
         RUN visagom_UI.
      END.
   END.
   ELSE DO:
      RUN openbdynspec_UI IN brwproc[5].
      RUN compdb_UI.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE compf_UI C-Win 

PROCEDURE compf_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RUN compfield_UI IN compproch ASYNCHRONOUS SET asynch 
      EVENT-PROCEDURE "compfevent_UI" IN THIS-PROCEDURE (OUTPUT klarlog, OUTPUT f1, OUTPUT f2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE conappserver_UI C-Win 

PROCEDURE conappserver_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER conappvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER gforetag AS CHARACTER NO-UNDO.
   {muswait.i}          
   IF apphand:CONNECTED() = FALSE THEN DO:
      appcon = apphand:CONNECT(conappvar, "ELPAO", "KAGGEN", gforetag).
      IF appcon = FALSE THEN DO:
         MESSAGE "Det går ej att ansluta appserver." SKIP "Kontakta Mikael Eriksson!" VIEW-AS ALERT-BOX.      
      END.   
   END.
   ELSE DO:                    
      IF VALID-HANDLE(compproch) THEN DELETE PROCEDURE compproch.
      IF ENTRY(3,apphand:NAME,":") NE ENTRY(2,conappvar," ") THEN DO:
         appcon = apphand:DISCONNECT().
         IF appcon = FALSE THEN DO:
            MESSAGE "Kan ej koppla från appserver" apphand:NAME.
         END.
         ELSE DO:
            appcon = apphand:CONNECT(conappvar, "ELPAO", "KAGGEN", gforetag).
            IF appcon = FALSE THEN DO:
               MESSAGE "Det går ej att ansluta appserver." SKIP 
               "Kontakta Mikael Eriksson!" VIEW-AS ALERT-BOX.      
            END.   
         END.
      END.
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE

PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE

PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY EDT-LOG FILL-IN_FORSTA FILL-IN_ANDRA 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_DB BRW_VDB BTN_OVER BTN_BACK BRW_TAB BRW_FALT BTN_FORST 
         FILL-IN_FORSTA BTN_ANDRA FILL-IN_ANDRA BRW_RES BTN_OK BTN-SKRIV 
         BTN_AVB RECT-2 RECT-5 RECT-6 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE skrivut_UI C-Win 

PROCEDURE skrivut_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tidut:
      DELETE tidut.
   END.
   CREATE tidut.
   SUBSTRING(tidut.UT,5) = "Databas 1".
   SUBSTRING(tidut.UT,20) = "Databas 2".
   SUBSTRING(tidut.UT,35) = FILL-IN_FORSTA:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
   SUBSTRING(tidut.UT,50) = FILL-IN_ANDRA:SCREEN-VALUE.
   CREATE tidut.
   tidut.UT = "****--------------*--------------*--------------*--------------*****".   
   GET FIRST BRW_RES NO-LOCK.
   DO WHILE AVAILABLE dbcompresult:
      CREATE tidut.
      SUBSTRING(tidut.UT,5) = dbcompresult.DBNAME1.
      SUBSTRING(tidut.UT,20) = dbcompresult.DBNAME2.
      SUBSTRING(tidut.UT,35) = dbcompresult.FVAL1.
      SUBSTRING(tidut.UT,50) = dbcompresult.FVAL2.
      GET NEXT BRW_RES NO-LOCK.
   END.   
   SYSTEM-DIALOG PRINTER-SETUP UPDATE utskriv.
   IF utskriv = TRUE THEN DO:      
      RUN EKLOGS.P.    
      skrivut = FALSE.      
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE start_UI C-Win 

PROCEDURE start_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   orgdb = "rt9"
   klar = TRUE
   stoppa = FALSE.
   IF NOT CONNECTED(orgdb) THEN DO:
      FIND FIRST valdbtemp WHERE valdbtemp.DBNAMN = orgdb NO-ERROR.
      CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.
   END.
   BRW_DB:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   RELEASE valdbtemp.
   tempkommando = orgdb + "._FILE".
   CREATE BUFFER orgbufh FOR TABLE tempkommando.
   CREATE QUERY orgqh.
   tempkommando = "FOR EACH " + orgdb + "._FILE NO-LOCK BY _File-Name.". 
   orgqh:SET-BUFFERS(orgbufh).
   orgqh:QUERY-PREPARE(tempkommando).
   orgqh:QUERY-OPEN.
   orgqh:GET-FIRST(NO-LOCK).
   DO WHILE orgqh:QUERY-OFF-END = FALSE:
      orgfieldh = orgbufh:BUFFER-FIELD("_File-Name").
      IF SUBSTRING(orgfieldh:BUFFER-VALUE,1,1) NE "_" THEN DO:
         CREATE filetemp.
         ASSIGN
         filetemp.FILENAME = orgfieldh:BUFFER-VALUE
         filetemp.FILEREC = orgbufh:RECID.
      END.      
      orgqh:GET-NEXT(NO-LOCK).
   END.
   orgqh:QUERY-CLOSE.
   DELETE OBJECT orgqh.
   DELETE OBJECT orgbufh.   
   tempkommando = orgdb + "._FIELD".
   CREATE BUFFER orgbufh FOR TABLE tempkommando.
   CREATE QUERY orgqh.
   tempkommando = "FOR EACH " + orgdb + "._FIELD NO-LOCK BY _File-recid.". 
   orgqh:SET-BUFFERS(orgbufh).
   orgqh:QUERY-PREPARE(tempkommando).
   orgqh:QUERY-OPEN.
   orgqh:GET-FIRST(NO-LOCK).
   DO WHILE orgqh:QUERY-OFF-END = FALSE:
      CREATE fieldtemp.
      orgfieldh = orgbufh:BUFFER-FIELD("_Field-Name").
      fieldtemp.FIELDNAME = orgfieldh:BUFFER-VALUE.   
      orgfieldh = orgbufh:BUFFER-FIELD("_File-recid").
      fieldtemp.FILEREC = orgfieldh:BUFFER-VALUE.
      orgqh:GET-NEXT(NO-LOCK).
   END.
   orgqh:QUERY-CLOSE.
   DELETE OBJECT orgqh.
   DELETE OBJECT orgbufh.   
   
/*    DISCONNECT VALUE(orgdb) NO-ERROR. */
/*    DELETE ALIAS VALUE(orgdb).        */
   CREATE SERVER apphand.
/*    RUN openbdynspec_UI IN brwproc[1]. */
   RUN openbdynspec_UI IN brwproc[3].
   APPLY "VALUE-CHANGED" TO BRW_TAB IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE tabevent_UI C-Win 

PROCEDURE tabevent_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER textmedd AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR dbcompresult.
   MESSAGE " SDFSD " textmedd VIEW-AS ALERT-BOX.
   IF textmedd = "" THEN DO:
      FIND LAST dbcompresult.
      EDT-LOG:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = EDT-LOG:SCREEN-VALUE + 
         dbcompresult.DBNAME1 + " " + dbcompresult.DBNAME2 + CHR(10).
      RUN openbdynspec_UI IN brwproc[5].
      IF stoppa = FALSE THEN RUN kopieratab_UI.
      ELSE DO:
         ASSIGN
         EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Jämförning avbruten!" + CHR(10)
         stoppa = FALSE
         klar = TRUE.
         RUN visagom_UI.
      END.
   END.
   ELSE DO:
      klar = TRUE.
      RUN visagom_UI.
      EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Klar!" + CHR(10).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE visagom_UI C-Win 

PROCEDURE visagom_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF klar = TRUE THEN DO:
      ENABLE BTN_OK BTN_AVB BTN-SKRIV WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_STOP WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ENABLE BTN_STOP WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_OK BTN_AVB BTN-SKRIV WITH FRAME {&FRAME-NAME}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

