&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ADMDYNBROWSE.W
SKA EJ GÅ ATT KOMPILERA I WEB
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
DEFINE VARIABLE extrah AS HANDLE NO-UNDO.
DEFINE VARIABLE    table-name  AS HANDLE.
DEFINE VARIABLE    col-handle  AS HANDLE.
DEFINE VARIABLE    fld-handle  AS HANDLE.
      
DEFINE VARIABLE    query-value AS CHARACTER.
  
DEFINE VARIABLE    cntr        AS INTEGER.
DEFINE VARIABLE  query-phrase AS CHARACTER.
DEFINE VARIABLE  where-phrase AS CHARACTER.
DEFINE VARIABLE  sort-phrase  AS CHARACTER.
DEFINE VARIABLE  fieldlist    AS CHARACTER.

DEFINE VARIABLE   dyn-query   AS HANDLE.
DEFINE VARIABLE   dyn-browse  AS HANDLE.
DEFINE VARIABLE   dyn-queryfile   AS HANDLE.
DEFINE VARIABLE   dyn-browsefile  AS HANDLE.
DEFINE VARIABLE   filh            AS HANDLE.
DEFINE VARIABLE   brwprocfile     AS HANDLE.
DEFINE VARIABLE   brwproc2file    AS HANDLE.
DEFINE VARIABLE   col-handlefile  AS HANDLE.

DEFINE VARIABLE fClobh AS HANDLE NO-UNDO EXTENT 100. /*FÖR ATT * SÄTTA FORMAT PÅ EXTENT FÄLT FÖR CLOB*/
DEFINE VARIABLE clobrak AS INTEGER NO-UNDO.  
DEFINE VARIABLE fclobrak  AS CHARACTER NO-UNDO EXTENT 100.  
  
DEFINE TEMP-TABLE coltemp
   FIELD PROCH AS HANDLE
   FIELD CH AS HANDLE
   FIELD PROCNAME AS CHARACTER
   FIELD CNAME AS CHARACTER
   FIELD DTYP AS CHARACTER
   FIELD NUM AS INTEGER
   INDEX NUM AS PRIMARY NUM.
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.

/*
/*{EGENBNS.I}*/
*/
DEFINE VARIABLE uppfaltnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE uppfaltvarde AS CHARACTER NO-UNDO.
DEFINE VARIABLE brwh AS HANDLE NO-UNDO.
DEFINE VARIABLE brwproc AS HANDLE NO-UNDO.
DEFINE VARIABLE brwproc2 AS HANDLE NO-UNDO.
DEFINE VARIABLE varsum AS INTEGER NO-UNDO.
DEFINE VARIABLE brwfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE exbrwfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE exfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE newfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE indexvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ttname AS CHARACTER NO-UNDO.
DEFINE VARIABLE tabname AS CHARACTER NO-UNDO.
DEFINE VARIABLE okvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare  AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE faltnamn AS CHARACTER EXTENT 20 NO-UNDO. /*FÖR ATT KUNA SKAPA NYA POSTER MED FÄLT MED EXTENT*/
DEFINE VARIABLE faltvarde AS CHARACTER EXTENT 20 NO-UNDO.
DEFINE VARIABLE nyvarden AS CHARACTER NO-UNDO.
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.

Guru.Konstanter:globforetag = FORETAG.FORETAG.
{FORESTYR.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 S-Tables S-fields S-fields-sort ~
S-fields-view FILL-IN-LOCK TOG_HIDDE TOG_UPP Btn-rpt ED_UPP E-query ~
BtnClear Btninfo BtnIndex Btn-NY FILL-IN-UPP BTN_FLYTTA Btn-clobblob ~
Btn-admdb Btn-BORT FILL-IN-startc FILL-IN-TILL Btn-upp Btn-Done 
&Scoped-Define DISPLAYED-OBJECTS S-Tables S-fields S-fields-sort ~
S-fields-view FILL-IN-LOCK TOG_HIDDE TOG_UPP ED_UPP E-query FILL-IN-UPP ~
FILL-IN-startc FILL-IN-TILL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-admdb DEFAULT 
     LABEL "AdminDb" 
     SIZE 11 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn-BORT DEFAULT 
     LABEL "Ta bort" 
     SIZE 11 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn-clobblob DEFAULT 
     LABEL "BLOBCLOB" 
     SIZE 11 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn-Done DEFAULT 
     LABEL "&Done" 
     SIZE 11 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn-NY DEFAULT 
     LABEL "Ny post" 
     SIZE 11 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn-rpt DEFAULT 
     LABEL "&Report" 
     SIZE 11 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn-upp DEFAULT 
     LABEL "Uppdatera" 
     SIZE 11 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BtnClear 
     LABEL "&Clear" 
     SIZE 11 BY 1.13.

DEFINE BUTTON BtnIndex 
     LABEL "Index" 
     SIZE 11 BY 1.13.

DEFINE BUTTON Btninfo 
     LABEL "Where" 
     SIZE 11 BY 1.13.

DEFINE BUTTON BTN_FLYTTA 
     LABEL "Flytta" 
     SIZE 11 BY 1.13.

DEFINE VARIABLE E-query AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 77 BY 4.17
     FONT 2 NO-UNDO.

DEFINE VARIABLE ED_UPP AS CHARACTER INITIAL "1.Togla Uppdatera 2.Marker fält ovan och dubbelklick 3.Markera 4.Tryck Uppdatera" 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 22.5 BY 4
     FONT 7 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOCK AS CHARACTER FORMAT "X(256)":U INITIAL "NO-LOCK" 
     VIEW-AS FILL-IN 
     SIZE 12.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-startc AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Flytta kolumn från" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TILL AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UPP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 85 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.63 BY 9.17.

DEFINE VARIABLE S-fields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 6.67 NO-UNDO.

DEFINE VARIABLE S-fields-sort AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 6.67 NO-UNDO.

DEFINE VARIABLE S-fields-view AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 6.67 NO-UNDO.

DEFINE VARIABLE S-Tables AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 28 BY 6.04 NO-UNDO.

DEFINE VARIABLE TOG_HIDDE AS LOGICAL INITIAL NO 
     LABEL "Hidden tables" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_UPP AS LOGICAL INITIAL NO 
     LABEL "Uppdatera markerade" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .79 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     S-Tables AT ROW 1.96 COL 4 NO-LABEL
     S-fields AT ROW 1.96 COL 35 NO-LABEL
     S-fields-sort AT ROW 1.96 COL 62 NO-LABEL
     S-fields-view AT ROW 1.96 COL 89 NO-LABEL
     FILL-IN-LOCK AT ROW 8.25 COL 6.5 COLON-ALIGNED NO-LABEL
     TOG_HIDDE AT ROW 8.71 COL 22.5 WIDGET-ID 2
     TOG_UPP AT ROW 8.71 COL 42 WIDGET-ID 4
     Btn-rpt AT ROW 9.04 COL 106.5
     ED_UPP AT ROW 9.5 COL 82 NO-LABEL WIDGET-ID 14
     E-query AT ROW 9.58 COL 4 NO-LABEL
     BtnClear AT ROW 10.13 COL 106.5
     Btninfo AT ROW 11.25 COL 106.5
     BtnIndex AT ROW 12.33 COL 106.5
     Btn-NY AT ROW 13.42 COL 106.5 WIDGET-ID 16
     FILL-IN-UPP AT ROW 13.5 COL 3.5 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     BTN_FLYTTA AT ROW 14.5 COL 59
     Btn-clobblob AT ROW 14.5 COL 71 WIDGET-ID 10
     Btn-admdb AT ROW 14.5 COL 83 WIDGET-ID 12
     Btn-BORT AT ROW 14.5 COL 106.5
     FILL-IN-startc AT ROW 14.63 COL 20.5 COLON-ALIGNED
     FILL-IN-TILL AT ROW 14.63 COL 38.5 COLON-ALIGNED
     Btn-upp AT ROW 15.58 COL 106.5 WIDGET-ID 6
     Btn-Done AT ROW 16.71 COL 106.5
     "Sort" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 1 COL 67
     "View" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 1 COL 94
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 1 COL 39
     "Tables" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 1 COL 8
     RECT-3 AT ROW 9.08 COL 105
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.13 ROW 1.04
         SIZE 119 BY 27.88.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Smart Query Generator"
         HEIGHT             = 27.88
         WIDTH              = 119
         MAX-HEIGHT         = 31.63
         MAX-WIDTH          = 143.38
         VIRTUAL-HEIGHT     = 31.63
         VIRTUAL-WIDTH      = 143.38
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Smart Query Generator */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Smart Query Generator */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-admdb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-admdb C-Win
ON CHOOSE OF Btn-admdb IN FRAME DEFAULT-FRAME /* AdminDb */
DO:
   RUN startadmdb.w.
   
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-BORT C-Win
ON CHOOSE OF Btn-BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
  MESSAGE "Vill du verkligen ta bort posterna?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE valborl2 AS LOGICAL.
   CASE valborl2:
      WHEN TRUE THEN DO:
      END.
      WHEN FALSE THEN DO:
         RETURN NO-APPLY.
      END.      
   END CASE.
  
   ASSIGN
   antal_valda = dyn-browse:NUM-SELECTED-ROWS
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = dyn-browse:FETCH-SELECTED-ROW(antal_raknare).                      
      DO TRANSACTION:
         dyn-query:GET-CURRENT(EXCLUSIVE-LOCK).  
         table-name:BUFFER-DELETE().
               
      END.
      antal_raknare = antal_raknare + 1.   
   END.
   
   /*
   dyn-query:GET-FIRST(NO-LOCK).  
   DO WHILE table-name:AVAILABLE:        
      DO TRANSACTION:
         dyn-query:GET-CURRENT(EXCLUSIVE-LOCK).  
         table-name:BUFFER-DELETE().      
      END.
      dyn-query:GET-NEXT(NO-LOCK).        
   END.  */
   dyn-query:QUERY-CLOSE(). 
   dyn-query:QUERY-OPEN().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-clobblob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-clobblob C-Win
ON CHOOSE OF Btn-clobblob IN FRAME DEFAULT-FRAME /* BLOBCLOB */
DO:
   DEFINE VARIABLE utfillong AS LONGCHAR NO-UNDO.
   DEFINE VARIABLE infillong AS MEMPTR NO-UNDO.
   DEFINE VARIABLE sparfilnamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE colbvar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE clobv AS INTEGER NO-UNDO.
   DEFINE VARIABLE table-name2 AS HANDLE NO-UNDO.
  DEFINE VARIABLE wherevar AS CHARACTER NO-UNDO. 
   sparfilnamn = SESSION:TEMP-DIRECTORY.
   wherevar = e-query:screen-value.
   IF INDEX(wherevar,"Where phrase") = 0 THEN where-phrase = "".
  ELSE DO:                                         
     wherevar = REPLACE(wherevar,CHR(10),"").                                                     
     wherevar = SUBSTRING(wherevar,INDEX(wherevar,"Where phrase") + 14).
     wherevar = REPLACE(wherevar,"Where phrase ="," AND "). 
     where-phrase = wherevar.
  END.
  
   MESSAGE  filh:BUFFER-FIELD("_File-Name"):BUFFER-VALUE SKIP
   "sparas i " sparfilnamn  where-phrase
   VIEW-AS ALERT-BOX.
       
   IF filh:BUFFER-FIELD("_File-Name"):BUFFER-VALUE = "BBLOBDATA" THEN DO:
      
      wherevar = TRIM(REPLACE(wherevar,"BLOBID = ","")).
      RUN bblobsok_UI IN  extrah (INPUT wherevar).
      /*
      FIND FIRST BBLOBDATA WHERE BBLOBDATA.BLOBID = INTEGER(wherevar) NO-LOCK NO-ERROR.
      FIND FIRST BLOBINFO WHERE BLOBINFO.ID = INTEGER(wherevar) NO-LOCK NO-ERROR.
      infillong = BBLOBDATA.DATA.
      sparfilnamn = sparfilnamn + BLOBINFO.FILNAMN.
      COPY-LOB FROM infillong TO FILE sparfilnamn.
      */
   END.
   ELSE IF filh:BUFFER-FIELD("_File-Name"):BUFFER-VALUE = "CLOBDATA" THEN DO:
      wherevar = TRIM(REPLACE(wherevar,"CLOBID = ","")).
      RUN clobsok_UI IN  extrah (INPUT wherevar).
      /*
      FIND FIRST CLOBDATA WHERE CLOBDATA.CLOBID = INTEGER(wherevar) NO-LOCK NO-ERROR.
      
      utfillong = CLOBDATA.CDATA.
      /*table-name:BUFFER-FIELD("CDATA"):BUFFER-VALUE.
      */
      sparfilnamn = sparfilnamn + "gurufil.txt".
      COPY-LOB FROM utfillong TO FILE sparfilnamn.
      */
   END. 
   ELSE DO:
      
      RUN CLOBUT_UI (OUTPUT colbvar).
      
      IF colbvar = FALSE THEN DO:
         wherevar = TRIM(REPLACE(wherevar,"BLOBID = ","")).
         RUN BBUT_UI IN  extrah (INPUT wherevar).
         /*
         FIND FIRST BBLOBDATA WHERE BBLOBDATA.BLOBID = INTEGER(wherevar) NO-LOCK NO-ERROR.
         FIND FIRST BLOBINFO WHERE BLOBINFO.ID = INTEGER(wherevar) NO-LOCK NO-ERROR.
         infillong = BBLOBDATA.DATA.
         sparfilnamn = sparfilnamn + BLOBINFO.FILNAMN.
         COPY-LOB FROM infillong TO FILE sparfilnamn.
         */
      END.
      ELSE DO:
         wherevar = TRIM(REPLACE(wherevar,"CLOBID = ","")).
         RUN CBUT_UI IN  extrah (INPUT wherevar).
         /*
         FIND FIRST CLOBDATA WHERE CLOBDATA.CLOBID = INTEGER(wherevar) NO-LOCK NO-ERROR.
         
         utfillong = CLOBDATA.CDATA.
         /*table-name:BUFFER-FIELD("CDATA"):BUFFER-VALUE.
         */
         sparfilnamn = sparfilnamn + "gurufil.txt".
         COPY-LOB FROM utfillong TO FILE sparfilnamn.
         */
      END.      
   END.     
   
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Done C-Win
ON CHOOSE OF Btn-Done IN FRAME DEFAULT-FRAME /* Done */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-NY C-Win
ON CHOOSE OF Btn-NY IN FRAME DEFAULT-FRAME /* Ny post */
DO:
  DEFINE VARIABLE vardei AS INTEGER NO-UNDO.
  DEFINE VARIABLE lasthit AS INTEGER NO-UNDO.
  DEFINE VARIABLE nexthit AS INTEGER NO-UNDO.
  DEFINE VARIABLE browrow AS ROWID NO-UNDO.
  faltnamn = "".
  faltvarde  = "".
  vardei = 0.  
  nexthit = 1.
  lasthit = 1.
  IF INDEX(nyvarden,"Where phrase") = 0 THEN.
  ELSE DO:
     nyvarden = REPLACE(nyvarden,"Where phrase"," ").
     REPEAT:
        vardei = vardei + 1.
        IF INDEX(nyvarden,"=",lasthit) > 0 THEN DO:
           lasthit = INDEX(nyvarden,"=",lasthit).
           nexthit = INDEX(nyvarden,"=",lasthit + 1).
           faltnamn[vardei] = TRIM(SUBSTRING(nyvarden,lasthit + 1,nexthit - (lasthit + 1))).
              
           lasthit = nexthit.
           nexthit = INDEX(nyvarden,"=",lasthit + 1).
           IF nexthit = 0 THEN DO:
              faltvarde[vardei] = TRIM(SUBSTRING(nyvarden,lasthit + 1)).
              faltvarde[vardei] = REPLACE(faltvarde[vardei],"'","").
              LEAVE.
           END.     
           faltvarde[vardei] = TRIM(SUBSTRING(nyvarden,lasthit + 1,nexthit - (lasthit + 1))).
           faltvarde[vardei] = REPLACE(faltvarde[vardei],"'","").
           lasthit = nexthit.
        END.      
     END. 
  END. 
  
  MESSAGE "Vill du skapa en ny post i " table-name:TABLE " med värderna?" SKIP 
  faltnamn[1] "="  faltvarde[1]  SKIP
  faltnamn[2] "="  faltvarde[2]  SKIP
  faltnamn[3] "="  faltvarde[3]  SKIP
  faltnamn[4] "="  faltvarde[4]  SKIP
  faltnamn[5] "="  faltvarde[5]  SKIP
  faltnamn[6] "="  faltvarde[6]  SKIP
  faltnamn[7] "="  faltvarde[7]  SKIP
  faltnamn[8] "="  faltvarde[8]  SKIP
  faltnamn[9] "="  faltvarde[9]  SKIP
  faltnamn[10] "=" faltvarde[10] SKIP
  faltnamn[11] "=" faltvarde[11] SKIP
  faltnamn[12] "=" faltvarde[12] SKIP
  faltnamn[13] "=" faltvarde[13] SKIP
  faltnamn[14] "=" faltvarde[14] SKIP
  faltnamn[15] "=" faltvarde[15] SKIP
  faltnamn[16] "=" faltvarde[16] SKIP
  faltnamn[17] "=" faltvarde[17] SKIP
  faltnamn[18] "=" faltvarde[18] SKIP
  faltnamn[19] "=" faltvarde[19] SKIP
  faltnamn[20] "=" faltvarde[20] SKIP
   
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE valborl2 AS LOGICAL.
   CASE valborl2:
      WHEN TRUE THEN DO:
      END.
      WHEN FALSE THEN DO:
         RETURN NO-APPLY.
      END.      
   END CASE.  
   DO TRANSACTION: 
      table-name:BUFFER-CREATE().
      browrow = table-name:ROWID.  
      vardei = 0.  
      REPEAT:     
         vardei = vardei + 1.
         IF vardei > 20 THEN LEAVE.
         IF faltnamn[vardei] = "" THEN LEAVE.
         table-name:BUFFER-FIELD(faltnamn[vardei]):BUFFER-VALUE = faltvarde[vardei].
          
      END.
   END.   
   dyn-query:QUERY-CLOSE(). 
   dyn-query:QUERY-OPEN().
   RUN repodyn_UI IN brwproc2 (INPUT browrow).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-rpt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-rpt C-Win
ON CHOOSE OF Btn-rpt IN FRAME DEFAULT-FRAME /* Report */
DO:
  DEFINE VARIABLE wherevar AS CHARACTER NO-UNDO.
  DEFINE VARIABLE arraycounter AS INTEGER NO-UNDO.
  DEFINE VARIABLE tabfalth AS HANDLE NO-UNDO.
  DEFINE VARIABLE colfieldh AS HANDLE NO-UNDO.
  IF dyn-query NE ? THEN DO:
     dyn-query:QUERY-OPEN().
     dyn-query:QUERY-CLOSE(). 
     dyn-query = ?.
     table-name = ?.
     dyn-browse  = ?.
     
     /*
     create buffer table-name for table "afil".
     create query dyn-query.
     dyn-query:set-buffers(table-name).
     dyn-query:query-prepare("FOR EACH AFIL"). 
     col-handle = dyn-browse:add-like-column("AFIL.NAMN").
     dyn-query:query-open().
     dyn-query:QUERY-CLOSE(). 
     dyn-query = ?.
     */
  END.
  /* get the name from the tables selection list */
  CREATE BUFFER table-name FOR TABLE STRING(filh:BUFFER-FIELD("_File-Name"):BUFFER-VALUE).
  CREATE QUERY dyn-query.
  
  /* set the query and create the browse */
  dyn-query:SET-BUFFERS(table-name).
  CREATE BROWSE dyn-browse
  ASSIGN 
  WIDTH      = 110
  HEIGHT     = 10
  EXPANDABLE = YES
  COLUMN     = 4
  ROW        = 18.25
  FRAME      = FRAME {&frame-name}:handle
  MULTIPLE   = TRUE
  READ-ONLY  = FALSE
  SENSITIVE  = TRUE
  TITLE =    "sökta poster".
  dyn-browse:COLUMN-RESIZABLE = TRUE.
  dyn-browse:COLUMN-SCROLLING = TRUE.
  /* attach the query to the browser */
  dyn-browse:QUERY = dyn-query.
  /* query value is the string which will be used to open the query */
  FILL-IN-LOCK = INPUT FILL-IN-LOCK.
  /*
  query-phrase = query-phrase + " " + INPUT FILL-IN-LOCK.
  */
  query-value = query-phrase + " " + FILL-IN-LOCK.
  RUN extratrigg.
  
  wherevar = e-query:screen-value.
  IF INDEX(wherevar,"Where phrase") = 0 THEN where-phrase = "".
  ELSE DO:                                         
     wherevar = REPLACE(wherevar,CHR(10),"").                                                     
     nyvarden = SUBSTRING(wherevar,INDEX(wherevar,"Where phrase") - 14).
     wherevar = SUBSTRING(wherevar,INDEX(wherevar,"Where phrase") + 14).
     
     wherevar = REPLACE(wherevar,"Where phrase ="," AND "). 
     wherevar = REPLACE(wherevar,"Sort phrase =", " ").
     
     where-phrase = wherevar.
  END.
  
  /*lopa edit*/
  IF where-phrase <> "" THEN query-value = query-value + " where " + where-phrase.
  IF sort-phrase  <> "" THEN query-value = query-value + " by "    + sort-phrase.
  
  /* prepare and open the query */ 
  MESSAGE query-value "Är query´n rätt?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE valborl AS LOGICAL.
  CASE valborl:
      WHEN TRUE THEN DO:
      END.
      WHEN FALSE THEN DO:
         RETURN NO-APPLY.
      END.      
  END CASE.
  clobrak = 0.
  DO cntr = 1 TO NUM-ENTRIES(s-fields-view:SCREEN-VALUE):
     col-handle = table-name:BUFFER-FIELD(ENTRY(cntr,s-fields-view:SCREEN-VALUE)).
     IF col-handle:DATA-TYPE = "CLOB" THEN DO:
        fClobh[cntr] = dyn-browse:ADD-CALC-COLUMN("char", "x(256)","",col-handle:NAME,cntr).
        fclobrak[cntr] = col-handle:NAME.
        clobrak = clobrak + 1.
        fClobh[cntr]:READ-ONLY = YES.
     END.  
    
     ELSE DO:
        col-handle = dyn-browse:ADD-LIKE-COLUMN(filh:BUFFER-FIELD("_File-Name"):BUFFER-VALUE + "." + ENTRY(cntr,s-fields-view:SCREEN-VALUE)).
        col-handle:READ-ONLY = NO.
     END.  
  END.
  
  RUN readonly.
  ASSIGN
  dyn-browse:SENSITIVE   = YES
  dyn-browse:VISIBLE     = YES.
  DEFINE VARIABLE rowdispvar AS LOGICAL NO-UNDO.
   
   APPLY "home" TO dyn-browse.
   RUN DYNTTSKARP.P PERSISTENT SET brwproc (INPUT dyn-browse,INPUT FRAME {&FRAME-NAME}:HANDLE). 
   RUN DYNBRW.P PERSISTENT SET brwproc2 (INPUT dyn-browse).
   col-handle = dyn-browse:GET-BROWSE-COLUMN(1).
   IF clobrak > 0 THEN DO:
      RUN dynprogextra IN brwproc2 (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
      RUN rowdispextrakor IN brwproc2 (INPUT TRUE).
   END. 
   arraycounter = 1.
   DO WHILE VALID-HANDLE(col-handle):
      IF col-handle:DATA-TYPE = "CHARACTER" THEN DO:
         col-handle:WIDTH-CHARS = 30.
         tabfalth = table-name:BUFFER-FIELD(col-handle:NAME).
         IF col-handle:NAME = ? THEN RUN setrowprocA_UI IN brwproc2 (arraycounter,INPUT THIS-PROCEDURE, INPUT "format256_UI", col-handle:LABEL).
         ELSE                        RUN setrowprocA_UI IN brwproc2 (arraycounter,INPUT THIS-PROCEDURE, INPUT "format256_UI", col-handle:NAME).
         
      END.
      IF col-handle:DATA-TYPE = "INTEGER" THEN DO:
         RUN setrowprocA_UI IN brwproc2 (arraycounter,INPUT THIS-PROCEDURE, INPUT "format256_UI", col-handle:NAME). 
      END.
      IF col-handle:DATA-TYPE = "DECIMAL" THEN DO:
         RUN setrowprocA_UI IN brwproc2 (arraycounter,INPUT THIS-PROCEDURE, INPUT "format256_UI", col-handle:NAME). 
      END.
      IF col-handle:DATA-TYPE = "CLOBDATA" THEN DO:
         col-handle:WIDTH-CHARS = 256.
         RUN setrowprocA_UI IN brwproc2 (arraycounter,INPUT THIS-PROCEDURE, INPUT "format256_UI", col-handle:NAME). 
      END.
      col-handle = col-handle:NEXT-COLUMN. 
      arraycounter = arraycounter + 1.        
   END.
   DO TRANSACTION:
      dyn-query:QUERY-PREPARE(query-value). 
      dyn-query:QUERY-OPEN().
      RUN settrigger_UI IN brwproc (INPUT 1,INPUT "rowleave_UI").
      IF where-phrase = "" THEN wherevar = wherevar.
      ELSE DO:
         wherevar = " WHERE " + wherevar . 
         RUN setcolsortvar_UI IN brwproc2 (INPUT wherevar).                
      END.
   END.
   RUN title_UI IN brwproc2.
   RUN getfirst_UI IN brwproc2.
   RUN lastselectdyn_UI IN brwproc2.
   
   colfieldh = dyn-browse:GET-BROWSE-COLUMN(1).
   IF colfieldh:NAME = ? THEN tabfalth = table-name:BUFFER-FIELD(colfieldh:LABEL).
   ELSE tabfalth = table-name:BUFFER-FIELD(colfieldh:NAME).
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-upp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-upp C-Win
ON CHOOSE OF Btn-upp IN FRAME DEFAULT-FRAME /* Uppdatera */
DO: 
   
  MESSAGE "Vill du verkligen UPDDATERA MARKERADE POSTER MED "  FILL-IN-UPP:SCREEN-VALUE "?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE valborl2 AS LOGICAL.
   CASE valborl2:
      WHEN TRUE THEN DO:
      END.
      WHEN FALSE THEN DO:
         RETURN NO-APPLY.
      END.      
   END CASE.
   FILL-IN-UPP = FILL-IN-UPP:SCREEN-VALUE.
   RUN rowUPP_UI.
   dyn-query:QUERY-CLOSE(). 
   dyn-query:QUERY-OPEN().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnClear C-Win
ON CHOOSE OF BtnClear IN FRAME DEFAULT-FRAME /* Clear */
DO:
  e-query:screen-value IN FRAME {&frame-name} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnIndex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnIndex C-Win
ON CHOOSE OF BtnIndex IN FRAME DEFAULT-FRAME /* Index */
DO:
  e-query:screen-value = e-query:screen-value + 
  chr(10) + chr(10) + dyn-query:INDEX-INFORMATION(1).
  IF NUM-ENTRIES(dyn-query:INDEX-INFORMATION(1)) > 1
  AND entry(1,dyn-query:INDEX-INFORMATION(1)) = "WHOLE-INDEX"
  THEN
   e-query:screen-value = e-query:screen-value + " SCANNED THE WHOLE TABLE FOR "
   + string(dyn-query:NUM-RESULTS) + " RECORDS!".
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btninfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btninfo C-Win
ON CHOOSE OF Btninfo IN FRAME DEFAULT-FRAME /* Where */
DO:
  e-query:screen-value = e-query:screen-value + 
  chr(10) + chr(10) + dyn-query:PREPARE-STRING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FLYTTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FLYTTA C-Win
ON CHOOSE OF BTN_FLYTTA IN FRAME DEFAULT-FRAME /* Flytta */
DO:
   FILL-IN-startc = INPUT FILL-IN-startc.
   FILL-IN-TILL = INPUT FILL-IN-TILL.
   RUN movecol_UI IN brwproc (INPUT FILL-IN-startc,INPUT FILL-IN-TILL).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME S-fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL S-fields C-Win
ON MOUSE-SELECT-DBLCLICK OF S-fields IN FRAME DEFAULT-FRAME
DO:
  IF TOG_UPP = FALSE THEN DO: 
     RUN wherebldr.w (SELF:screen-value).
     
     /* return value provides a comma-delimited list: the first entry
     is the comparator sign (= >= or <=)
     the second is the actual value. But the where builder doesn't know what field,
     so we have to get the field and determine the data type */
     where-phrase = s-fields:screen-value + " " + entry(1,RETURN-VALUE). /* e.g. cust-num >= */
     FIND FIRST _field WHERE _field._FILE-RECID = RECID(_file) AND _field-name = s-fields:screen-value NO-LOCK NO-ERROR.
     IF _data-type BEGINS "ch" THEN where-phrase = where-phrase + " '" + entry(2,RETURN-VALUE) + "'".
     ELSE where-phrase = where-phrase + " " + entry(2,RETURN-VALUE).
     /* e.g. cust-num >= 4 */
     e-query:screen-value = 
     e-query:screen-value + chr(13) + 
     "Where phrase = " + where-phrase.
   END.
   ELSE DO:
      RUN whereblUPP.w (SELF:screen-value).
      uppfaltnamn = s-fields:SCREEN-VALUE.
      
      FILL-IN-UPP:SCREEN-VALUE = s-fields:screen-value + " " + entry(1,RETURN-VALUE). /* e.g. cust-num >= */
      uppfaltvarde = ENTRY(1,RETURN-VALUE).
      FIND FIRST _field WHERE _field._FILE-RECID = RECID(_file) AND _field-name = s-fields:screen-value NO-LOCK NO-ERROR.
      IF _data-type BEGINS "ch" THEN DO:
         FILL-IN-UPP:SCREEN-VALUE =  FILL-IN-UPP:SCREEN-VALUE + " '" + entry(2,RETURN-VALUE) + "'".
         /*
         uppfaltvarde = " '" + entry(2,return-value) + "'".
         */
         uppfaltvarde = STRING(ENTRY(2,RETURN-VALUE)).
      END.   
      ELSE DO:
         FILL-IN-UPP:SCREEN-VALUE =  FILL-IN-UPP:SCREEN-VALUE + " " + entry(2,RETURN-VALUE).
         uppfaltvarde = STRING(ENTRY(2,RETURN-VALUE)).
      END.   
      FILL-IN-UPP = FILL-IN-UPP:SCREEN-VALUE.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME S-fields-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL S-fields-sort C-Win
ON MOUSE-SELECT-DBLCLICK OF S-fields-sort IN FRAME DEFAULT-FRAME
DO:
  IF s-fields-sort:screen-value <> "" THEN
  sort-phrase = "by " + s-fields-sort:screen-value.
  
  .
  sort-phrase = s-fields-sort:screen-value.

  e-query:screen-value = 
  e-query:screen-value + chr(13) + 
  "Sort phrase = by " + sort-phrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME S-Tables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL S-Tables C-Win
ON MOUSE-MENU-CLICK OF S-Tables IN FRAME DEFAULT-FRAME
DO:

   APPLY "MOUSE-SELECT-DBLCLICK" TO S-Tables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL S-Tables C-Win
ON MOUSE-SELECT-DBLCLICK OF S-Tables IN FRAME DEFAULT-FRAME
DO:
  query-phrase = "for each " + s-tables:screen-value.
  e-query:screen-value = "Query phrase =  '" + query-phrase + "'".
  . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL S-Tables C-Win
ON VALUE-CHANGED OF S-Tables IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
  s-fields:list-items = ""
  s-fields-sort:list-items = ""
  s-fields-view:list-items = "".
  FIND _file NO-LOCK WHERE _file-name = s-tables:screen-value.
  FOR EACH _field NO-LOCK OF _file:

    s-fields:add-last(_field-name).
    s-fields-sort:add-last(_field-name).
    s-fields-view:add-last(_field-name).
  END.
  DISPLAY s-fields s-fields-sort s-fields-view 
  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_HIDDE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_HIDDE C-Win
ON VALUE-CHANGED OF TOG_HIDDE IN FRAME DEFAULT-FRAME /* Hidden tables */
DO:
   TOG_HIDDE = INPUT TOG_HIDDE.
   RUN load-tables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_UPP C-Win
ON VALUE-CHANGED OF TOG_UPP IN FRAME DEFAULT-FRAME /* Uppdatera markerade */
DO:
   TOG_UPP = INPUT TOG_UPP.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   DEFINE VARIABLE dbi AS INTEGER NO-UNDO. 
   dbi = 1.
   SETUSERID({setuser.I},{setpwd.I}).
   DO WHILE dbi <= NUM-DBS:
      MESSAGE PDBNAME(dbi)
      VIEW-AS ALERT-BOX.
       
      IF PDBNAME(dbi) NE "DARPLU" THEN DO: 
         RUN ADMINDYNBRWEXTRA.P PERSISTENT SET extrah.
      END. 
      dbi = dbi + 1. 
   END.  
     
   
   RUN load-tables.
   CREATE BUFFER filh FOR TABLE "_file".
   CREATE QUERY dyn-queryfile.
  
   /* set the query and create the browse */
   dyn-queryfile:SET-BUFFERS(filh).
   CREATE BROWSE dyn-browsefile
      ASSIGN 
      
      WIDTH      = 30
      HEIGHT     = 7
      EXPANDABLE = NO
      COLUMN     = 4
      ROW        = 1
      FRAME      = FRAME {&frame-name}:handle
       ROW-MARKERS = NO
      MULTIPLE   = FALSE
      READ-ONLY  = TRUE
      SENSITIVE  = TRUE
      TITLE =    "sökta poster".
   dyn-browsefile:SCROLLBAR-VERTICAL = FALSE.
   dyn-browsefile:COLUMN-RESIZABLE = FALSE.
   dyn-browsefile:COLUMN-SCROLLING = FALSE.
   dyn-browsefile:VISIBLE    = YES.
   /* attach the query to the browser */
   dyn-browsefile:QUERY = dyn-queryfile.
   col-handlefile = dyn-browsefile:ADD-LIKE-COLUMN("_File._File-Name").
   col-handlefile:WIDTH = 25.
   RUN DYNTTSKARP.P PERSISTENT SET brwprocfile (INPUT dyn-browsefile,INPUT FRAME {&FRAME-NAME}:HANDLE). 
   RUN DYNBRW.P PERSISTENT SET brwproc2file (INPUT dyn-browsefile).
   ON 'MOUSE-SELECT-DBLCLICK' OF dyn-browsefile  PERSISTENT RUN dbclickfile  IN THIS-PROCEDURE.
   ON 'VALUE-CHANGED' OF dyn-browsefile PERSISTENT RUN vbch IN THIS-PROCEDURE.
   
   dyn-queryfile:QUERY-PREPARE("FOR EACH _file where _file._file-number > 0 AND _file._file-number < 32000  NO-LOCK by _file-name"). 
   dyn-queryfile:QUERY-OPEN().
   ed_upp = "1.Togla Uppdatera" + chr(10) + 
   "2.Marker fält ovan och dubbelklick" + chr(10) +
   "3.Markera" + chr(10) +
   "4.Tryck Uppdatera".
   RUN enable_UI.
   
   {musarrow.i}
      /*
   IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
   END.
   */
   s-tables:screen-value IN FRAME {&FRAME-NAME} = entry(1,s-tables:list-items).
   s-tables:VISIBLE = FALSE.
   APPLY "value-changed" TO s-tables.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CLOBUT_UI C-Win 
PROCEDURE CLOBUT_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT  PARAMETER clobblob AS LOGICAL NO-UNDO.
   
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE colfieldh AS HANDLE NO-UNDO.
   DEFINE VARIABLE colfieldhj AS HANDLE NO-UNDO.
   DEFINE VARIABLE tabfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE arraycounter AS INTEGER NO-UNDO.
   
   ASSIGN
   tempcounter = 1
   arraycounter = 1
   antal_valda = dyn-browse:NUM-SELECTED-ROWS
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = dyn-browse:FETCH-SELECTED-ROW(antal_raknare).                      
      DO TRANSACTION:
         dyn-query:GET-CURRENT(EXCLUSIVE-LOCK).                                      
         DO WHILE tempcounter LE dyn-browse:NUM-COLUMNS:
            colfieldh = dyn-browse:GET-BROWSE-COLUMN(tempcounter).
            IF colfieldhj NE ? THEN DO:
               IF colfieldhj:NAME = colfieldh:NAME THEN arraycounter = arraycounter + 1.
               ELSE DO:
                  colfieldhj = colfieldh.
                  arraycounter = 1.
               END.
            END.
            ELSE colfieldhj = colfieldh.         
            tabfalth = table-name:BUFFER-FIELD(colfieldh:NAME).
            IF "clobid" = tabfalth:NAME THEN DO:
               clobblob = TRUE.
               RETURN.               
            END. 
            ELSE IF "blobid" = tabfalth:NAME THEN DO:
               clobblob = FALSE.
               RETURN.               
            END. 
              
            tempcounter = tempcounter + 1.
         END.       
      END.
      arraycounter = 1. 
      tempcounter = 1.
      antal_raknare = antal_raknare + 1.   
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dbclickfile C-Win 
PROCEDURE dbclickfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   query-phrase = "for each " + filh:BUFFER-FIELD("_File-Name"):BUFFER-VALUE.
   e-query:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "Query phrase =  '" + query-phrase + "'".
  . 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY S-Tables S-fields S-fields-sort S-fields-view FILL-IN-LOCK TOG_HIDDE 
          TOG_UPP ED_UPP E-query FILL-IN-UPP FILL-IN-startc FILL-IN-TILL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-3 S-Tables S-fields S-fields-sort S-fields-view FILL-IN-LOCK 
         TOG_HIDDE TOG_UPP Btn-rpt ED_UPP E-query BtnClear Btninfo BtnIndex 
         Btn-NY FILL-IN-UPP BTN_FLYTTA Btn-clobblob Btn-admdb Btn-BORT 
         FILL-IN-startc FILL-IN-TILL Btn-upp Btn-Done 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extratrigg C-Win 
PROCEDURE extratrigg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(dyn-browse) THEN DO:
      ON 'VALUE-CHANGED' OF dyn-browse PERSISTENT RUN vbrw IN THIS-PROCEDURE.
      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE format C-Win 
PROCEDURE format :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*
   IF coltyp = "INTEGER" THEN colh:FORMAT = ">>>>>>>>>>>>9".      
   */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE format256_UI C-Win 
PROCEDURE format256_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cnam AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER colh AS HANDLE     NO-UNDO.
   DEFINE INPUT  PARAMETER colnamn AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER coltyp AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE arraycounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE tabfalth AS HANDLE NO-UNDO.
   IF coltyp = "CHARACTER" THEN DO:
      colh:FORMAT = "X(256)".
   END.   
   IF coltyp = "CLOB" THEN colh:FORMAT = "X(256)".
   IF coltyp = "INTEGER" THEN colh:FORMAT = "->>>>>>>>>>>>9".  
   IF coltyp = "DECIMAL" THEN colh:FORMAT = "->>>>>>>>>>9.9<<<".    
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-tables C-Win 
PROCEDURE load-tables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  s-tables:list-items IN FRAME {&FRAME-NAME} = "".
  FOR EACH _file NO-LOCK:
     IF TOG_HIDDE = FALSE THEN DO:
        IF _file-name BEGINS "_" OR _file-name BEGINS "SYS" THEN NEXT.
         s-tables:add-last(_file-name) IN FRAME {&FRAME-NAME}.
     END.
     ELSE DO:
        IF _file-name BEGINS "_" OR _file-name BEGINS "SYS" THEN s-tables:add-last(_file-name) IN FRAME {&FRAME-NAME}. 
        ELSE NEXT.
         
     END.       
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readonly C-Win 
PROCEDURE readonly :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE colfieldh AS HANDLE NO-UNDO.
   DEFINE VARIABLE colfieldhj AS HANDLE NO-UNDO.
   DEFINE VARIABLE tabfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE arraycounter AS INTEGER NO-UNDO.
   tempcounter = 1.
   DO WHILE tempcounter LE dyn-browse:NUM-COLUMNS:
      colfieldh = dyn-browse:GET-BROWSE-COLUMN(tempcounter).
      colfieldh:READ-ONLY = NO.
      tempcounter = tempcounter + 1.            
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RepoDyn_UI C-Win 
PROCEDURE RepoDyn_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI C-Win 
PROCEDURE rowdispextra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   DEFINE VARIABLE calcname AS CHARACTER NO-UNDO.
   
   DO clobrak = 1 TO cntr:
     IF fClobh[clobrak] NE ? THEN DO:
         FIND FIRST coltemp WHERE coltemp.CNAME = fclobrak[clobrak] USE-INDEX NUM NO-LOCK NO-ERROR. 
         IF AVAILABLE coltemp THEN DO:
            calcname = table-name:BUFFER-FIELD(fclobrak[clobrak]):BUFFER-VALUE.
            coltemp.CH:SCREEN-VALUE = calcname.
         END.   
      END.     
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowleave_UI C-Win 
PROCEDURE rowleave_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE colfieldh AS HANDLE NO-UNDO.
   DEFINE VARIABLE colfieldhj AS HANDLE NO-UNDO.
   DEFINE VARIABLE tabfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE arraycounter AS INTEGER NO-UNDO.
   /*
   DEFINE BUFFER ed FOR EXTRADATA.
   DEFINE BUFFER ek FOR EXTRAKOPPLINGAR.
   DEFINE BUFFER xst FOR XSEK.
   DEFINE BUFFER xlt FOR XLABEL.
   */
   
   tempcounter = 1.
   arraycounter = 1.
   dyn-browse:SELECT-FOCUSED-ROW() NO-ERROR.
   DO TRANSACTION:
      dyn-query:GET-CURRENT(EXCLUSIVE-LOCK).        
      DO WHILE tempcounter LE dyn-browse:NUM-COLUMNS:
         colfieldh = dyn-browse:GET-BROWSE-COLUMN(tempcounter).
         IF colfieldhj NE ? THEN DO:
            IF colfieldhj:NAME = colfieldh:NAME THEN arraycounter = arraycounter + 1.
            ELSE DO:
               colfieldhj = colfieldh.
               arraycounter = 1.
            END.
         END.
         ELSE colfieldhj = colfieldh.
         
         tabfalth = table-name:BUFFER-FIELD(colfieldh:NAME).
         IF tabfalth:EXTENT = 0 THEN DO:
            tabfalth:BUFFER-VALUE = colfieldh:SCREEN-VALUE.       
         END.
         ELSE DO: 
            tabfalth:BUFFER-VALUE(arraycounter) = colfieldh:SCREEN-VALUE.
            
         END.

         tempcounter = tempcounter + 1.
         
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowUPP_UI C-Win 
PROCEDURE rowUPP_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE colfieldh AS HANDLE NO-UNDO.
   DEFINE VARIABLE colfieldhj AS HANDLE NO-UNDO.
   DEFINE VARIABLE tabfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE arraycounter AS INTEGER NO-UNDO.
  /*
   DEFINE BUFFER ed FOR EXTRADATA.
   DEFINE BUFFER ek FOR EXTRAKOPPLINGAR.
   */
   ASSIGN
   tempcounter = 1
   arraycounter = 1
   antal_valda = dyn-browse:NUM-SELECTED-ROWS
   antal_raknare = 1.
   DO WHILE tempcounter LE dyn-browse:NUM-COLUMNS:
       colfieldh = dyn-browse:GET-BROWSE-COLUMN(tempcounter).
       IF colfieldh:NAME = uppfaltnamn THEN tempcounter = dyn-browse:NUM-COLUMNS.
       tempcounter = tempcounter + 1.
   END.    
       
   
   
   
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = dyn-browse:FETCH-SELECTED-ROW(antal_raknare).                      
      DO TRANSACTION:
         dyn-query:GET-CURRENT(EXCLUSIVE-LOCK).                                      
         tabfalth = table-name:BUFFER-FIELD(colfieldh:NAME).
         IF uppfaltnamn = tabfalth:NAME THEN DO:

            IF tabfalth:EXTENT = 0 THEN DO:
               tabfalth:BUFFER-VALUE = uppfaltvarde.       
            END.
            ELSE DO:
               IF INDEX(FILL-IN-UPP,"[") > 0 AND INDEX(FILL-IN-UPP,"]") > 0 THEN DO:
                  arraycounter = INTEGER(SUBSTRING(FILL-IN-UPP,INDEX(FILL-IN-UPP,"[") + 1,(INDEX(FILL-IN-UPP,"]") - INDEX(FILL-IN-UPP,"[")) - 1)).
               END.
               ELSE DO:
                  MESSAGE "Du har inte anget vilken matris"
                  VIEW-AS ALERT-BOX.
                  RETURN .
               END.
               
               
               
               tabfalth:BUFFER-VALUE(arraycounter) = uppfaltvarde.
               
            END. 
  
         END.       
      END.
      arraycounter = 1. 
      tempcounter = 1.
      antal_raknare = antal_raknare + 1.   
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vbch C-Win 
PROCEDURE vbch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   dyn-browsefile:SELECT-FOCUSED-ROW() NO-ERROR.
   
   RUN setlastrowid_UI IN brwproc2file (INPUT filh:ROWID).
   ASSIGN
  s-fields:list-items IN FRAME {&FRAME-NAME} = ""
  s-fields-sort:list-items = ""
  s-fields-view:list-items = "".
  /*
  find _file no-lock where _file-name = s-tables:screen-value.
  */
  FOR EACH _field WHERE _Field._File-recid =  filh:RECID NO-LOCK:

    s-fields:add-last(_field-name).
    s-fields-sort:add-last(_field-name).
    s-fields-view:add-last(_field-name).
  END.
  FIND FIRST _file WHERE RECID(_file) = filh:RECID NO-LOCK NO-ERROR.
  DISPLAY s-fields s-fields-sort s-fields-view 
  WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vbrw C-Win 
PROCEDURE vbrw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   dyn-browse:SELECT-FOCUSED-ROW() NO-ERROR.
   dyn-query:GET-CURRENT(EXCLUSIVE-LOCK) NO-ERROR.
   RUN setlastrowid_UI IN brwproc2 (INPUT table-name:ROWID).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

