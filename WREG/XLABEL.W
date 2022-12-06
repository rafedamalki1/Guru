&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt9              PROGRESS
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 11/20/97 - 12:37 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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



DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
/*{EGENBNS.I}*/
DEFINE NEW SHARED VARIABLE xlabelrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO. 

DEFINE VARIABLE brwproc AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda2 AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare2 AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_fore AS INTEGER NO-UNDO.
DEFINE VARIABLE xlabelrec2 AS RECID NO-UNDO.
DEFINE VARIABLE xhop AS CHARACTER NO-UNDO. 
DEFINE VARIABLE xlev AS INTEGER NO-UNDO.
DEFINE VARIABLE xbort AS CHARACTER EXTENT 50 NO-UNDO.        
DEFINE VARIABLE xbort2 AS INTEGER EXTENT 50 NO-UNDO. 
DEFINE VARIABLE bort_raknare AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE xtemp 
   FIELD NYAND AS CHARACTER
   FIELD MENYVART LIKE XLABEL.MENYVART
   FIELD MENY LIKE XLABEL.MENY
   FIELD ORDNING LIKE XLABEL.ORDNING
   INDEX MENYVART IS PRIMARY MENYVART ORDNING.
DEFINE TEMP-TABLE xlabeltemp NO-UNDO LIKE XLABEL.
/*
DEf VAR fgv as char.

FIND FIRST FORETAG EXCLUSIVE-LOCK.
fgv = FORETAG.FORETAG.
   UPDATE FORETAG.FORETAG. 
find first XFOREORDNING where XFOREORDNING.FORETAG = fgv no-error.
XFOREORDNING.FORETAG = FORETAG.FORETAG.
FOR EACH XLABEL where XLABEL.FORETAG[XFOREORDNING.ordning] = fgv:
   XLABEL.FORETAG[XFOREORDNING.ordning] = FORETAG.FORETAG.
END.
XGURU.
FOR EACH OMRADETAB:
DELETE OMRADETAB.
END.
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_FOR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES XFOREORDNING xlabeltemp XLABEL

/* Definitions for BROWSE BRW_FOR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_FOR XFOREORDNING.ORDNING ~
XFOREORDNING.FORETAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FOR 
&Scoped-define QUERY-STRING-BRW_FOR FOR EACH XFOREORDNING NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FOR OPEN QUERY BRW_FOR FOR EACH XFOREORDNING NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FOR XFOREORDNING
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FOR XFOREORDNING


/* Definitions for BROWSE BRW_FORE                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FORE xlabeltemp.MENYVART xlabeltemp.MENY ~
xlabeltemp.ORDNING xlabeltemp.KOPPLING xlabeltemp.STORKOPPLING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FORE 
&Scoped-define QUERY-STRING-BRW_FORE FOR EACH xlabeltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FORE OPEN QUERY BRW_FORE FOR EACH xlabeltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FORE xlabeltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FORE xlabeltemp


/* Definitions for BROWSE BRW_LABEL                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_LABEL XLABEL.MENYVART XLABEL.MENY ~
XLABEL.ORDNING XLABEL.KOPPLING XLABEL.STORKOPPLING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LABEL 
&Scoped-define QUERY-STRING-BRW_LABEL FOR EACH XLABEL NO-LOCK ~
    BY XLABEL.MENYVART ~
       BY XLABEL.ORDNING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_LABEL OPEN QUERY BRW_LABEL FOR EACH XLABEL NO-LOCK ~
    BY XLABEL.MENYVART ~
       BY XLABEL.ORDNING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_LABEL XLABEL
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LABEL XLABEL


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_FOR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-20 BRW_FOR EDI_FOR SEL_FORE BRW_LABEL ~
BRW_FORE BTN_OVER BTN_BACK BTN_NY BTN_UPP BTN_BORT BTN_FOR BTN_NYFORE ~
BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS EDI_FOR SEL_FORE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 10.5 BY 1.5.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER/btn_back.gif":U
     LABEL "BO" 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta Bort":L 
     SIZE 10.5 BY 1.5 TOOLTIP "OM DU HAR ETT FÖRETAG MARKERAT TAS FUNKTIONEN BORT FÖR DET FÖRETAGET".

DEFINE BUTTON BTN_FOR 
     LABEL "Företag":L 
     SIZE 10.5 BY 1.5.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 10.5 BY 1.5.

DEFINE BUTTON BTN_NYFORE 
     LABEL "NY FÖRETAGSUPPLÄGG" 
     SIZE 17.75 BY 1.5 TOOLTIP "MARKERA ALLA FUNKTIONER SOM  FÖRE. SKALL HA TRYCK SEDAN HÄR OBS PÅ NY DATABAS"
     FONT 18.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER/btn_over.gif":U
     LABEL "Ny" 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 10.5 BY 1.5.

DEFINE VARIABLE EDI_FOR AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 47.63 BY 7.54 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 121.5 BY 2.5
     BGCOLOR 8 .

DEFINE VARIABLE SEL_FORE AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 10.25 BY 7.25
     BGCOLOR 8  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FOR FOR 
      XFOREORDNING SCROLLING.

DEFINE QUERY BRW_FORE FOR 
      xlabeltemp SCROLLING.

DEFINE QUERY BRW_LABEL FOR 
      XLABEL SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FOR WINDOW-1 _STRUCTURED
  QUERY BRW_FOR NO-LOCK DISPLAY
      XFOREORDNING.ORDNING FORMAT "->>>>>>9":U
      XFOREORDNING.FORETAG FORMAT "x(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 21.25 BY 7.25
         BGCOLOR 8 
         TITLE BGCOLOR 8 "FÖRETAG".

DEFINE BROWSE BRW_FORE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FORE WINDOW-1 _STRUCTURED
  QUERY BRW_FORE NO-LOCK DISPLAY
      xlabeltemp.MENYVART FORMAT "x(8)":U
      xlabeltemp.MENY FORMAT "x(25)":U
      xlabeltemp.ORDNING FORMAT ">>9":U
      xlabeltemp.KOPPLING COLUMN-LABEL "KOPP." FORMAT ">>9":U
      xlabeltemp.STORKOPPLING COLUMN-LABEL "ST.KOPP" FORMAT ">>9":U
            WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 58.88 BY 15
         TITLE "Företagsupplägg" EXPANDABLE.

DEFINE BROWSE BRW_LABEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LABEL WINDOW-1 _STRUCTURED
  QUERY BRW_LABEL NO-LOCK DISPLAY
      XLABEL.MENYVART FORMAT "x(8)":U
      XLABEL.MENY FORMAT "x(25)":U
      XLABEL.ORDNING FORMAT ">>9":U
      XLABEL.KOPPLING COLUMN-LABEL "KOPP." FORMAT ">>9":U
      XLABEL.STORKOPPLING COLUMN-LABEL "ST.KOPP." FORMAT ">>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 59 BY 15
         BGCOLOR 8 
         TITLE BGCOLOR 8 "SEKRETESS KNAPPAR".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_FOR AT ROW 3.25 COL 4.13
     EDI_FOR AT ROW 3.25 COL 27.25 NO-LABEL
     SEL_FORE AT ROW 3.25 COL 77.88 NO-LABEL
     BRW_LABEL AT ROW 11 COL 1.5
     BRW_FORE AT ROW 11 COL 65.63
     BTN_OVER AT ROW 16 COL 61.88
     BTN_BACK AT ROW 19.75 COL 61.88
     BTN_NY AT ROW 27.25 COL 7.13
     BTN_UPP AT ROW 27.25 COL 18.5
     BTN_BORT AT ROW 27.25 COL 29.75
     BTN_FOR AT ROW 27.25 COL 78
     BTN_NYFORE AT ROW 27.25 COL 89.5
     BTN_AVSL AT ROW 27.25 COL 112
     "HA VILKA KNAPPAR" VIEW-AS TEXT
          SIZE 35.75 BY 1.04 AT ROW 2.25 COL 4.38
     "VÄLJ FÖR VILKA FÖRETAG SOM SKALL" VIEW-AS TEXT
          SIZE 32.88 BY 1.04 AT ROW 1.33 COL 4.38
     "Valda företag" VIEW-AS TEXT
          SIZE 14.75 BY 1.38 AT ROW 1.58 COL 76.75
     RECT-20 AT ROW 26.71 COL 1.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: xlabeltemp T "?" NO-UNDO temp-db xlabeltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "SEKRETTES MENY"
         COLUMN             = 19.5
         ROW                = 2.13
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_FOR RECT-20 FRAME-A */
/* BROWSE-TAB BRW_LABEL SEL_FORE FRAME-A */
/* BROWSE-TAB BRW_FORE BRW_LABEL FRAME-A */
ASSIGN 
       BRW_LABEL:MAX-DATA-GUESS IN FRAME FRAME-A         = 500.

ASSIGN 
       BTN_BORT:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_FOR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_NY:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_UPP:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       EDI_FOR:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FOR
/* Query rebuild information for BROWSE BRW_FOR
     _TblList          = "rt9.XFOREORDNING"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = rt9.XFOREORDNING.ORDNING
     _FldNameList[2]   = rt9.XFOREORDNING.FORETAG
     _Query            is OPENED
*/  /* BROWSE BRW_FOR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FORE
/* Query rebuild information for BROWSE BRW_FORE
     _TblList          = "Temp-Tables.xlabeltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.xlabeltemp.MENYVART
     _FldNameList[2]   = Temp-Tables.xlabeltemp.MENY
     _FldNameList[3]   > Temp-Tables.xlabeltemp.ORDNING
"xlabeltemp.ORDNING" ? ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.xlabeltemp.KOPPLING
"xlabeltemp.KOPPLING" "KOPP." ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.xlabeltemp.STORKOPPLING
"xlabeltemp.STORKOPPLING" "ST.KOPP" ">>9" "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FORE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LABEL
/* Query rebuild information for BROWSE BRW_LABEL
     _TblList          = "rt9.XLABEL"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "rt9.XLABEL.MENYVART|yes,rt9.XLABEL.ORDNING|yes"
     _FldNameList[1]   = rt9.XLABEL.MENYVART
     _FldNameList[2]   = rt9.XLABEL.MENY
     _FldNameList[3]   > rt9.XLABEL.ORDNING
"XLABEL.ORDNING" ? ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > rt9.XLABEL.KOPPLING
"XLABEL.KOPPLING" "KOPP." ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > rt9.XLABEL.STORKOPPLING
"XLABEL.STORKOPPLING" "ST.KOPP." ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LABEL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_LABEL
&Scoped-define SELF-NAME BRW_LABEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LABEL WINDOW-1
ON VALUE-CHANGED OF BRW_LABEL IN FRAME FRAME-A /* SEKRETESS KNAPPAR */
DO:
   RUN valdafore_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL WINDOW-1
ON CHOOSE OF BTN_AVSL IN FRAME FRAME-A /* Avsluta */
DO:       
   {muswait.i} 
   FIND FIRST XFOREORDNING WHERE XFOREORDNING.FORETAG = Guru.Konstanter:globforetag NO-LOCK NO-ERROR.
   IF bort_raknare > 0 THEN DO:
      REPEAT: 
         OPEN QUERY xgq FOR EACH XGURU WHERE XGURU.MENYVART = xbort[bort_raknare] AND
         XGURU.ORDNING = xbort2[bort_raknare] 
         USE-INDEX ORDNING NO-LOCK.
         DO TRANSACTION:      
            GET FIRST xgq EXCLUSIVE-LOCK. 
            IF AVAILABLE XGURU THEN DO:                     
               OPEN QUERY xsq FOR EACH XSEK WHERE XSEK.MENYVART = XGURU.MENYVART 
               USE-INDEX XSEK NO-LOCK.                
               GET FIRST xsq EXCLUSIVE-LOCK.
               IF AVAILABLE XSEK THEN ASSIGN XSEK.SEK[XGURU.ORDNING] = no.
               REPEAT:                                 
                  GET NEXT xsq EXCLUSIVE-LOCK.
                  IF AVAILABLE XSEK THEN ASSIGN XSEK.SEK[XGURU.ORDNING] = no.
                  ELSE LEAVE.
               END.                      
               DELETE XGURU.               
            END. 
         END.
         REPEAT TRANSACTION:                                           
            GET NEXT xgq EXCLUSIVE-LOCK.   
            IF NOT AVAILABLE XGURU THEN LEAVE.                                                           
            OPEN QUERY xsq FOR EACH XSEK WHERE XSEK.MENYVART = XGURU.MENYVART
            USE-INDEX XSEK NO-LOCK.
            GET FIRST xsq EXCLUSIVE-LOCK.
            IF AVAILABLE XSEK THEN ASSIGN XSEK.SEK[XGURU.ORDNING] = no.
            REPEAT:                                 
               GET NEXT xsq EXCLUSIVE-LOCK.
               IF AVAILABLE XSEK THEN ASSIGN XSEK.SEK[XGURU.ORDNING] = no.
               ELSE LEAVE.
            END.             
            DELETE XGURU.          
         END.               
         DO TRANSACTION:
            OPEN QUERY xsq FOR EACH XSEK WHERE XSEK.MENYVART = xbort[bort_raknare] 
            USE-INDEX XSEK NO-LOCK.                               
            GET FIRST xsq EXCLUSIVE-LOCK.
            IF AVAILABLE XSEK THEN DO:
               FIND FIRST XGURU WHERE XGURU.MENYVART = XSEK.MENYVART 
               USE-INDEX ORDNING NO-LOCK NO-ERROR.
               IF NOT AVAILABLE XGURU THEN DO:
                  DELETE XSEK.
                  REPEAT:
                     GET NEXT xsq EXCLUSIVE-LOCK.  
                     IF AVAILABLE XSEK THEN DELETE XSEK.
                     ELSE LEAVE.
                  END.   
               END.
            END.
         END.   
         bort_raknare = bort_raknare - 1.
         IF bort_raknare = 0 THEN LEAVE.
      END.
   END.
   WINDOW-1:TITLE = "LÄGE 2".
   FOR EACH xtemp USE-INDEX MENYVART NO-LOCK:
      IF xtemp.NYAND = "BORT" THEN NEXT.
      xhop = xtemp.MENYVART.                           
      WINDOW-1:TITLE = xtemp.MENYVART.
      RUN sek_UI.         
      IF xhop = "" THEN NEXT.
      RUN xsek_UI.  
   END.
   {musarrow.i}                                             
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK WINDOW-1
ON CHOOSE OF BTN_BACK IN FRAME FRAME-A /* BO */
DO:
   status-ok = BRW_FORE:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.   
   IF AVAILABLE xlabeltemp THEN DO:
      RUN selnextprevrow_UI IN brwproc.
      DELETE xlabeltemp.
      RUN setcolindex_UI IN brwproc (INPUT " xlabeltemp.MENYVART BY xlabeltemp.ORDNING").
      RUN openbdynspec_UI IN brwproc.
      RUN lastselectdyn_UI IN brwproc.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta Bort */
DO:   
   RUN bort_UI.                 
   RUN valdafore_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FOR WINDOW-1
ON CHOOSE OF BTN_FOR IN FRAME FRAME-A /* Företag */
DO:
   {muswait.i} 
   antal_valda = BRW_FOR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:
      MESSAGE "OBS! DU VALDE INGET FÖRETAG" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.      
   END.
   ELSE DO:      /* 
      antal_valda2 = {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
      antal_raknare2 = 1.
      DO WHILE antal_raknare2 LE antal_valda2 :
         status-ok = {&BROWSE-NAME}:FETCH-SELECTED-ROW(antal_raknare2).           
         antal_raknare = 1.
         DO WHILE antal_raknare LE antal_valda :            
            status-ok = BRW_FOR:FETCH-SELECTED-ROW(antal_raknare).
            xlabelrec = RECID(XLABEL).
            DO TRANSACTION:
               FIND XLABEL WHERE RECID(XLABEL) = xlabelrec EXCLUSIVE-LOCK NO-ERROR.         
               ASSIGN        
               XLABEL.FORETAG[XFOREORDNING.ORDNING] = XFOREORDNING.FORETAG.
               antal_raknare = antal_raknare + 1.
            END.
         END.   
         FIND FIRST xtemp WHERE xtemp.MENYVART = XLABEL.MENYVART AND
         xtemp.MENY = XLABEL.MENY AND
         xtemp.ORDNING = XLABEL.ORDNING NO-ERROR.
         IF NOT AVAILABLE xtemp THEN CREATE xtemp.
         ASSIGN
         /*xtemp.NYAND = "AND"*/
         xtemp.MENYVART = XLABEL.MENYVART
         xtemp.MENY = XLABEL.MENY
         xtemp.ORDNING = XLABEL.ORDNING.
         antal_raknare2 = antal_raknare2 + 1.
      END. 
      */
      FOR EACH xlabeltemp:
         FIND XLABEL WHERE XLABEL.MENYVART = xlabeltemp.MENYVART AND
         XLABEL.ORDNING = xlabeltemp.ORDNING EXCLUSIVE-LOCK NO-ERROR.         
         IF AVAILABLE XLABEL THEN DO:
            ASSIGN        
            XLABEL.FORETAG[XFOREORDNING.ORDNING] = XFOREORDNING.FORETAG.
            FIND FIRST xtemp WHERE xtemp.MENYVART = XLABEL.MENYVART AND
            xtemp.MENY = XLABEL.MENY AND
            xtemp.ORDNING = XLABEL.ORDNING NO-ERROR.
            IF NOT AVAILABLE xtemp THEN CREATE xtemp.
            ASSIGN
            /*xtemp.NYAND = "AND"*/
            xtemp.MENYVART = XLABEL.MENYVART
            xtemp.MENY = XLABEL.MENY
            xtemp.ORDNING = XLABEL.ORDNING.
         END.
         ELSE DO:
            MESSAGE xlabeltemp.MENYVART xlabeltemp.ORDNING
            VIEW-AS ALERT-BOX.  
         END.    
      END.
   END.  
   EMPTY TEMP-TABLE xlabeltemp NO-ERROR. 
   RUN openbdynspec_UI IN brwproc.
   RUN valdafore_UI.
      
   musz = FALSE.
   /*
   OPEN QUERY BRW_LABEL FOR EACH XLABEL USE-INDEX MENYVART NO-LOCK INDEXED-REPOSITION.
   
   REPOSITION BRW_LABEL TO RECID xlabelrec.
   */
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
   RUN ny_UI.    
   RUN XLABAND.W.
   {muswait.i} 
   IF musz = FALSE THEN DO:
      RUN valda_UI.
      FIND XLABEL WHERE RECID(XLABEL) = xlabelrec NO-LOCK NO-ERROR.
      FIND FIRST xtemp WHERE xtemp.MENYVART = XLABEL.MENYVART AND      
      xtemp.ORDNING = XLABEL.ORDNING NO-ERROR.
      IF NOT AVAILABLE xtemp THEN CREATE xtemp.
      ASSIGN                       
      xtemp.NYAND = "NY"
      xtemp.MENYVART = XLABEL.MENYVART
      xtemp.MENY = XLABEL.MENY
      xtemp.ORDNING = XLABEL.ORDNING.
      OPEN QUERY BRW_LABEL FOR EACH XLABEL USE-INDEX MENYVART NO-LOCK INDEXED-REPOSITION.
      REPOSITION BRW_LABEL TO RECID xlabelrec.
   END.
   ELSE DO TRANSACTION:
      FIND XLABEL WHERE RECID(XLABEL) = xlabelrec EXCLUSIVE-LOCK NO-ERROR.
      DELETE XLABEL.
   END.   
   RUN valdafore_UI.
   musz = FALSE. 
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYFORE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYFORE WINDOW-1
ON CHOOSE OF BTN_NYFORE IN FRAME FRAME-A /* NY FÖRETAGSUPPLÄGG */
DO:
   MESSAGE "VILL DU LÄGGA UPP NYTT ALLT GAMMALT KOMMER ATT RADERAS" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         musz = FALSE.              
      END.
      WHEN FALSE THEN DO:
         musz = TRUE. 
      END.      
   END CASE.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   MESSAGE "Har du skapat en ny post XFOREORDNING ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val13 AS LOGICAL.
   CASE val13:
      WHEN TRUE THEN DO:
         musz = FALSE.              
      END.
      WHEN FALSE THEN DO:
         musz = TRUE. 
      END.      
   END CASE.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   status-ok = BRW_FOR:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
   MESSAGE "VILL DU LÄGGA UPP NYTT FÖR" XFOREORDNING.FORETAG " ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.
   CASE val2:
      WHEN TRUE THEN DO:
         musz = FALSE.              
      END.
      WHEN FALSE THEN DO:
         musz = TRUE. 
      END.      
   END CASE.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   {muswait.i}
   OPEN QUERY xgq             
   FOR EACH XGURU USE-INDEX ORDNING NO-LOCK.
   DO TRANSACTION:    
      GET FIRST xgq EXCLUSIVE-LOCK.
      IF AVAILABLE XGURU THEN DELETE XGURU.
   END.
   REPEAT:
      DO TRANSACTION:    
         GET NEXT xgq EXCLUSIVE-LOCK.
         IF AVAILABLE XGURU THEN DELETE XGURU.
         ELSE LEAVE.
      END.
   END.
   musz = musz.
   OPEN QUERY xsq             
   FOR EACH XSEK NO-LOCK.
   DO TRANSACTION:    
      GET FIRST xsq EXCLUSIVE-LOCK.
      IF AVAILABLE XSEK THEN DELETE XSEK.
   END.
   REPEAT:
      DO TRANSACTION:    
         GET NEXT xsq EXCLUSIVE-LOCK.
         IF AVAILABLE XSEK THEN DELETE XSEK.
         ELSE LEAVE.
      END.
   END.   
   musz = musz.
   OPEN QUERY xlabelq FOR EACH XLABEL WHERE XLABEL.FORETAG[XFOREORDNING.ORDNING] = XFOREORDNING.FORETAG NO-LOCK.
   GET FIRST xlabelq NO-LOCK.
   DO WHILE AVAILABLE(XLABEL):   
      IF XLABEL.MENYVART = "" THEN musz = musz.
      ELSE DO TRANSACTION:
         CREATE XGURU.
         ASSIGN 
         XGURU.AV-LEVEL = 0 
         XGURU.KOPPLING = XLABEL.KOPPLING 
         XGURU.STORKOPPLING = XLABEL.STORKOPPLING
         XGURU.MENY = XLABEL.MENY 
         XGURU.MENYVART = XLABEL.MENYVART
         XGURU.MENYOK = TRUE 
         XGURU.ORDNING = XLABEL.ORDNING.   
      END.
      GET NEXT xlabelq NO-LOCK.
   END.
   musz = musz.
   OPEN QUERY xgq             
   FOR EACH XGURU USE-INDEX ORDNING NO-LOCK.
   GET FIRST xgq NO-LOCK.         
   DO WHILE AVAILABLE(XGURU): 
      DO TRANSACTION:
         FIND FIRST XSEK WHERE XSEK.MENYVART = XGURU.MENYVART AND
         XSEK.AV-LEVEL = XGURU.AV-LEVEL                     
         USE-INDEX XSEK EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE XSEK THEN DO:
            CREATE XSEK.
            ASSIGN XSEK.MENYVART = XGURU.MENYVART              
            XSEK.AV-LEVEL = XGURU.AV-LEVEL
            XSEK.SEK[XGURU.ORDNING] = XGURU.MENYOK.      
         END.
         ELSE DO:
            XSEK.SEK[XGURU.ORDNING] = XGURU.MENYOK.
         END.
      END.             
      GET NEXT xgq NO-LOCK.
   END.  
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER WINDOW-1
ON CHOOSE OF BTN_OVER IN FRAME FRAME-A /* Ny */
DO:
   antal_valda = BRW_LABEL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda > 0 THEN DO:
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda :
         status-ok = BRW_LABEL:FETCH-SELECTED-ROW(antal_raknare).  
         FIND FIRST xlabeltemp WHERE xlabeltemp.MENYVART = XLABEL.MENYVART AND xlabeltemp.ORDNING = XLABEL.ORDNING NO-LOCK NO-ERROR.
         IF AVAILABLE xlabeltemp THEN antal_raknare = antal_raknare.
         ELSE DO:
            CREATE xlabeltemp.
            BUFFER-COPY XLABEL TO xlabeltemp.
         END.


         antal_raknare = antal_raknare + 1.
      END.
   END.
   IF AVAILABLE xlabeltemp THEN DO:
      RUN setlastrowid_UI IN brwproc (INPUT ROWID(xlabeltemp)).
      RUN setcolindex_UI IN brwproc (INPUT " xlabeltemp.MENYVART BY xlabeltemp.ORDNING").
      RUN openbdynspec_UI IN brwproc.
      RUN lastselectdyn_UI IN brwproc.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A /* Ändra */
DO:
   RUN andra_UI.
   RUN XLABAND.W. 
   {muswait.i} 
   IF musz = FALSE THEN DO:
      RUN valda_UI.
      FIND FIRST xtemp WHERE xtemp.MENYVART = XLABEL.MENYVART AND
      xtemp.MENY = XLABEL.MENY AND
      xtemp.ORDNING = XLABEL.ORDNING NO-ERROR.
      IF NOT AVAILABLE xtemp THEN CREATE xtemp.
      ASSIGN
      /*xtemp.NYAND = "AND"*/
      xtemp.MENYVART = XLABEL.MENYVART
      xtemp.MENY = XLABEL.MENY
      xtemp.ORDNING = XLABEL.ORDNING.
      OPEN QUERY BRW_LABEL FOR EACH XLABEL USE-INDEX MENYVART NO-LOCK INDEXED-REPOSITION.
      REPOSITION BRW_LABEL TO RECID xlabelrec.
   END.   
   RUN valdafore_UI.
   musz = FALSE.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FOR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:  
   RUN DYNBRW.P PERSISTENT SET brwproc (INPUT BRW_FORE:HANDLE IN FRAME {&FRAME-NAME}).
   EDI_FOR = "NYTT FÖRETAG I EN NY DATABAS:" + CHR(10) +
   "1. SKAPA EN NY XFOREORDNING. BÖR FINNAS! se TILL HÖGER" + CHR(10) +
   "2. MARKERA FÖRETAGET OCH DE POSTER SOM FÖRETAGET SKA HA NEDAN." + CHR(10) +
   "3. TRYCK PÅ KNAPPEN FÖRETAG" + CHR(10) +
   "4. TRYCK PÅ KNAPPEN NY FÖRETAGSUPPLÄGG." + CHR(10) +
   "5. GÅ IN I SEKRETESS MODULEN OCH LÄGGUPP EN NY NIVÅ.".                        
   bort_raknare = 0.
   FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.  
   FIND FIRST XFOREORDNING WHERE XFOREORDNING.FORETAG = FORETAG.FORETAG NO-LOCK NO-ERROR.
   IF NOT AVAILABLE XFOREORDNING THEN DO:
      FIND LAST XFOREORDNING WHERE USE-INDEX ORDNING NO-LOCK NO-ERROR.
      IF AVAILABLE XFOREORDNING THEN bort_raknare = XFOREORDNING.ORDNING.
      DO TRANSACTION:
         CREATE XFOREORDNING.
         XFOREORDNING.ORDNING = bort_raknare + 1.
         XFOREORDNING.FORETAG = FORETAG.FORETAG.
      END.   
   END. 
   FIND FIRST XFOREORDNING WHERE XFOREORDNING.FORETAG = FORETAG.FORETAG NO-LOCK NO-ERROR.
   bort_raknare = 0.
   OPEN QUERY BRW_LABEL FOR EACH XLABEL USE-INDEX MENYVART NO-LOCK INDEXED-REPOSITION.
   RUN setcolindex_UI IN brwproc (INPUT " xlabeltemp.MENYVART BY xlabeltemp.ORDNING").
   RUN enable_UI.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI WINDOW-1 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = BRW_LABEL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.   
   xlabelrec = RECID(XLABEL).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE VARIABLE va AS LOGICAL.
   status-ok = BRW_LABEL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.   
   xlabelrec = RECID(XLABEL).  
   antal_valda = BRW_FOR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO TRANSACTION:
      MESSAGE "VILL DU TA BORT ?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE va.
      CASE va:
         WHEN TRUE THEN DO:
            status-ok = BRW_LABEL:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
            IF status-ok = TRUE THEN xlabelrec2 = RECID( XLABEL).
            ELSE DO:          
               status-ok = BRW_LABEL:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
               IF status-ok = TRUE THEN xlabelrec2 = RECID( XLABEL).          
               ELSE xlabelrec2 = xlabelrec.
            END.    
            FIND XLABEL WHERE RECID(XLABEL) = xlabelrec EXCLUSIVE-LOCK NO-ERROR.              
            bort_raknare = bort_raknare + 1.           
            xbort[bort_raknare] = XLABEL.MENYVART.
            xbort2[bort_raknare] = XLABEL.ORDNING.
            FIND FIRST xtemp WHERE xtemp.MENYVART = XLABEL.MENYVART AND            
            xtemp.ORDNING = XLABEL.ORDNING NO-ERROR.
            IF NOT AVAILABLE xtemp THEN CREATE xtemp.
            ASSIGN                       
            xtemp.NYAND = "BORT"
            xtemp.MENYVART = XLABEL.MENYVART
            xtemp.MENY = XLABEL.MENY
            xtemp.ORDNING = XLABEL.ORDNING.      
            DELETE XLABEL. 
            OPEN QUERY BRW_LABEL FOR EACH XLABEL USE-INDEX MENYVART NO-LOCK INDEXED-REPOSITION.
            REPOSITION BRW_LABEL TO RECID xlabelrec2.
         END.
      END CASE.
   END.
   ELSE DO TRANSACTION:   
      FIND XLABEL WHERE RECID(XLABEL) = xlabelrec EXCLUSIVE-LOCK NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda :
         status-ok = BRW_FOR:FETCH-SELECTED-ROW(antal_raknare).         
         ASSIGN        
         XLABEL.FORETAG[XFOREORDNING.ORDNING] = "".
         antal_raknare = antal_raknare + 1.
      END. 
      FIND FIRST xtemp WHERE xtemp.MENYVART = XLABEL.MENYVART AND      
      xtemp.ORDNING = XLABEL.ORDNING NO-ERROR.
      IF NOT AVAILABLE xtemp THEN CREATE xtemp.
      ASSIGN                       
      xtemp.NYAND = "AND"
      xtemp.MENYVART = XLABEL.MENYVART
      xtemp.MENY = XLABEL.MENY
      xtemp.ORDNING = XLABEL.ORDNING.                                     
   END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY EDI_FOR SEL_FORE 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-20 BRW_FOR EDI_FOR SEL_FORE BRW_LABEL BRW_FORE BTN_OVER BTN_BACK 
         BTN_NY BTN_UPP BTN_BORT BTN_FOR BTN_NYFORE BTN_AVSL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI WINDOW-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE XLABEL.
   xlabelrec = RECID(XLABEL). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sek_UI WINDOW-1 
PROCEDURE sek_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
            
   /*SER TILL ATT MENYN OCH ALLA NIVÅER FÅR RÄTT STORLEK*/
   FIND FIRST XGURU WHERE XGURU.MENYVART = xhop 
   USE-INDEX ORDNING NO-LOCK NO-ERROR.
   IF NOT AVAILABLE XGURU THEN DO:
      /*MENYN FINNS INTE I HUVUDTAGET*/
      OPEN QUERY xlq
      FOR EACH XLABEL WHERE XLABEL.MENYVART = xhop 
      USE-INDEX MENYVART NO-LOCK.
      GET FIRST xlq NO-LOCK.
      DO WHILE AVAILABLE(XLABEL):   
         DO TRANSACTION:
            xlev = 0.
            CREATE XGURU.
            ASSIGN 
            XGURU.AV-LEVEL = 0 
            XGURU.KOPPLING = XLABEL.KOPPLING 
            XGURU.STORKOPPLING = XLABEL.STORKOPPLING
            XGURU.MENY = XLABEL.MENY 
            XGURU.MENYVART = xhop
            XGURU.MENYOK = TRUE 
            XGURU.ORDNING = XLABEL.ORDNING.
            IF Guru.Konstanter:globforetag = "ELPA" THEN XGURU.MENYOK = TRUE.
            ELSE IF XLABEL.FORETAG[XFOREORDNING.ORDNING] = "" THEN XGURU.MENYOK = FALSE.                                   
         END.
         REPEAT:
            /*SKAPAR NYA RADER FÖR ALLA NIVÅER*/
            FIND FIRST XGURU WHERE XGURU.MENYVART = "GURU" AND
            XGURU.AV-LEVEL > xlev
            USE-INDEX ORDNING NO-LOCK NO-ERROR.
            IF NOT AVAILABLE XGURU THEN LEAVE.
            ELSE DO TRANSACTION:                                  
               xlev = XGURU.AV-LEVEL.
               CREATE XGURU.
               ASSIGN 
               XGURU.AV-LEVEL = xlev 
               XGURU.KOPPLING = XLABEL.KOPPLING 
               XGURU.STORKOPPLING = XLABEL.STORKOPPLING
               XGURU.MENY = XLABEL.MENY 
               XGURU.MENYVART = xhop
               XGURU.MENYOK = FALSE 
               XGURU.ORDNING = XLABEL.ORDNING.  
               IF Guru.Konstanter:globforetag = "ELPA" THEN XGURU.MENYOK = TRUE.
               ELSE IF XLABEL.FORETAG[XFOREORDNING.ORDNING] = "" THEN XGURU.MENYOK = FALSE.                                        
            END.
         END.
         GET NEXT xlq NO-LOCK.
      END.       
      CLOSE QUERY xlq. 
   END.          
   ELSE DO:
      FIND FIRST XLABEL WHERE XLABEL.MENYVART = xhop AND 
      XLABEL.ORDNING = xtemp.ORDNING
      USE-INDEX MENYVART NO-LOCK NO-ERROR.
      FIND FIRST XGURU WHERE XGURU.MENYVART = xhop AND 
      XGURU.ORDNING = XLABEL.ORDNING
      USE-INDEX ORDNING NO-LOCK NO-ERROR.        
      IF NOT AVAILABLE XGURU THEN DO:
         /*MENY RADEN FINNS INTE*/
         /*SKAPAR NY RAD FÖR NOLL NIVÅN*/
         DO TRANSACTION:
            CREATE XGURU.
            ASSIGN 
            XGURU.KOPPLING = XLABEL.KOPPLING 
            XGURU.STORKOPPLING = XLABEL.STORKOPPLING
            XGURU.AV-LEVEL = 0 
            XGURU.MENY = XLABEL.MENY 
            XGURU.MENYVART = xhop
            XGURU.MENYOK = TRUE
            XGURU.ORDNING = XLABEL.ORDNING.
            IF Guru.Konstanter:globforetag = "ELPA" THEN XGURU.MENYOK = TRUE.
            ELSE IF XLABEL.FORETAG[XFOREORDNING.ORDNING] = "" THEN XGURU.MENYOK = FALSE.                                                    
            xlev = 0.
         END.
         REPEAT:
            /*SKAPAR NYA RADER FÖR ALLA NIVÅER*/            
            FIND FIRST XGURU WHERE XGURU.MENYVART = xhop AND             
            XGURU.AV-LEVEL > xlev       
            USE-INDEX ORDNING NO-LOCK NO-ERROR.
            IF NOT AVAILABLE XGURU THEN LEAVE.
            ELSE DO TRANSACTION:                                  
               xlev = XGURU.AV-LEVEL.
               CREATE XGURU.
               ASSIGN                
               XGURU.KOPPLING = XLABEL.KOPPLING 
               XGURU.STORKOPPLING = XLABEL.STORKOPPLING
               XGURU.AV-LEVEL = xlev 
               XGURU.MENY = xtemp.MENY 
               XGURU.MENYVART = xhop
               XGURU.MENYOK = FALSE 
               XGURU.ORDNING = XLABEL.ORDNING. 
               IF Guru.Konstanter:globforetag = "ELPA" THEN XGURU.MENYOK = TRUE.
               ELSE IF XLABEL.FORETAG[XFOREORDNING.ORDNING] = "" THEN XGURU.MENYOK = FALSE.                                         
            END.
         END.
      END.
      ELSE DO:
         DO TRANSACTION:            
            FIND FIRST XGURU WHERE XGURU.MENYVART = xhop AND 
            XGURU.ORDNING = XLABEL.ORDNING
            USE-INDEX ORDNING EXCLUSIVE-LOCK NO-ERROR.   
            ASSIGN 
            XGURU.KOPPLING = XLABEL.KOPPLING 
            XGURU.STORKOPPLING = XLABEL.STORKOPPLING
            XGURU.AV-LEVEL = 0 
            XGURU.MENY = XLABEL.MENY 
            XGURU.MENYVART = xhop            
            XGURU.ORDNING = XLABEL.ORDNING.
            IF Guru.Konstanter:globforetag = "ELPA" THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.
            ELSE IF XLABEL.FORETAG[XFOREORDNING.ORDNING] = "" THEN XGURU.MENYOK = FALSE.                                                    
            xlev = 0.
         END.
         REPEAT:                             
            DO TRANSACTION:  
               FIND NEXT XGURU WHERE XGURU.MENYVART = xhop AND 
               XGURU.ORDNING = XLABEL.ORDNING AND
               XGURU.AV-LEVEL > xlev       
               USE-INDEX ORDNING EXCLUSIVE-LOCK NO-ERROR.            
               IF NOT AVAILABLE XGURU THEN LEAVE.
               ELSE DO:                                                
                  xlev = XGURU.AV-LEVEL.               
                  ASSIGN                
                  XGURU.KOPPLING = XLABEL.KOPPLING 
                  XGURU.STORKOPPLING = XLABEL.STORKOPPLING
                  XGURU.AV-LEVEL = xlev 
                  XGURU.MENY = XLABEL.MENY 
                  XGURU.MENYVART = xhop               
                  XGURU.ORDNING = XLABEL.ORDNING.                
                  IF Guru.Konstanter:globforetag = "ELPA" THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.
                  ELSE IF XLABEL.FORETAG[XFOREORDNING.ORDNING] = "" THEN XGURU.MENYOK = FALSE.                                                      
               END.
            END.
         END.
      END.        
   END.     /*HÄR MENYSLUT*/
   CLOSE QUERY xlq.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdafore_UI WINDOW-1 
PROCEDURE valdafore_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF antal_fore > 0 THEN DO:
      REPEAT:
         status-ok = SEL_FORE:DELETE(antal_fore) IN FRAME {&FRAME-NAME}.         
         antal_fore = antal_fore - 1.
         IF antal_fore = 0 THEN LEAVE.
      END.
   END.      
   antal_fore = 1.
   REPEAT:
      IF XLABEL.FORETAG[antal_fore] NE "" THEN DO:
         status-ok = SEL_FORE:ADD-LAST(XLABEL.FORETAG[antal_fore]) IN FRAME {&FRAME-NAME}.    
      END.
      antal_fore = antal_fore + 1. 
      IF antal_fore > 50 THEN LEAVE.
   END.       
   DISPLAY SEL_FORE WITH FRAME {&FRAME-NAME}.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valda_UI WINDOW-1 
PROCEDURE valda_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   antal_valda = BRW_FOR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:
      MESSAGE "OBS! DU VALDE INGET FÖRETAG" VIEW-AS ALERT-BOX.      
   END.
   ELSE DO TRANSACTION:   
      FIND XLABEL WHERE RECID(XLABEL) = xlabelrec EXCLUSIVE-LOCK NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda :
         status-ok = BRW_FOR:FETCH-SELECTED-ROW(antal_raknare).         
         ASSIGN        
         XLABEL.FORETAG[XFOREORDNING.ORDNING] = XFOREORDNING.FORETAG.
         antal_raknare = antal_raknare + 1.
      END.
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xsek_UI WINDOW-1 
PROCEDURE xsek_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   OPEN QUERY xgq             
   FOR EACH XGURU WHERE XGURU.MENYVART = xhop
   USE-INDEX ORDNING NO-LOCK.
   GET FIRST xgq NO-LOCK.         
   DO WHILE AVAILABLE(XGURU):
      DO TRANSACTION:
         FIND FIRST XSEK WHERE XSEK.MENYVART = XGURU.MENYVART AND
         XSEK.AV-LEVEL = XGURU.AV-LEVEL                     
         USE-INDEX XSEK EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE XSEK THEN DO:
            CREATE XSEK.
            ASSIGN XSEK.MENYVART = XGURU.MENYVART              
            XSEK.AV-LEVEL = XGURU.AV-LEVEL
            XSEK.SEK[XGURU.ORDNING] = XGURU.MENYOK.      
         END. /*HUR*/  
         IF XSEK.AV-LEVEL = 0 THEN DO:  
            IF Guru.Konstanter:globforetag = "ELPA" THEN
            XSEK.SEK[XGURU.ORDNING] = TRUE.                     
            ELSE DO:
               FIND FIRST XLABEL WHERE XLABEL.MENYVART = XGURU.MENYVART AND
               XLABEL.ORDNING = XGURU.ORDNING USE-INDEX MENYVART NO-LOCK NO-ERROR.
               IF XLABEL.FORETAG[XFOREORDNING.ORDNING] = "" 
               THEN XSEK.SEK[XGURU.ORDNING] = FALSE.  
               ELSE XSEK.SEK[XGURU.ORDNING] = TRUE. 
            END.   
         END.
      END.
      GET NEXT xgq NO-LOCK.
   END.          
   CLOSE QUERY xgq.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

