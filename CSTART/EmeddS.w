&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 03/25/96 -  4:43 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER utvar AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW

DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE logga      AS CHARACTER NO-UNDO.
DEFINE VARIABLE Vitems     AS INTEGER NO-UNDO.
DEFINE VARIABLE vline     AS INTEGER NO-UNDO.
DEFINE VARIABLE h       AS INTEGER NO-UNDO.
DEFINE VARIABLE w       AS INTEGER NO-UNDO.
DEFINE VARIABLE a          AS DECIMAL NO-UNDO.
DEFINE VARIABLE forsta     AS LOGICAL NO-UNDO.
{pdf_StartInc.i}

{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.

DEFINE SHARED VARIABLE globanvextra AS CHARACTER NO-UNDO.
DEFINE VARIABLE epostvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE brwvar AS INTEGER NO-UNDO.     
DEFINE VARIABLE brwopen AS LOGICAL EXTENT 10 NO-UNDO.
DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.
DEFINE VARIABLE brwwh AS WIDGET-HANDLE.
DEFINE VARIABLE brwsortvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE anvtabrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE txtut AS CHARACTER NO-UNDO.   
DEFINE VARIABLE tempch AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-Files AS CHARACTER NO-UNDO.
DEFINE VARIABLE fnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE fileok AS LOGICAL NO-UNDO.
DEFINE VARIABLE tempnamn AS CHARACTER.
DEFINE VARIABLE entryepost AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.

DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.



DEFINE TEMP-TABLE provag
   FIELD VAGNR AS INTEGER
   FIELD VAG AS CHARACTER
   INDEX VAGNR IS PRIMARY VAGNR.
{TIDUTTTSHARED.I}

DEFINE TEMP-TABLE eptemp
   FIELD EPOST AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD ANVANDARE AS CHARACTER.
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_ANV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES personaltemp eptemp

/* Definitions for BROWSE BRW_ANV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANV personaltemp.FORNAMN ~
personaltemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANV 
&Scoped-define QUERY-STRING-BRW_ANV FOR EACH personaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANV OPEN QUERY BRW_ANV FOR EACH personaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANV personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANV personaltemp


/* Definitions for BROWSE BRW_EGNAEPOST                                 */
&Scoped-define FIELDS-IN-QUERY-BRW_EGNAEPOST eptemp.EPOST 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_EGNAEPOST eptemp.EPOST 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_EGNAEPOST eptemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_EGNAEPOST eptemp
&Scoped-define QUERY-STRING-BRW_EGNAEPOST FOR EACH eptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_EGNAEPOST OPEN QUERY BRW_EGNAEPOST FOR EACH eptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_EGNAEPOST eptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_EGNAEPOST eptemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_ANV}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-49 IMAGE-1 FILL-IN_MOTTAGARE ~
BRW_EGNAEPOST FILL-IN-AMNE FILL-IN-FIL SELECT-FILES FBTN_OUTLOOK BTN_NY ~
BTN_AND BTN_BORT EDITOR_MEDD BRW_ANV FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN ~
BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_MOTTAGARE FILL-IN-AMNE FILL-IN-FIL ~
SELECT-FILES FILL-IN-FILNAMN EDITOR_MEDD FILL-IN_SFORNAMN ~
FILL-IN_SEFTERNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB AUTO-GO 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OUTLOOK 
     LABEL "Från Outlook" 
     SIZE 14 BY 1 TOOLTIP "Epost adresser från Outlook".

DEFINE VARIABLE EDITOR_MEDD AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 69.5 BY 9.17
     FONT 22 NO-UNDO.

DEFINE VARIABLE FILL-IN-AMNE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ämne" 
     VIEW-AS FILL-IN 
     SIZE 57.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FIL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Infoga fil" 
     VIEW-AS FILL-IN 
     SIZE 57.5 BY 1 TOOLTIP "Obs! Hela sökvägen till filen måste vara med! Högra mustangenten ger hjälp." NO-UNDO.

DEFINE VARIABLE FILL-IN-FILNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filnamn" 
     VIEW-AS FILL-IN 
     SIZE 57.5 BY 1 TOOLTIP "Endast dokument namn" NO-UNDO.

DEFINE VARIABLE FILL-IN_MOTTAGARE AS CHARACTER FORMAT "x(256)" 
     LABEL "Mottagare" 
     VIEW-AS FILL-IN 
     SIZE 57.5 BY 1 TOOLTIP "Om fältet lämnas blank tas e-post adress från register".

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(256)" 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 16.88 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(256)" 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 16.88 BY .83.

DEFINE IMAGE IMAGE-1
     FILENAME "BILDER/sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39.63 BY 2.42
     BGCOLOR 8 .

DEFINE VARIABLE SELECT-FILES AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 57.5 BY 2 TOOLTIP "Obs! Högra mustangenten ger hjälp." NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANV FOR 
      personaltemp SCROLLING.

DEFINE QUERY BRW_EGNAEPOST FOR 
      eptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANV DIALOG-1 _STRUCTURED
  QUERY BRW_ANV NO-LOCK DISPLAY
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 15
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 39.63 BY 6.42 TOOLTIP "OBS! Mottagarnamnet måste stå på samma sätt som i adressboken i Outlook.".

DEFINE BROWSE BRW_EGNAEPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_EGNAEPOST DIALOG-1 _STRUCTURED
  QUERY BRW_EGNAEPOST NO-LOCK DISPLAY
      eptemp.EPOST COLUMN-LABEL "E-post" FORMAT "X(256)":U WIDTH 40
  ENABLE
      eptemp.EPOST
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 39.63 BY 7.75
         TITLE "Mina egna e-postadresser".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_MOTTAGARE AT ROW 3 COL 11 COLON-ALIGNED
     BRW_EGNAEPOST AT ROW 3 COL 71.38
     FILL-IN-AMNE AT ROW 4.5 COL 11 COLON-ALIGNED
     FILL-IN-FIL AT ROW 6 COL 11 COLON-ALIGNED
     SELECT-FILES AT ROW 7.5 COL 13 NO-LABEL
     FBTN_OUTLOOK AT ROW 8 COL 111.5
     FILL-IN-FILNAMN AT ROW 9.96 COL 11 COLON-ALIGNED
     BTN_NY AT ROW 11 COL 72.13
     BTN_AND AT ROW 11 COL 84.88
     BTN_BORT AT ROW 11 COL 97.63
     EDITOR_MEDD AT ROW 12.58 COL 1.5 NO-LABEL
     BRW_ANV AT ROW 12.58 COL 71.38
     FILL-IN_SFORNAMN AT ROW 19.58 COL 89 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 20.58 COL 89 COLON-ALIGNED
     BTN_OK AT ROW 22.04 COL 96.5
     BTN_AVB AT ROW 22.04 COL 111.5
     "E-post meddelande" VIEW-AS TEXT
          SIZE 33 BY 1.08 AT ROW 1.5 COL 1.5
          FONT 17
     RECT-49 AT ROW 19.38 COL 71.38
     IMAGE-1 AT ROW 19.67 COL 72.25
     SPACE(45.49) SKIP(2.87)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "E-post från Guru".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: anvandartemp T "?" NO-UNDO TEMP-DB anvandartemp
      TABLE: eptemp T "?" NO-UNDO temp-db eptemp
      TABLE: personaltemp T "?" NO-UNDO TEMP-DB personaltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_EGNAEPOST FILL-IN_MOTTAGARE DIALOG-1 */
/* BROWSE-TAB BRW_ANV EDITOR_MEDD DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-FILNAMN IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANV
/* Query rebuild information for BROWSE BRW_ANV
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_ANV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_EGNAEPOST
/* Query rebuild information for BROWSE BRW_EGNAEPOST
     _TblList          = "Temp-Tables.eptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.eptemp.EPOST
"eptemp.EPOST" "E-post" "X(256)" "character" ? ? ? ? ? ? yes ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_EGNAEPOST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* E-post från Guru */
DO:
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* E-post från Guru */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON GO OF FRAME DIALOG-1 /* E-post från Guru */
DO:
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANV
&Scoped-define SELF-NAME BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ANV DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_ANV IN FRAME DIALOG-1
DO:
   IF AVAILABLE personaltemp THEN DO: 
      FILL-IN_MOTTAGARE = INPUT FILL-IN_MOTTAGARE.
      IF FILL-IN_MOTTAGARE = "" THEN FILL-IN_MOTTAGARE =  personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.
      ELSE FILL-IN_MOTTAGARE = FILL-IN_MOTTAGARE + ";" + personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.
      DISPLAY FILL-IN_MOTTAGARE WITH FRAME {&FRAME-NAME}.     
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ANV DIALOG-1
ON VALUE-CHANGED OF BRW_ANV IN FRAME DIALOG-1
DO:
   /*
   
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_EGNAEPOST
&Scoped-define SELF-NAME BRW_EGNAEPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_EGNAEPOST DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_EGNAEPOST IN FRAME DIALOG-1 /* Mina egna e-postadresser */
DO:
  IF AVAILABLE eptemp THEN DO: 
      FILL-IN_MOTTAGARE = INPUT FILL-IN_MOTTAGARE.
      IF FILL-IN_MOTTAGARE = "" THEN FILL-IN_MOTTAGARE =  eptemp.EPOST.
      ELSE FILL-IN_MOTTAGARE = FILL-IN_MOTTAGARE + ";" + eptemp.EPOST.
      DISPLAY FILL-IN_MOTTAGARE WITH FRAME {&FRAME-NAME}.
      DISPLAY  eptemp.EPOST WITH BROWSE BRW_EGNAEPOST.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_EGNAEPOST DIALOG-1
ON ROW-LEAVE OF BRW_EGNAEPOST IN FRAME DIALOG-1 /* Mina egna e-postadresser */
DO:
   
   status-ok = BRW_EGNAEPOST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE eptemp THEN DO:
      DISPLAY  eptemp.EPOST WITH BROWSE BRW_EGNAEPOST.
      eptemp.EPOST = INPUT BROWSE BRW_EGNAEPOST eptemp.EPOST.
      DISPLAY  eptemp.EPOST WITH BROWSE BRW_EGNAEPOST.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_EGNAEPOST DIALOG-1
ON VALUE-CHANGED OF BRW_EGNAEPOST IN FRAME DIALOG-1 /* Mina egna e-postadresser */
DO:      
   status-ok = BRW_EGNAEPOST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   /*
   IF AVAILABLE eptemp THEN DO: 
      FILL-IN_MOTTAGARE =  eptemp.EPOST.
      DISPLAY FILL-IN_MOTTAGARE WITH FRAME {&FRAME-NAME}.
      DISPLAY  eptemp.EPOST WITH BROWSE BRW_EGNAEPOST.
   END.   
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eptemp.EPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eptemp.EPOST BRW_EGNAEPOST _BROWSE-COLUMN DIALOG-1
ON ENTRY OF eptemp.EPOST IN BROWSE BRW_EGNAEPOST /* E-post */
DO:
   entryepost = TRUE.
   IF AVAILABLE eptemp THEN DISPLAY  eptemp.EPOST WITH BROWSE BRW_EGNAEPOST.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eptemp.EPOST BRW_EGNAEPOST _BROWSE-COLUMN DIALOG-1
ON LEAVE OF eptemp.EPOST IN BROWSE BRW_EGNAEPOST /* E-post */
DO:
  
  IF AVAILABLE eptemp THEN DO:
      musz = FALSE.
      IF eptemp.EPOST = "" THEN musz = TRUE.
      IF eptemp.EPOST NE INPUT BROWSE BRW_EGNAEPOST eptemp.EPOST THEN musz = TRUE.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         eptemp.EPOST = INPUT BROWSE BRW_EGNAEPOST eptemp.EPOST.   
         DISPLAY  eptemp.EPOST WITH BROWSE BRW_EGNAEPOST.
         RUN EPOSTKOLL.P (INPUT eptemp.EPOST,OUTPUT musz).
         IF musz = FALSE OR eptemp.EPOST = "" THEN DO:
            MESSAGE "Vill du ta bort den?" VIEW-AS ALERT-BOX 
            QUESTION BUTTONS YES-NO UPDATE svar.
            IF svar THEN DO:
               APPLY "CHOOSE" TO BTN_BORT IN FRAME {&FRAME-NAME}.                  
               eptemp.EPOST:READ-ONLY IN BROWSE BRW_EGNAEPOST = TRUE.
            END.
            ELSE DO:               
               APPLY "ENTRY" TO eptemp.EPOST IN BROWSE BRW_EGNAEPOST.
               RETURN NO-APPLY.
            END.
         END.               
         musz = FALSE.
      END.
      ELSE eptemp.EPOST:READ-ONLY IN BROWSE BRW_EGNAEPOST = TRUE.
   END.
   entryepost = FALSE.
   SESSION:DATA-ENTRY-RETURN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND DIALOG-1
ON CHOOSE OF BTN_AND IN FRAME DIALOG-1 /* Ändra */
DO:   
   IF AVAILABLE eptemp THEN DO:
      eptemp.EPOST:READ-ONLY IN BROWSE BRW_EGNAEPOST = FALSE.
      APPLY "ENTRY" TO eptemp.EPOST IN BROWSE BRW_egnaepost. 
      DISPLAY BRW_EGNAEPOST WITH FRAME {&FRAME-NAME}.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   MESSAGE "Vill du spara dina valda epostadresser för snabbare åtkomst ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
   IF val = TRUE THEN DO:
      FOR EACH eptemp WHERE eptemp.EPOST = "":
         DELETE eptemp.
      END.
      FOR EACH eptemp:
         eptemp.AONR = eptemp.EPOST.
      END.
      tthandle = TEMP-TABLE eptemp:HANDLE.
      FIND FIRST sparaladdatemp NO-ERROR.
      IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
      ASSIGN
      sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/
      sparaladdatemp.BENAMNING = "EPOST" /*Benämnings sufix, i detta fall ELPAO$STOR*/
      sparaladdatemp.TABVAL = "eptemp" /*Tabellnamn*/
      sparaladdatemp.FALTVALAO = "AONR" /*Character field*/
      sparaladdatemp.FALTVALDEL = "" /*Integer field*/
      sparaladdatemp.FALTVALDATE = "".  /*DATE field*/
      RUN sparabrw_UI IN brwproc[2] 
         (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp).
   END.        
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-1
ON CHOOSE OF BTN_BORT IN FRAME DIALOG-1 /* Ta bort */
DO:
   status-ok = BRW_EGNAEPOST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE eptemp THEN DO:
      MESSAGE "Vill du ta bort " eptemp.EPOST "?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.
      IF svar = FALSE THEN RETURN NO-APPLY.
      DELETE eptemp.
      RUN selnextprevrow_UI IN brwproc[2].
      RUN openbdyn_UI IN brwproc[2] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[2].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-1
ON CHOOSE OF BTN_NY IN FRAME DIALOG-1 /* Ny */
DO:   
   CREATE eptemp.
   /*
   ASSIGN eptemp.EPOST = FILL-IN_MOTTAGARE.
   */
   RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(eptemp)).            
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[2].
   eptemp.EPOST:READ-ONLY IN BROWSE BRW_EGNAEPOST = FALSE.
   APPLY "ENTRY" TO eptemp.EPOST IN BROWSE BRW_egnaepost. 
   DISPLAY BRW_EGNAEPOST WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i}   
   ASSIGN
   FILL-IN_MOTTAGARE = INPUT FILL-IN_MOTTAGARE
   FILL-IN-AMNE = INPUT FILL-IN-AMNE
   FILL-IN-FIL = INPUT FILL-IN-FIL
   FILL-IN-FILNAMN = INPUT FILL-IN-FILNAMN
   EDITOR_MEDD = INPUT EDITOR_MEDD.      
   /*EDITOR_MEDD = "".*/                                               
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FIL IN FRAME {&FRAME-NAME}.
   tempnamn = SELECT-FILES:LIST-ITEMS.
   IF tempnamn = ? THEN tempnamn = "".   
   
   DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.  
   DEFINE VARIABLE currdir AS CHARACTER NO-UNDO.   
   /*
   RUN GetCurrentDirectory_UI IN Guru.Konstanter:hpApi  (OUTPUT currdir).   
   */
   file-info:file-name = ".".
   orgdir = file-info:full-pathname.

   RUN EPOST.P (INPUT "",INPUT FILL-IN_MOTTAGARE,INPUT FILL-IN-AMNE,INPUT EDITOR_MEDD,
                INPUT tempnamn,INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick, OUTPUT efel).   
   RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).   
   IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.
   IF skick = TRUE THEN RETURN NO-APPLY. 

   MESSAGE "Vill du spara dina valda epostadresser för snabbare åtkomst ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
   IF val = TRUE THEN DO:
      FOR EACH eptemp WHERE eptemp.EPOST = "":
         DELETE eptemp.
      END.
      FOR EACH eptemp:
         eptemp.AONR = eptemp.EPOST.
      END.
      tthandle = TEMP-TABLE eptemp:HANDLE.
      FIND FIRST sparaladdatemp NO-ERROR.
      IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
      ASSIGN
      sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/
      sparaladdatemp.BENAMNING = "EPOST" /*Benämnings sufix, i detta fall ELPAO$STOR*/
      sparaladdatemp.TABVAL = "eptemp" /*Tabellnamn*/
      sparaladdatemp.FALTVALAO = "AONR" /*Character field*/
      sparaladdatemp.FALTVALDEL = "" /*Integer field*/
      sparaladdatemp.FALTVALDATE = "".  /*DATE field*/
      RUN sparabrw_UI IN brwproc[2] 
         (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp).
   END.        
   
   {BORTBRWPROC.I}
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR_MEDD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_MEDD DIALOG-1
ON CTRL-A OF EDITOR_MEDD IN FRAME DIALOG-1
DO:
   EDITOR_MEDD:SET-SELECTION(1,-1).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_MEDD DIALOG-1
ON LEAVE OF EDITOR_MEDD IN FRAME DIALOG-1
DO:
   EDITOR_MEDD = INPUT EDITOR_MEDD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OUTLOOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OUTLOOK DIALOG-1
ON CHOOSE OF FBTN_OUTLOOK IN FRAME DIALOG-1 /* Från Outlook */
DO:
   {muswait.i}  
   {AVBGOM.I}
   RUN EPOSTVAL.W (INPUT FILL-IN_MOTTAGARE,OUTPUT epostvar).
   {AVBFRAM.I}   
   {musarrow.i}
   IF epostvar NE "" THEN DO:
      status-ok = BRW_ANV:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR. 
      FILL-IN_MOTTAGARE = epostvar.
      DISPLAY FILL-IN_MOTTAGARE WITH FRAME {&FRAME-NAME}.
      APPLY "ENTRY" TO FILL-IN-AMNE IN FRAME {&FRAME-NAME}.
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AMNE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AMNE DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-AMNE IN FRAME DIALOG-1 /* Ämne */
DO:
   /*
   SYSTEM-DIALOG GET-FILE filnamn
   TITLE          "Välj den fil som skall skickas med brevet."
   FILTERS        "All Files (*.*)"  "*.*"   
   MUST-EXIST         
   USE-FILENAME
   UPDATE OKvald.
   IF OKvald = TRUE THEN DO:
      FOR EACH provag:
         DELETE provag.
      END.    
      FILL-IN-FIL = filnamn.
      REPEAT i=1 TO NUM-ENTRIES(filnamn,"\"):
         CREATE provag.
         ASSIGN
         provag.VAGNR = i
         provag.VAG = STRING(ENTRY(i,filnamn,"\"),"x(78)").
      END.
      FIND LAST provag NO-LOCK NO-ERROR.
      IF AVAILABLE provag THEN DO:  
         FILL-IN-FILNAMN = provag.VAG.         
      END.
   END. 
   */         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FIL DIALOG-1
ON ANY-KEY OF FILL-IN-FIL IN FRAME DIALOG-1 /* Infoga fil */
DO:        
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FIL IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FIL DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-FIL IN FRAME DIALOG-1 /* Infoga fil */
DO:
   SYSTEM-DIALOG GET-FILE filnamn
   TITLE          "Välj den fil som skall skickas med brevet."
   FILTERS        "All Files (*.*)"  "*.*"
   INITIAL-DIR    "."
   MUST-EXIST         
   USE-FILENAME
   UPDATE OKvald.
   IF OKvald = TRUE THEN DO:
      FOR EACH provag:
         DELETE provag.
      END.
      MESSAGE filnamn
      VIEW-AS ALERT-BOX.    
      FILL-IN-FIL = filnamn.
      REPEAT i=1 TO NUM-ENTRIES(filnamn,"\"):
         CREATE provag.
         ASSIGN
         provag.VAGNR = i
         provag.VAG = STRING(ENTRY(i,filnamn,"\"),"x(78)").
      END.
      FIND LAST provag NO-LOCK NO-ERROR.
      IF AVAILABLE provag THEN DO:  
         FILL-IN-FILNAMN = provag.VAG.         
      END.
   END.
   
   DISPLAY FILL-IN-FIL WITH WITH FRAME DIALOG-1.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FIL IN FRAME {&FRAME-NAME}.   
   FILL-IN-FIL = "".       
   DISPLAY FILL-IN-FIL WITH WITH FRAME DIALOG-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FIL DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FIL IN FRAME DIALOG-1 /* Infoga fil */
DO:
   FILL-IN-FIL = INPUT FILL-IN-FIL.
   IF FILL-IN-FIL = "" THEN LEAVE.
   ELSE FILE-INFO:FILE-NAME = FILL-IN-FIL.
   IF FILE-INFO:PATHNAME = ? THEN DO:
      MESSAGE "Kan inte hitta filen:" FILL-IN-FIL VIEW-AS ALERT-BOX.
      LEAVE.
   END.
   ELSE IF FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
      MESSAGE "Infoga fil kan inte vara en katalog!" VIEW-AS ALERT-BOX.
      LEAVE.
   END.
   ELSE DO: 
      SELECT-FILES:ADD-LAST(FILE-INFO:PATHNAME).
      RUN filestring_UI.
      FILL-IN-FIL = "".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FILNAMN
&Scoped-define SELF-NAME FILL-IN_SEFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN DIALOG-1
ON LEAVE OF FILL-IN_SEFTERNAMN IN FRAME DIALOG-1 /* Efternamn */
DO:
  FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-1
ON LEAVE OF FILL-IN_SFORNAMN IN FRAME DIALOG-1 /* Förnamn */
DO:
  FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-FILES
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-FILES DIALOG-1
ON ANY-KEY OF SELECT-FILES IN FRAME DIALOG-1
DO:
   SELECT-FILES = INPUT SELECT-FILES.    
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = "DELETE-CHARACTER" THEN DO:
      SELECT-FILES:DELETE(SELECT-FILES).
      RUN filestring_UI.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-FILES DIALOG-1
ON MOUSE-MENU-CLICK OF SELECT-FILES IN FRAME DIALOG-1
DO:
   FRAME DIALOG-1:HIDDEN = TRUE.
   RUN FILEMULTISEL.P ( INPUT "All Files (*.*)|*.*",
                        INPUT ?,
                        INPUT "Välj den fil som skall skickas med brevet.",
                        OUTPUT lv-Files,
                        OUTPUT fileok).
   FRAME DIALOG-1:HIDDEN = FALSE.
   IF fileok THEN DO:
      REPEAT i = 1 TO NUM-ENTRIES(lv-Files):
         SELECT-FILES:ADD-LAST(ENTRY(i,lv-Files)).
      END.
      RUN filestring_UI.
   END.
   APPLY "ENTRY" TO SELECT-FILES IN FRAME DIALOG-1.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-FILES DIALOG-1
ON VALUE-CHANGED OF SELECT-FILES IN FRAME DIALOG-1
DO:
  SELECT-FILES = INPUT SELECT-FILES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANV
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   IF utvar = TRUE THEN RUN tidut_UI.
   FILL-IN-FIL = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN FILL-IN-FIL = webclienttempdir.   
   OS-CREATE-DIR VALUE(FILL-IN-FIL) NO-ERROR.
   /*
   FILL-IN-FILNAMN = Guru.Konstanter:globanv + ".doc".
   */  
   FILL-IN-FILNAMN = Guru.Konstanter:globanv + ".pdf".
   txtut = FILL-IN-FIL + FILL-IN-FILNAMN.
   SELECT-FILES:ADD-LAST(txtut).
   FILL-IN-FIL = "".
/*    FILL-IN-FIL = txtut. */
/*
   OUTPUT TO VALUE(txtut).
   FOR EACH tidut NO-LOCK:       
      PUT tidut.UT AT 1 SKIP.
   END.    
   OUTPUT CLOSE.
   */
   {BILDPDF.I}
IF Guru.GlobalaVariabler:globsidl = 0 THEN Guru.GlobalaVariabler:globsidl = 53.
IF Guru.GlobalaVariabler:globsids = 0 THEN Guru.GlobalaVariabler:globsids = 73.
IF logga NE ? THEN Guru.GlobalaVariabler:globsidl = Guru.GlobalaVariabler:globsidl - 3.

RUN startpdf_UI.
RUN pdf_close IN h_PDFinc ("Spdf").
IF VALID-HANDLE(h_PDFinc) THEN DELETE PROCEDURE h_PDFinc NO-ERROR. 
h_PDFinc = ?.
   FOR EACH personaltemp :
      FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE anvandartemp THEN DELETE personaltemp.
   END.
   RUN enable_UI.  
   IF globanvextra = "" THEN DO:
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN DO:     
         FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = anvandartemp.PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE personaltemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(personaltemp)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.     
      END.
      ELSE DO:
         FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
         IF AVAILABLE personaltemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(personaltemp)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.     
   
      END.
   END.
   ELSE DO:
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = globanvextra NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN DO:     
         FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = anvandartemp.PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE personaltemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(personaltemp)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.     
      END.
      ELSE DO:
         FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = globanvextra NO-LOCK NO-ERROR.
         IF AVAILABLE personaltemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(personaltemp)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.     
   
      END.

   END.
   {FRMSIZED.I}
   {musarrow.i}
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      ASSIGN
      BRW_ANV:HIDDEN = TRUE
      FILL-IN_SEFTERNAMN:HIDDEN = TRUE 
      FILL-IN_SFORNAMN:HIDDEN = TRUE
      RECT-49:HIDDEN = TRUE
      IMAGE-1:HIDDEN = TRUE.
   END.
   {DIA_M_SLUT.I}
   eptemp.EPOST:READ-ONLY IN BROWSE BRW_EGNAEPOST = TRUE.
   IF AVAILABLE eptemp THEN DO:
      status-ok = BRW_EGNAEPOST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      /*
      DISPLAY  eptemp.EPOST WITH BROWSE BRW_EGNAEPOST.
      status-ok = BRW_EGNAEPOST:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      */
   END.
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_ANV:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_EGNAEPOST:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN_SFORNAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "FORNAMN").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN_SEFTERNAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "EFTERNAMN"). 
   tthandle = TEMP-TABLE eptemp:HANDLE.
   FIND FIRST sparaladdatemp NO-ERROR.
   IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
   ASSIGN
   sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/
   sparaladdatemp.BENAMNING = "EPOST" /*Benämnings sufix, i detta fall ELPAO$STOR*/
   sparaladdatemp.TABVAL = "eptemp" /*Tabellnamn*/
   sparaladdatemp.FALTVALAO = "AONR" /*CHARACTER field*/
   sparaladdatemp.FALTVALDEL = "" /*Integer field*/
   sparaladdatemp.FALTVALDATE = "".   /*DATE field*/   
   RUN laddabrw2_UI IN brwproc[2] 
      (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp).       
    FOR EACH eptemp:
      eptemp.EPOST = eptemp.AONR.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_MOTTAGARE FILL-IN-AMNE FILL-IN-FIL SELECT-FILES 
          FILL-IN-FILNAMN EDITOR_MEDD FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 
      WITH FRAME DIALOG-1.
  ENABLE RECT-49 IMAGE-1 FILL-IN_MOTTAGARE BRW_EGNAEPOST FILL-IN-AMNE 
         FILL-IN-FIL SELECT-FILES FBTN_OUTLOOK BTN_NY BTN_AND BTN_BORT 
         EDITOR_MEDD BRW_ANV FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filestring_UI DIALOG-1 
PROCEDURE filestring_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/            
   DEFINE VARIABLE i AS INTEGER NO-UNDO.   
   REPEAT i = 1 TO NUM-ENTRIES(SELECT-FILES:LIST-ITEMS IN FRAME {&FRAME-NAME}):
      tempch = ENTRY(i,SELECT-FILES:LIST-ITEMS).
      tempch = SUBSTRING(tempch, R-INDEX(tempch, "\") + 1, LENGTH(tempch)).
      IF i = 1 THEN DO: 
         fnamn = tempch.
         SELECT-FILES:SCREEN-VALUE = ENTRY(i,SELECT-FILES:LIST-ITEMS).
      END.
      ELSE fnamn = fnamn + "," + tempch.
   END.
   IF SELECT-FILES:NUM-ITEMS = 0 THEN DO:
      FILL-IN-FILNAMN = "".
   END.
   ELSE FILL-IN-FILNAMN = fnamn.
   DISPLAY FILL-IN-FILNAMN WITH FRAME {&FRAME-NAME}.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new_page DIALOG-1 
PROCEDURE new_page :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/            
   RUN pdf_new_page2 IN h_PDFinc ("Spdf","Landscape").
   RUN pdf_set_LeftMargin IN h_PDFinc ("Spdf",20).
   RUN pdf_set_TopMargin IN h_PDFinc ("Spdf",30).
   /*Sätt font*/
   RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",9.0).
   RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
   vline = 0. 
   IF forsta = TRUE THEN DO:
      
      RUN pdf_skip IN h_PDFinc ("Spdf").
   END.
   /*Init. logga*/
   IF forsta = FALSE THEN DO:
      IF logga NE ? THEN DO:
         RUN pdf_load_image IN h_PDFinc ("Spdf","ProSysLogo",logga).
         h = pdf_ImageDim ("Spdf","ProSysLogo","HEIGHT").
         w = pdf_ImageDim ("Spdf","ProSysLogo","WIDTH").
         IF h > 30 THEN DO:
            a = 30 / h.
            h = a * h.
            w = a * w.            
         END.
         RUN pdf_place_image IN h_PDFinc ("Spdf","ProSysLogo",pdf_LeftMargin("Spdf"), pdf_TopMargin("Spdf") + 10,w,h).
         RUN pdf_skipn IN h_PDFinc ("Spdf",3).
         vline = 3.
      END.
      forsta = TRUE.
   END.
   ELSE DO:
      /*Placera ut logga överst på varje sida*/
      IF logga NE ? THEN DO:
         RUN pdf_place_image IN h_PDFinc ("Spdf","ProSysLogo",pdf_LeftMargin("Spdf"), pdf_TopMargin("Spdf") + 10,w,h).
         RUN pdf_skipn IN h_PDFinc ("Spdf",3).
         vline = 3.
      END.
      RUN pdf_text IN h_PDFinc ("Spdf", tidut.UT).
      RUN pdf_skip IN h_PDFinc ("Spdf").      
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowleave_UI DIALOG-1 
PROCEDURE rowleave_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/     
  IF entryepost = TRUE THEN DO:
     IF AVAILABLE eptemp THEN DO:   
        APPLY "LEAVE" TO eptemp.EPOST IN BROWSE BRW_EGNAEPOST.
     END.
  END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startpdf_UI DIALOG-1 
PROCEDURE startpdf_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/            
   FIND FIRST tidut NO-LOCK NO-ERROR.
   tempnamn = txtut.
   IF AVAILABLE tidut THEN DO: 
      RUN pdf_new IN h_PDFinc ("Spdf",tempnamn).
      RUN pdf_set_LeftMargin IN h_PDFinc ("Spdf",20).
      RUN pdf_set_TopMargin IN h_PDFinc ("Spdf",30).
      /* Dokument Information */ 
      RUN pdf_set_info("Spdf","Author","Elpool i Umeå AB").
      RUN pdf_set_info("Spdf","Subject","Kalkyl").
      RUN pdf_set_info("Spdf","Title","Kalkyl").
      RUN pdf_set_info("Spdf","Keywords","Kalkyl").
      RUN pdf_set_info("Spdf","Creator","PDFinclude").
      RUN pdf_set_info("Spdf","Producer","UTFKALPDF.P").
      /*Skapa ny sida*/
      RUN new_page.
      RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",9.0).
      RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
      FOR EACH tidut NO-LOCK:
         IF SUBSTRING(tidut.UT,1,10) = "          " AND forsta = FALSE THEN DO: 
            RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",9.0).
            RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
            forsta = TRUE.
         END.
         IF tidut.UT = ? THEN NEXT.
         vline = vline + 1.
         IF SUBSTRING(tidut.UT,132,1) = "$" THEN DO:
            RUN new_page.
         END.
         IF vline >= Guru.GlobalaVariabler:globsidl THEN DO:
            RUN new_page.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,4) = "====" THEN DO:
            RUN pdf_line IN h_PDFinc ("Spdf", pdf_LeftMargin("Spdf") , pdf_TextY("Spdf") + 8, pdf_PageWidth("Spdf") - 10 , pdf_TextY("Spdf") + 8, 1).
            RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",9.0).
            RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
         END.
         ELSE DO:
            RUN pdf_text IN h_PDFinc ("Spdf", tidut.UT).
            RUN pdf_skip IN h_PDFinc ("Spdf").
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tidut_UI DIALOG-1 
PROCEDURE tidut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   EDITOR_MEDD = 
   "För snygga utskrifter markera allt och byt font till Courier storlek 8" + CHR(10) + CHR(10).
*/   
   FOR EACH tidut NO-LOCK:
      IF LENGTH(EDITOR_MEDD + SUBSTRING(tidut.UT,1,LENGTH(tidut.UT)) + CHR(10),"CHARACTER") > 30000 THEN DO:
         MESSAGE "Hela din rapport kan ej visas på skärmen," SKIP
         "men hela rapporten kommer att skickas som bifogad fil" 
         VIEW-AS ALERT-BOX.
         EDITOR_MEDD = SUBSTRING(EDITOR_MEDD,1,500).
         RETURN.
      END.
      ELSE DO:
         IF LENGTH(tidut.UT) > 0 THEN DO:         
            EDITOR_MEDD = EDITOR_MEDD + SUBSTRING(tidut.UT,1,LENGTH(tidut.UT)) + CHR(10).
         END.
         ELSE EDITOR_MEDD = EDITOR_MEDD + CHR(10).
      END.   
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI DIALOG-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/            
   {PRINTSTAENDE.I}          
   DISPLAY "MEDELANDE TILL :" AT 6    
   FILL-IN_MOTTAGARE AT 23  NO-LABEL TODAY AT 38
           "ÄMNE           :" AT 6  
   FILL-IN-AMNE AT 23  NO-LABEL 
   EDITOR_MEDD AT 6  VIEW-AS EDITOR SIZE 50 BY 9 NO-LABEL.
   OUTPUT CLOSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

