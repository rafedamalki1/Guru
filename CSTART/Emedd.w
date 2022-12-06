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
{GLOBVAR2DEL1.I}


DEFINE NEW SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
/*Behövs dessa?*/
DEFINE VARIABLE brwvar AS INTEGER NO-UNDO.
DEFINE VARIABLE brwopen AS LOGICAL EXTENT 10 NO-UNDO.
DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.
DEFINE VARIABLE brwwh AS WIDGET-HANDLE.
DEFINE VARIABLE brwsortvar AS INTEGER NO-UNDO.
DEFINE VARIABLE anvtabrec AS RECID NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE i AS INTEGER.                
DEFINE VARIABLE tempch AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-Files AS CHARACTER NO-UNDO.
DEFINE VARIABLE fnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE fileok AS LOGICAL NO-UNDO.

DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE entryepost AS LOGICAL NO-UNDO.
DEFINE VARIABLE epostvar AS CHARACTER NO-UNDO.

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

/* Name of first Frame and/or Browse and/or first Query                 */
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
BRW_EGNAEPOST FILL-IN-AMNE FILL-IN-FILSOK SELECT-FILES FBTN_OUTLOOK BTN_SKR ~
BTN_NY BTN_AND BTN_BORT EDITOR_MEDD BRW_ANV FILL-IN_SFORNAMN ~
FILL-IN_SEFTERNAMN BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_MOTTAGARE FILL-IN-AMNE ~
FILL-IN-FILSOK SELECT-FILES FILL-IN-FILNAMN EDITOR_MEDD FILL-IN_SFORNAMN ~
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

DEFINE BUTTON BTN_SKR 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OUTLOOK 
     LABEL "Från Outlook" 
     SIZE 14 BY 1.

DEFINE VARIABLE EDITOR_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 9
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-AMNE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ämne" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FILNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filnamn" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 TOOLTIP "Endast dokument namn" NO-UNDO.

DEFINE VARIABLE FILL-IN-FILSOK AS CHARACTER FORMAT "X(256)":U 
     LABEL "Infoga fil" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 TOOLTIP "Obs! Hela sökvägen till filen måste vara med!" NO-UNDO.

DEFINE VARIABLE FILL-IN_MOTTAGARE AS CHARACTER FORMAT "x(256)" 
     LABEL "Mottagare" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 TOOLTIP "Om fältet lämnas blank tas e-post adress från register".

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
     SIZE 44.13 BY 2.42
     BGCOLOR 8 .

DEFINE VARIABLE SELECT-FILES AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 52 BY 2 TOOLTIP "Obs! Högra mustangenten ger hjälp." NO-UNDO.

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
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(256)":U
            WIDTH 15
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(256)":U
            WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 44.13 BY 6.04 TOOLTIP "OBS! Mottagarnamnet måste stå på samma sätt som i adressboken i Outlook.".

DEFINE BROWSE BRW_EGNAEPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_EGNAEPOST DIALOG-1 _STRUCTURED
  QUERY BRW_EGNAEPOST NO-LOCK DISPLAY
      eptemp.EPOST COLUMN-LABEL "E-post" FORMAT "X(256)":U WIDTH 40
  ENABLE
      eptemp.EPOST
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 44.13 BY 7.75
         TITLE "Mina egna e-postadresser".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_MOTTAGARE AT ROW 3 COL 11.5 COLON-ALIGNED
     BRW_EGNAEPOST AT ROW 3 COL 66.25
     FILL-IN-AMNE AT ROW 4.42 COL 11.5 COLON-ALIGNED
     FILL-IN-FILSOK AT ROW 5.83 COL 11.5 COLON-ALIGNED
     SELECT-FILES AT ROW 7.25 COL 13.5 NO-LABEL
     FBTN_OUTLOOK AT ROW 8 COL 111.5
     BTN_SKR AT ROW 9.08 COL 111.5
     FILL-IN-FILNAMN AT ROW 9.71 COL 11.5 COLON-ALIGNED
     BTN_NY AT ROW 11 COL 70.25
     BTN_AND AT ROW 11 COL 83.5
     BTN_BORT AT ROW 11 COL 96.63
     EDITOR_MEDD AT ROW 13.71 COL 1.5 NO-LABEL
     BRW_ANV AT ROW 16 COL 66.25
     FILL-IN_SFORNAMN AT ROW 23.13 COL 86.88 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 24.13 COL 86.88 COLON-ALIGNED
     BTN_OK AT ROW 25.5 COL 96.5
     BTN_AVB AT ROW 25.5 COL 111.5
     "Meddelande:" VIEW-AS TEXT
          SIZE 12.5 BY .63 AT ROW 12.79 COL 1.5
     "E-post meddelande" VIEW-AS TEXT
          SIZE 33 BY 1 AT ROW 1.5 COL 1.5
          FONT 17
     RECT-49 AT ROW 22.79 COL 66.25
     IMAGE-1 AT ROW 23.25 COL 67.38
     SPACE(50.24) SKIP(2.58)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "E-post från Guru".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: eptemp T "?" NO-UNDO temp-db eptemp
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
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
"personaltemp.FORNAMN" "Förnamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_ANV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_EGNAEPOST
/* Query rebuild information for BROWSE BRW_EGNAEPOST
     _TblList          = "Temp-Tables.eptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.eptemp.EPOST
"eptemp.EPOST" "E-post" "X(256)" "character" ? ? ? ? ? ? yes ? no no "40" yes no no "U" "" ""
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
   /*status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   
   IF Guru.Konstanter:globforetag = "ELPA"    OR 
      Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "UMEA"
   THEN DO:
      
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = anvandartemp.PERSONALKOD
      NO-LOCK NO-ERROR.
      IF AVAILABLE personaltemp THEN DO:
         FILL-IN_MOTTAGARE = personaltemp.EFTERNAMN + " " + personaltemp.FORNAMN.
      END.
      ELSE DO:
         FILL-IN_MOTTAGARE = anvandartemp.AV-NAMN.
      END.
   END.
   ELSE DO:
      FILL-IN_MOTTAGARE = anvandartemp.AV-NAMN.
   END.   
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
            QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
            IF svar THEN DO:
               APPLY "CHOOSE" TO BTN_BORT IN FRAME {&FRAME-NAME}.                  
               eptemp.EPOST:READ-ONLY IN BROWSE BRW_EGNAEPOST = TRUE.
            END.
            ELSE DO:
               musz = TRUE.
               APPLY "ENTRY" TO eptemp.EPOST IN BROWSE BRW_EGNAEPOST.
               RETURN NO-APPLY.
            END.
         END.               
         musz = FALSE.
      END.
      ELSE eptemp.EPOST:READ-ONLY IN BROWSE BRW_EGNAEPOST = TRUE.
   END.   
   entryepost = FALSE.
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
   musz = TRUE.
   {BORTBRWPROC.I}
   RETURN.
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
   DEFINE VARIABLE tempnamn AS CHARACTER.
   ASSIGN
   FILL-IN_MOTTAGARE = INPUT FILL-IN_MOTTAGARE
   FILL-IN-AMNE = INPUT FILL-IN-AMNE
   FILL-IN-FILNAMN = INPUT FILL-IN-FILNAMN
   FILL-IN-FILSOK = INPUT FILL-IN-FILSOK
   EDITOR_MEDD = INPUT EDITOR_MEDD.

   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FILSOK IN FRAME {&FRAME-NAME}.
   tempnamn = SELECT-FILES:LIST-ITEMS.
   IF tempnamn = ? THEN tempnamn = "".
   status-ok = SESSION:SET-WAIT-STATE("GENERAL").
   DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.  
   file-info:file-name = ".".
   orgdir = file-info:full-pathname.

   RUN EPOST.P (INPUT "",INPUT FILL-IN_MOTTAGARE,INPUT FILL-IN-AMNE,INPUT EDITOR_MEDD,
                INPUT tempnamn, INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick, OUTPUT efel).
   RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).
   status-ok = SESSION:SET-WAIT-STATE("").
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

/*    /*GG 20060208 - För dom som sitter på citrixlösning...funkar ej MAPI epostfunktionen*/      */
/*    {SMTPEXTRA.I}                                                                               */
/*                                                                                                */
/*    ELSE DO:                                                                                    */
/*       APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FILSOK IN FRAME {&FRAME-NAME}.                  */
/*       tempnamn = SELECT-FILES:LIST-ITEMS.                                                      */
/*       IF tempnamn = ? THEN tempnamn = "".                                                      */
/*       status-ok = SESSION:SET-WAIT-STATE("GENERAL").                                           */
/*       DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.                                             */
/*       file-info:file-name = ".".                                                               */
/*       orgdir = file-info:full-pathname.                                                        */
/*       RUN EPOST.P (INPUT "",INPUT FILL-IN_MOTTAGARE,INPUT FILL-IN-AMNE,INPUT EDITOR_MEDD,      */
/*                    INPUT tempnamn, INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT skick, OUTPUT efel). */
/*       RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).                                       */
/*       status-ok = SESSION:SET-WAIT-STATE("").                                                  */
/*       IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.                                       */
/*       IF skick = TRUE THEN RETURN NO-APPLY.                                                    */
/*    END.                                                                                        */
/*    MESSAGE "Vill du spara dina valda epostadresser för snabbare åtkomst ?"                     */
/*    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.                            */
/*    IF val = TRUE THEN DO:                                                                      */
/*       FOR EACH eptemp WHERE eptemp.EPOST = "":                                                 */
/*          DELETE eptemp.                                                                        */
/*       END.                                                                                     */
/*       FOR EACH eptemp:                                                                         */
/*          eptemp.AONR = eptemp.EPOST.                                                           */
/*       END.                                                                                     */
/*       tthandle = TEMP-TABLE eptemp:HANDLE.                                                     */
/*       FIND FIRST sparaladdatemp NO-ERROR.                                                      */
/*       IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.                              */
/*       ASSIGN                                                                                   */
/*       sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/                       */
/*       sparaladdatemp.BENAMNING = "EPOST" /*Benämnings sufix, i detta fall ELPAO$STOR*/         */
/*       sparaladdatemp.TABVAL = "eptemp" /*Tabellnamn*/                                          */
/*       sparaladdatemp.FALTVALAO = "AONR" /*Character field*/                                    */
/*       sparaladdatemp.FALTVALDEL = "" /*Integer field*/                                         */
/*       sparaladdatemp.FALTVALDATE = "".  /*DATE field*/                                         */
/*       RUN sparabrw_UI IN brwproc[2]                                                            */
/*          (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp).                            */
/*    END.                                                                                        */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-1
ON CHOOSE OF BTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).          
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.      
   END.   
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-1
ON MOUSE-MENU-CLICK OF BTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
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
   /*SYSTEM-DIALOG GET-FILE filnamn
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


&Scoped-define SELF-NAME FILL-IN-FILSOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FILSOK DIALOG-1
ON ANY-KEY OF FILL-IN-FILSOK IN FRAME DIALOG-1 /* Infoga fil */
DO:        
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FILSOK IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FILSOK DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FILSOK IN FRAME DIALOG-1 /* Infoga fil */
DO:
   FILL-IN-FILSOK = INPUT FILL-IN-FILSOK.
   IF FILL-IN-FILSOK = "" THEN LEAVE.
   ELSE FILE-INFO:FILE-NAME = FILL-IN-FILSOK.
   IF FILE-INFO:PATHNAME = ? THEN DO:
      MESSAGE "Kan inte hitta filen:" FILL-IN-FILSOK VIEW-AS ALERT-BOX.
      LEAVE.
   END.
   ELSE IF FILE-INFO:FILE-TYPE BEGINS "D" THEN DO:
      MESSAGE "Infoga fil kan inte vara en katalog!" VIEW-AS ALERT-BOX.
      LEAVE.
   END.
   ELSE DO: 
      SELECT-FILES:ADD-LAST(FILE-INFO:PATHNAME).
      RUN filestring_UI.
      FILL-IN-FILSOK = "".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   DEFINE VARIABLE kfunc AS CHARACTER NO-UNDO.
   kfunc = KEYFUNCTION(LASTKEY).         
   IF kfunc = "DELETE-CHARACTER" THEN DO:
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
   FOR EACH personaltemp :
      FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE anvandartemp THEN DELETE personaltemp.
   END.
   IF utvar = TRUE THEN RUN tidut_UI.
   RUN enable_UI.  
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
   
   IF Guru.Konstanter:varforetypchar[50] NE "" THEN DO:
      REPEAT i = 1 TO NUM-ENTRIES(Guru.Konstanter:varforetypchar[50]):
         SELECT-FILES:ADD-LAST(ENTRY(i,Guru.Konstanter:varforetypchar[50])).
      END.
      RUN filestring_UI.
      DISPLAY SELECT-FILES WITH FRAME {&FRAME-NAME}. 

   END.
   {FRMSIZED.I}
   eptemp.EPOST:READ-ONLY IN BROWSE BRW_EGNAEPOST = TRUE.
   IF AVAILABLE eptemp THEN DO:
      status-ok = BRW_EGNAEPOST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      /*
      DISPLAY  eptemp.EPOST WITH BROWSE BRW_EGNAEPOST.
      status-ok = BRW_EGNAEPOST:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      */
   END.
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      ASSIGN
      BRW_ANV:HIDDEN = TRUE
      FILL-IN_SEFTERNAMN:HIDDEN = TRUE 
      FILL-IN_SFORNAMN:HIDDEN = TRUE
      RECT-49:HIDDEN = TRUE
      IMAGE-1:HIDDEN = TRUE.
   END.
   {musarrow.i}
   {DIA_M_SLUT.I} 
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
  DISPLAY FILL-IN_MOTTAGARE FILL-IN-AMNE FILL-IN-FILSOK SELECT-FILES 
          FILL-IN-FILNAMN EDITOR_MEDD FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 
      WITH FRAME DIALOG-1.
  ENABLE RECT-49 IMAGE-1 FILL-IN_MOTTAGARE BRW_EGNAEPOST FILL-IN-AMNE 
         FILL-IN-FILSOK SELECT-FILES FBTN_OUTLOOK BTN_SKR BTN_NY BTN_AND 
         BTN_BORT EDITOR_MEDD BRW_ANV FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 
         BTN_OK BTN_AVB 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tidut_UI DIALOG-1 
PROCEDURE tidut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*
   EDITOR_MEDD:SCROLLBAR-VERTICAL IN FRAME {&FRAME-NAME} = TRUE. 
    */
   FOR EACH tidut NO-LOCK:
      EDITOR_MEDD = EDITOR_MEDD + tidut.UT + CHR(10).
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

