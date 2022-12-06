&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW    
&Scoped-define SHARED 
{FLEXTAB.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE hjvart AS CHARACTER FORMAT "X(3)" NO-UNDO.
{OMRTEMPW.I}
DEFINE SHARED TEMP-TABLE flbettemp
   FIELD PERSONALKOD AS CHARACTER    
   FIELD DATUM AS DATE  
   FIELD TIMMAR AS DECIMAL  
   FIELD ANVANDARE AS CHARACTER  
   FIELD ACCFORE AS DECIMAL  
   FIELD ACCEFTER AS DECIMAL  
   FIELD FREC AS RECID
   INDEX PKOD IS PRIMARY PERSONALKOD DATUM
   INDEX DATUM DATUM PERSONALKOD
   INDEX TIMMAR TIMMAR 
   INDEX ACCFORE   ACCFORE   
   INDEX ACCEFTER  ACCEFTER  
   INDEX ANVANDARE ANVANDARE.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE tempvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE hjtim AS DECIMAL FORMAT "99.99" NO-UNDO.
&Scoped-define NEW     
&Scoped-define SHARED SHARED 
{ANVPERS.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define BROWSE-NAME BRW_PERS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES personaltemp

/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.FORNAMN personaltemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PERS personaltemp
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH personaltemp NO-LOCK ~
    BY personaltemp.PERSONALKOD
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK ~
    BY personaltemp.PERSONALKOD.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for DIALOG-BOX DIALOG-3                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_OMR FILL-IN-PKOD BRW_PERS BTN_NVE-2 ~
FILL-IN_DATUM BTN_FVE-2 FILL-IN_TIMMAR FILL-IN_SPERSONALKOD ~
FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TEXT CMB_OMR FILL-IN-PKOD ~
FILL-IN_DATUM FILL-IN_TIMMAR FILL-IN-SOKPA FILL-IN_SPERSONALKOD ~
FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN-SOKPA AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr/personal för:" 
     VIEW-AS FILL-IN 
     SIZE 29.88 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_ACCFLEX AS DECIMAL FORMAT "->>9.99" INITIAL 0 
     LABEL "Flexsaldo" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" INITIAL 02/23/00 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "E-namn" 
     VIEW-AS FILL-IN 
     SIZE 14.5 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(15)" 
     LABEL "F-namn" 
     VIEW-AS FILL-IN 
     SIZE 11.88 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .83.

DEFINE VARIABLE FILL-IN_TIMMAR AS DECIMAL FORMAT "->>9.99" INITIAL 0 
     LABEL "Timmar" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE RECTANGLE RECT-SOK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75.88 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS DIALOG-3 _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "x(5)":U
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
            WIDTH 29.75
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
            WIDTH 31.25
  ENABLE
      personaltemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 75.88 BY 11.17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     FILL-IN-TEXT AT ROW 1.63 COL 45.75 COLON-ALIGNED NO-LABEL
     CMB_OMR AT ROW 2.58 COL 53.5 COLON-ALIGNED NO-LABEL
     FILL-IN-PKOD AT ROW 3.5 COL 13 COLON-ALIGNED
     BRW_PERS AT ROW 4 COL 29.75
     BTN_NVE-2 AT ROW 4.5 COL 24.63
     FILL-IN_DATUM AT ROW 4.75 COL 13 COLON-ALIGNED
     BTN_FVE-2 AT ROW 5.38 COL 24.63
     FILL-IN_TIMMAR AT ROW 5.96 COL 13 COLON-ALIGNED
     FILL-IN_ACCFLEX AT ROW 7.17 COL 13 COLON-ALIGNED
     FILL-IN-SOKPA AT ROW 15.83 COL 29 COLON-ALIGNED NO-LABEL
     FILL-IN_SPERSONALKOD AT ROW 15.83 COL 49.5 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 15.83 COL 67.5 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 15.83 COL 88.5 COLON-ALIGNED
     BTN_OK AT ROW 17.13 COL 76.63
     BTN_AVBRYT AT ROW 17.13 COL 91.63
     RECT-SOK AT ROW 15.63 COL 29.75
     SPACE(0.61) SKIP(1.69)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ny/ändra flex justering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_PERS FILL-IN-PKOD DIALOG-3 */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

ASSIGN 
       BRW_PERS:MAX-DATA-GUESS IN FRAME DIALOG-3         = 300.

/* SETTINGS FOR FILL-IN FILL-IN-SOKPA IN FRAME DIALOG-3
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-3
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ACCFLEX IN FRAME DIALOG-3
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ACCFLEX:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-SOK IN FRAME DIALOG-3
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.personaltemp.PERSONALKOD|yes"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no "29.75" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no "31.25" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Ny/ändra flex justering */
DO:
  {muswait.i}
   musz = TRUE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS DIALOG-3
ON LEAVE OF BRW_PERS IN FRAME DIALOG-3
DO:
   IF INPUT FILL-IN-PKOD = "" THEN DO:
      MESSAGE "Enhet/Sign kan inte vara blank." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BRW_PERS IN FRAME {&FRAME-NAME}.
   END.    
   FILL-IN-PKOD = INPUT FILL-IN-PKOD.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD AND 
   personaltemp.AKTIV = TRUE NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Enhet/Sign" FILL-IN-PKOD "finns inte eller är inaktiv." VIEW-AS ALERT-BOX.       
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BRW_PERS IN FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS DIALOG-3
ON VALUE-CHANGED OF BRW_PERS IN FRAME DIALOG-3
DO:
   status-ok = BRW_PERS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_PERS.
   IF musz = FALSE THEN DO:
      FILL-IN-PKOD = personaltemp.PERSONALKOD.
      DISPLAY FILL-IN-PKOD  WITH FRAME {&FRAME-NAME}.         
   END.
   musz = FALSE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* Avbryt */
DO:
   {muswait.i}
   musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 DIALOG-3
ON CHOOSE OF BTN_FVE-2 IN FRAME DIALOG-3 /* - */
DO: 
   ASSIGN
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.   
   FILL-IN_DATUM = FILL-IN_DATUM - 1.   
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 DIALOG-3
ON CHOOSE OF BTN_NVE-2 IN FRAME DIALOG-3 /* + */
DO:   
   ASSIGN
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.   
   FILL-IN_DATUM = FILL-IN_DATUM + 1.
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}
   FILL-IN-PKOD = INPUT FILL-IN-PKOD.
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.
   FILL-IN_TIMMAR = INPUT FILL-IN_TIMMAR.
   IF FILL-IN-PKOD = "" THEN DO:
      MESSAGE "Enhet/Sign kan inte vara blankt." VIEW-AS ALERT-BOX.    
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END. 
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD AND 
   personaltemp.AKTIV = TRUE USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Enhet/Sign" FILL-IN-PKOD "finns inte eller är inaktiv." VIEW-AS ALERT-BOX.   
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.    
   ELSE DO:
      FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
      NO-LOCK NO-ERROR.   
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
      FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE flexavttemp THEN DO:
         MESSAGE "Enhet/Sign" FILL-IN-PKOD "har inte flexavtal." VIEW-AS ALERT-BOX.   
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF flexavttemp.FLEXTID = FALSE THEN DO:
         MESSAGE "Enhet/Sign" FILL-IN-PKOD "har inte flexavtal." VIEW-AS ALERT-BOX.   
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.
   END.   
   FIND FIRST flbettemp WHERE flbettemp.PERSONALKOD = FILL-IN-PKOD AND flbettemp.DATUM = FILL-IN_DATUM
   AND RECID(flbettemp) NE brec NO-LOCK NO-ERROR.
   IF AVAILABLE flbettemp THEN DO:
      MESSAGE "Enhet/Sign finns redan upplag för detta datum använd ändra istället." VIEW-AS ALERT-BOX.   
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.    
   FIND FIRST flbettemp WHERE flbettemp.PERSONALKOD = FILL-IN-PKOD AND flbettemp.DATUM > FILL-IN_DATUM
   AND RECID(flbettemp) NE brec NO-LOCK NO-ERROR.
   IF AVAILABLE flbettemp THEN DO:
      MESSAGE "Flexsaldo finns redan upplagt för senare datum " flbettemp.DATUM VIEW-AS ALERT-BOX.   
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END. 
   FIND FIRST flexregtemp USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF FILL-IN_DATUM LE flexregtemp.SALDOKORD THEN DO:    
      MESSAGE "Flexen är låst till och med." flexregtemp.SALDOKORD  VIEW-AS ALERT-BOX.   
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END. 
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT FILL-IN-PKOD,INPUT FILL-IN_DATUM,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT FILL-IN-PKOD,INPUT FILL-IN_DATUM,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.      
   END.   
   FIND flbettemp WHERE RECID(flbettemp) = brec NO-ERROR.
   hjtim = flbettemp.TIMMAR.
   ASSIGN 
   flbettemp.PERSONALKOD = FILL-IN-PKOD
   flbettemp.DATUM = FILL-IN_DATUM
   flbettemp.TIMMAR = FILL-IN_TIMMAR
   flbettemp.ANVANDARE = Guru.Konstanter:globanv.
   IF hjvart = "NYA" THEN DO:
      flbettemp.FREC = ?.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN FLUTBETH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT 2,INPUT flbettemp.FREC,INPUT hjtim,INPUT-OUTPUT TABLE flbettemp).
   END.
   ELSE DO:
      RUN FLUTBETH.P 
      (INPUT 2,INPUT flbettemp.FREC,INPUT hjtim,INPUT-OUTPUT TABLE flbettemp).
   END.              
   FIND FIRST flbettemp WHERE flbettemp.PERSONALKOD = FILL-IN-PKOD AND
   flbettemp.DATUM = FILL-IN_DATUM AND flbettemp.TIMMAR = FILL-IN_TIMMAR NO-LOCK NO-ERROR.
   brec = RECID(flbettemp).
   APPLY "GO" TO FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-3
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-3
DO:
   CMB_OMR = INPUT CMB_OMR.            
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
   USE-INDEX OMRNAMN NO-LOCK NO-ERROR.              
   RUN omrcheck_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PKOD DIALOG-3
ON LEAVE OF FILL-IN-PKOD IN FRAME DIALOG-3 /* Enhet/Sign */
DO:
   FILL-IN-PKOD = INPUT FILL-IN-PKOD.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUM DIALOG-3
ON MOUSE-MENU-CLICK OF FILL-IN_DATUM IN FRAME DIALOG-3 /* Datum */
DO:
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_DATUM.
   RUN AlmanBtn.w.
   FILL-IN_DATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SEFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN DIALOG-3
ON ANY-KEY OF FILL-IN_SEFTERNAMN IN FRAME DIALOG-3 /* E-namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SEFTERNAMN IN FRAME DIALOG-3 /* E-namn */
DO:
   FILL-IN_SEFTERNAMN = INPUT FILL-IN_SEFTERNAMN.
   IF FILL-IN_SEFTERNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "EFTERNAMN", INPUT FILL-IN_SEFTERNAMN).
   IF AVAILABLE personaltemp THEN DO: 
      FILL-IN-PKOD = personaltemp.PERSONALKOD.
      DISPLAY FILL-IN-PKOD WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-3
ON ANY-KEY OF FILL-IN_SFORNAMN IN FRAME DIALOG-3 /* F-namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SFORNAMN IN FRAME DIALOG-3 /* F-namn */
DO:
   FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
   IF FILL-IN_SFORNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "FORNAMN", INPUT FILL-IN_SFORNAMN).  
   IF AVAILABLE personaltemp THEN DO: 
      FILL-IN-PKOD = personaltemp.PERSONALKOD.
      DISPLAY FILL-IN-PKOD WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-3
ON ANY-KEY OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-3 /* Enhet/Sign */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-3 /* Enhet/Sign */
DO:
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.
   IF FILL-IN_SPERSONALKOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "PERSONALKOD", INPUT FILL-IN_SPERSONALKOD).   
   IF AVAILABLE personaltemp THEN DO: 
      FILL-IN-PKOD = personaltemp.PERSONALKOD.
      DISPLAY FILL-IN-PKOD WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


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
   {ALLSTARTDYN.I}
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FLEXTAB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).         END.
   ELSE DO:
      RUN FLEXTAB.P 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).      
   END.
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").
   FOR EACH omrtemp:
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).   
   END.
   ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
   CMB_OMR = INPUT CMB_OMR.
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}. 
   FIND flbettemp WHERE RECID(flbettemp) = brec NO-LOCK NO-ERROR.
   IF flbettemp.PERSONALKOD = "" THEN DO:
      ASSIGN FILL-IN-PKOD = ""
      FILL-IN_DATUM = TODAY
      FILL-IN_TIMMAR = 0.
   END.
   ELSE DO:
      ASSIGN FILL-IN-PKOD = flbettemp.PERSONALKOD
      FILL-IN_DATUM= flbettemp.DATUM
      FILL-IN_TIMMAR = flbettemp.TIMMAR.      
   END.   
   FILL-IN-SOKPA = "Sök på:".
   RUN enable_UI.       
   {FRMSIZED.I}
   RUN omrcheck_UI.
   IF hjvart = "AND" THEN DISABLE BRW_PERS WITH FRAME {&FRAME-NAME}.
   IF flbettemp.PERSONALKOD NE "" THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = flbettemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE(personaltemp) THEN DO:         
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(personaltemp)).
         RUN lastselectdyn_UI IN brwproc[1].               
      END.
   END.
   ASSIGN
   FILL-IN-TEXT = "Visa personal för:"
   FILL-IN-TEXT:HIDDEN = TRUE.
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   personaltemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_PERS = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-TEXT CMB_OMR FILL-IN-PKOD FILL-IN_DATUM FILL-IN_TIMMAR 
          FILL-IN-SOKPA FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 
      WITH FRAME DIALOG-3.
  ENABLE CMB_OMR FILL-IN-PKOD BRW_PERS BTN_NVE-2 FILL-IN_DATUM BTN_FVE-2 
         FILL-IN_TIMMAR FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN 
         FILL-IN_SEFTERNAMN BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrcheck_UI DIALOG-3 
PROCEDURE omrcheck_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO: 
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
   END.
   ELSE DO:
      tempvar = " WHERE personaltemp.OMRADE = """ + STRING(omrtemp.OMRADE) + '"'.
      RUN openbdyn_UI IN brwproc[1] (INPUT tempvar).
   END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

