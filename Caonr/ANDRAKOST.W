&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


/* Temp-Table and Buffer definitions                                    */
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER rad AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}
{LEVTEMP.I}
&Scoped-define SHARED SHARED
{KOSTREG.I}
{DIRDEF.I}
{AONRDEF.I}


DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE kostrec AS RECID NO-UNDO.
DEFINE VARIABLE kostrec2 AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE andkostapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_LEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES levtemp

/* Definitions for BROWSE BRW_LEV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LEV levtemp.LEVKOD levtemp.LEVNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LEV levtemp.LEVKOD ~
levtemp.LEVNAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_LEV levtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_LEV levtemp
&Scoped-define QUERY-STRING-BRW_LEV FOR EACH levtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LEV OPEN QUERY BRW_LEV FOR EACH levtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LEV levtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LEV levtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_LEVKOD FILL-IN_FAKTNR ~
FILL-IN_BENAMNING FILL-IN_BOKKONTO BTN_NVE FILL-IN_REGDATUM BTN_FVE ~
FILL-IN_INKOMST FILL-IN_PERSKOST FILL-IN_TRAKTKOST FILL-IN_MASKKOST ~
FILL-IN_MTRL FILL-IN_OVRKR FILL-IN_MOMS FILL-IN_UPPLAGT BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_LEVKOD FILL-IN_FAKTNR ~
FILL-IN_BENAMNING FILL-IN_BOKKONTO FILL-IN_REGDATUM FILL-IN_INKOMST ~
FILL-IN_PERSKOST FILL-IN_TRAKTKOST FILL-IN_MASKKOST FILL-IN_MTRL ~
FILL-IN_OVRKR FILL-IN_MOMS FILL-IN_BETDATUM FILL-IN_UPPLAGT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_REG AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_BENAMNING AS CHARACTER FORMAT "X(42)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN_BETDATUM AS DATE FORMAT "99/99/99" 
     LABEL "In.datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_BOKKONTO AS CHARACTER FORMAT "X(8)" 
     LABEL "Bokföringskonto" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_FAKTNR AS CHARACTER FORMAT "x(15)" 
     LABEL "Ref. nr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN_FAKTURERAD AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Fakturerad" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_INKOMST AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE FILL-IN_LEVKOD AS CHARACTER FORMAT "x(4)" 
     LABEL "Lev-id" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE FILL-IN_MASKKOST AS DECIMAL FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Mask.kost." 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_MOMS AS DECIMAL FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Moms" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_MTRL AS DECIMAL FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Mtrl kr" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_OVRKR AS DECIMAL FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Övrig kostnad" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_PERSKOST AS DECIMAL FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Pers.kost." 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_REGDATUM AS DATE FORMAT "99/99/99" 
     LABEL "Bokf.datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_TRAKTKOST AS DECIMAL FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Trakt.kost." 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_UPPLAGT AS LOGICAL FORMAT "Automatiskt/Manuellt":U INITIAL NO 
     LABEL "Upplagt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_LEV FOR 
      levtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LEV DIALOG-1 _STRUCTURED
  QUERY BRW_LEV NO-LOCK DISPLAY
      levtemp.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U
      levtemp.LEVNAMN FORMAT "x(25)":U
  ENABLE
      levtemp.LEVKOD
      levtemp.LEVNAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 34.5 BY 12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_LEVKOD AT ROW 1.5 COL 18.63 COLON-ALIGNED
     FILL-IN_FAKTNR AT ROW 2.71 COL 18.63 COLON-ALIGNED
     BRW_LEV AT ROW 3.17 COL 37.5
     FILL-IN_BENAMNING AT ROW 3.92 COL 18.63 COLON-ALIGNED
     FILL-IN_BOKKONTO AT ROW 5.13 COL 18.63 COLON-ALIGNED
     BTN_NVE AT ROW 6.33 COL 32
     FILL-IN_REGDATUM AT ROW 6.67 COL 18.63 COLON-ALIGNED
     BTN_FVE AT ROW 7.17 COL 32
     FILL-IN_INKOMST AT ROW 8 COL 18.63 COLON-ALIGNED NO-LABEL
     BTN_NVE-2 AT ROW 8.08 COL 32
     BTN_FVE-2 AT ROW 8.96 COL 32
     FILL-IN_PERSKOST AT ROW 9.29 COL 18.63 COLON-ALIGNED
     FILL-IN_TRAKTKOST AT ROW 10.58 COL 18.63 COLON-ALIGNED
     FILL-IN_MASKKOST AT ROW 11.88 COL 18.63 COLON-ALIGNED
     FILL-IN_MTRL AT ROW 13.17 COL 18.63 COLON-ALIGNED
     FILL-IN_OVRKR AT ROW 14.46 COL 18.5 COLON-ALIGNED
     FILL-IN_MOMS AT ROW 15.75 COL 18.63 COLON-ALIGNED
     FILL-IN_BETDATUM AT ROW 17.04 COL 18.63 COLON-ALIGNED
     FILL-IN_FAKTURERAD AT ROW 18.33 COL 18.63 COLON-ALIGNED
     FILL-IN_UPPLAGT AT ROW 19.5 COL 18.75 COLON-ALIGNED
     BTN_REG AT ROW 19.5 COL 43
     BTN_AVS AT ROW 19.5 COL 58
     "Intäkter:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 8 COL 10.5
          FGCOLOR 9 
     SPACE(53.49) SKIP(11.74)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra eller registrera kostnader":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: levtemp T "?" NO-UNDO temp-db levtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_LEV FILL-IN_FAKTNR DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_LEV IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_LEV:HIDDEN  IN FRAME DIALOG-1                = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_BETDATUM IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_FAKTURERAD IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FAKTURERAD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LEV
/* Query rebuild information for BROWSE BRW_LEV
     _TblList          = "Temp-Tables.levtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.levtemp.LEVKOD
"LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.levtemp.LEVNAMN
"LEVNAMN" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändra eller registrera kostnader */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(andkostapph) THEN DELETE PROCEDURE andkostapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra eller registrera kostnader */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LEV
&Scoped-define SELF-NAME BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEV DIALOG-1
ON LEAVE OF BRW_LEV IN FRAME DIALOG-1
DO:
   FILL-IN_LEVKOD = INPUT FILL-IN_LEVKOD.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = FILL-IN_LEVKOD AND
   levtemp.BORTTAG = FALSE
   USE-INDEX LEV NO-LOCK NO-ERROR.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEV DIALOG-1
ON VALUE-CHANGED OF BRW_LEV IN FRAME DIALOG-1
DO:
   IF AVAILABLE levtemp THEN FILL-IN_LEVKOD = levtemp.LEVKOD.     
   DISPLAY FILL-IN_LEVKOD  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME levtemp.LEVNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL levtemp.LEVNAMN BRW_LEV _BROWSE-COLUMN DIALOG-1
ON MOUSE-SELECT-CLICK OF levtemp.LEVNAMN IN BROWSE BRW_LEV /* Namn */
DO:
  MESSAGE "WWW"
  VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   IF VALID-HANDLE(andkostapph) THEN DELETE PROCEDURE andkostapph.
   musz = TRUE.
   APPLY "GO" TO BTN_AVS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON GO OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE DIALOG-1
ON CHOOSE OF BTN_FVE IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN_REGDATUM = INPUT FILL-IN_REGDATUM.   
   FILL-IN_REGDATUM = FILL-IN_REGDATUM - 1.      
   DISPLAY FILL-IN_REGDATUM WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 DIALOG-1
ON CHOOSE OF BTN_FVE-2 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN_BETDATUM = INPUT FILL-IN_BETDATUM.      
   FILL-IN_BETDATUM = FILL-IN_BETDATUM - 1.     
   IF FILL-IN_BETDATUM = ? THEN FILL-IN_BETDATUM = TODAY. 
   DISPLAY FILL-IN_BETDATUM WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE DIALOG-1
ON CHOOSE OF BTN_NVE IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN_REGDATUM = INPUT FILL-IN_REGDATUM.   
   FILL-IN_REGDATUM = FILL-IN_REGDATUM + 1.        
   DISPLAY FILL-IN_REGDATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 DIALOG-1
ON CHOOSE OF BTN_NVE-2 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN_BETDATUM = INPUT FILL-IN_BETDATUM.   
   FILL-IN_BETDATUM = FILL-IN_BETDATUM + 1.   
   IF FILL-IN_BETDATUM = ? THEN FILL-IN_BETDATUM = TODAY.     
   DISPLAY FILL-IN_BETDATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   ASSIGN
   musz = FALSE
   FILL-IN_INKOMST      = INPUT FILL-IN_INKOMST
   FILL-IN_BENAMNING    = INPUT FILL-IN_BENAMNING
   FILL-IN_BOKKONTO     = INPUT FILL-IN_BOKKONTO
   FILL-IN_BETDATUM     = INPUT FILL-IN_BETDATUM
   FILL-IN_FAKTNR       = INPUT FILL-IN_FAKTNR
   FILL-IN_MASKKOST     = INPUT FILL-IN_MASKKOST    
   FILL-IN_MOMS         = INPUT FILL-IN_MOMS
   FILL-IN_MTRL         = INPUT FILL-IN_MTRL
   FILL-IN_OVRKR        = INPUT FILL-IN_OVRKR
   FILL-IN_PERSKOST     = INPUT FILL-IN_PERSKOST
   FILL-IN_REGDATUM     = INPUT FILL-IN_REGDATUM
   FILL-IN_TRAKTKOST    = INPUT FILL-IN_TRAKTKOST
   FILL-IN_LEVKOD = INPUT FILL-IN_LEVKOD
   FILL-IN_UPPLAGT = INPUT FILL-IN_UPPLAGT.
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN DO:
      IF INPUT FILL-IN_FAKTURERAD = ? THEN FILL-IN_FAKTURERAD = ?.
      ELSE FILL-IN_FAKTURERAD = INPUT FILL-IN_FAKTURERAD.
   END.
   FILL-IN_FAKTURERAD:HIDDEN = TRUE.
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN DO:
      DISPLAY FILL-IN_FAKTURERAD WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globanv = {setuser.I} THEN DO:
         ENABLE FILL-IN_FAKTURERAD WITH FRAME {&FRAME-NAME}.
      END.
   END.
   FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND 
   kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad NO-LOCK NO-ERROR.
   IF AVAILABLE kostregtemp THEN DO:
      ASSIGN 
      kostregtemp.AONR = aonrvar
      kostregtemp.DELNR = delnrvar
      kostregtemp.INKOMST = FILL-IN_INKOMST
      kostregtemp.BENAMNING = FILL-IN_BENAMNING
      kostregtemp.BOKKONTO = FILL-IN_BOKKONTO
      kostregtemp.FAKTNR = FILL-IN_FAKTNR  
      kostregtemp.LEVKOD = FILL-IN_LEVKOD  
      kostregtemp.MASKKOST = FILL-IN_MASKKOST
      kostregtemp.MOMS = FILL-IN_MOMS      
      kostregtemp.MTRL = FILL-IN_MTRL      
      kostregtemp.OVRKR = FILL-IN_OVRKR    
      kostregtemp.PERSKOST = FILL-IN_PERSKOST
      kostregtemp.BETDATUM = FILL-IN_BETDATUM
      kostregtemp.REGDATUM  = FILL-IN_REGDATUM
      kostregtemp.TRAKTKOST  = FILL-IN_TRAKTKOST
      kostregtemp.FAKTURERAD = FILL-IN_FAKTURERAD
      kostregtemp.KOSTAUTO = FILL-IN_UPPLAGT.
      SUBSTRING(kostregtemp.ANVANDARE,1,12) = Guru.Konstanter:globanv.
      IF kostregtemp.BETDATUM = ? THEN kostregtemp.BETDATUM = TODAY.
      {muswait.i}  
      RUN btnok IN andkostapph (INPUT aonrvar,INPUT delnrvar,INPUT rad,INPUT Guru.Konstanter:globanv,
                                INPUT TABLE kostregtemp).
      {musarrow.i}
   END.
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(andkostapph) THEN DELETE PROCEDURE andkostapph.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BETDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BETDATUM DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN_BETDATUM IN FRAME DIALOG-1 /* In.datum */
DO:   
   ASSIGN
   FILL-IN_BETDATUM = INPUT FILL-IN_BETDATUM
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_BETDATUM.
   RUN AlmanBtn.w.
   FILL-IN_BETDATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN_BETDATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_REGDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_REGDATUM DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN_REGDATUM IN FRAME DIALOG-1 /* Bokf.datum */
DO:
   ASSIGN
   FILL-IN_REGDATUM = INPUT FILL-IN_REGDATUM
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_REGDATUM.
   RUN AlmanBtn.w.
   FILL-IN_REGDATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN_REGDATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_UPPLAGT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_UPPLAGT DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_UPPLAGT IN FRAME DIALOG-1 /* Upplagt */
DO:
   FILL-IN_UPPLAGT = INPUT FILL-IN_UPPLAGT.
   IF FILL-IN_UPPLAGT = TRUE THEN FILL-IN_UPPLAGT = FALSE.
   ELSE IF FILL-IN_UPPLAGT = FALSE THEN FILL-IN_UPPLAGT = TRUE.
   DISPLAY FILL-IN_UPPLAGT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   {ALLSTARTDYN.I} 
   RUN ladda IN andkostapph (OUTPUT TABLE levtemp).
   FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND 
   kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad NO-LOCK NO-ERROR.    
   RUN grundtid_UI. 
   RUN enable_UI.       
   {FRMSIZED.I}                         
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN DO:
      DISPLAY FILL-IN_FAKTURERAD WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globanv = {setuser.I} THEN DO:
         ENABLE FILL-IN_FAKTURERAD WITH FRAME {&FRAME-NAME}.
      END.
   END.
   IF FILL-IN_UPPLAGT = TRUE THEN DO:
      MESSAGE "Du kan inte ändra automatiskt inlagda registreringar!" VIEW-AS ALERT-BOX.
      IF Guru.Konstanter:globanv NE {setuser.I} THEN DO:
         ASSIGN
         BTN_NVE:HIDDEN = TRUE
         BTN_FVE-2:HIDDEN = TRUE
         BTN_FVE:HIDDEN = TRUE
         BTN_NVE-2:HIDDEN = TRUE.
         BTN_REG:HIDDEN = TRUE.
         DISABLE FILL-IN_LEVKOD FILL-IN_FAKTNR FILL-IN_BENAMNING 
         FILL-IN_BOKKONTO  FILL-IN_REGDATUM  
         FILL-IN_BETDATUM  FILL-IN_PERSKOST FILL-IN_TRAKTKOST 
         FILL-IN_MASKKOST FILL-IN_MTRL FILL-IN_OVRKR  
         FILL-IN_MOMS FILL-IN_INKOMST BRW_LEV
         WITH FRAME DIALOG-1.
      END.
   END.
  
   my1hand = FILL-IN_LEVKOD:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BRW_LEV:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BRW_LEV:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_FAKTNR:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_OVRKR:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_MOMS:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_MOMS:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_INKOMST:MOVE-AFTER-TAB-ITEM(my1hand).   
   OPEN QUERY BRW_LEV FOR EACH levtemp WHERE levtemp.BORTTAG = FALSE
   USE-INDEX LEV NO-LOCK.    
   BRW_LEV:HIDDEN = FALSE.
   IF FILL-IN_UPPLAGT = FALSE THEN DO:
      ENABLE BRW_LEV WITH FRAME {&FRAME-NAME}.
      BRW_LEV:HIDDEN = FALSE.   
      FIND FIRST levtemp WHERE levtemp.LEVKOD = FILL-IN_LEVKOD AND
      levtemp.BORTTAG = FALSE 
      USE-INDEX LEV NO-LOCK NO-ERROR.   
      IF AVAILABLE levtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(levtemp)).
         RUN lastselectdyn_UI IN brwproc[1].   
      END.
      ELSE DO:
         APPLY "HOME" TO BRW_LEV.
         status-ok = BRW_LEV:SELECT-FOCUSED-ROW() NO-ERROR.
      END.
   END.
   IF Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "ELPA"
   OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      ASSIGN
      FILL-IN_REGDATUM :LABEL IN FRAME {&FRAME-NAME} = "Bokf.datum"
      FILL-IN_BETDATUM :LABEL IN FRAME  {&FRAME-NAME} = "Inläst.datum".
   END.
   IF Guru.Konstanter:globanv NE {setuser.I} THEN  FILL-IN_UPPLAGT:HIDDEN = TRUE.
   ON 'MOUSE-SELECT-CLICK' OF levtemp.LEVNAMN IN BROWSE BRW_LEV PERSISTENT RUN avismc_UI IN THIS-PROCEDURE.
   
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
   ASSIGN
   BRW_LEV:READ-ONLY IN FRAME {&FRAME-NAME} = FALSE.
   levtemp.LEVKOD:READ-ONLY IN BROWSE BRW_LEV = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_LEV:HANDLE IN FRAME {&FRAME-NAME}).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN ANDKOSTAPP.P PERSISTENT SET andkostapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ANDKOSTAPP.P PERSISTENT SET andkostapph.
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
  DISPLAY FILL-IN_LEVKOD FILL-IN_FAKTNR FILL-IN_BENAMNING FILL-IN_BOKKONTO 
          FILL-IN_REGDATUM FILL-IN_INKOMST FILL-IN_PERSKOST FILL-IN_TRAKTKOST 
          FILL-IN_MASKKOST FILL-IN_MTRL FILL-IN_OVRKR FILL-IN_MOMS 
          FILL-IN_BETDATUM FILL-IN_UPPLAGT 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_LEVKOD FILL-IN_FAKTNR FILL-IN_BENAMNING FILL-IN_BOKKONTO 
         BTN_NVE FILL-IN_REGDATUM BTN_FVE FILL-IN_INKOMST FILL-IN_PERSKOST 
         FILL-IN_TRAKTKOST FILL-IN_MASKKOST FILL-IN_MTRL FILL-IN_OVRKR 
         FILL-IN_MOMS FILL-IN_UPPLAGT BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid_UI DIALOG-1 
PROCEDURE grundtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/
   FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND 
   kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad NO-LOCK NO-ERROR.      
   IF vart = "NYA" THEN DO:
      FILL-IN_REGDATUM = TODAY.
   END.
   ELSE DO:
      IF AVAILABLE kostregtemp THEN DO:
         ASSIGN
         FILL-IN_REGDATUM = kostregtemp.REGDATUM
         FILL-IN_BETDATUM = kostregtemp.BETDATUM.
      END.
   END.
   IF AVAILABLE kostregtemp THEN DO:
      ASSIGN
      FILL-IN_INKOMST = kostregtemp.INKOMST 
      FILL-IN_BENAMNING = kostregtemp.BENAMNING 
      FILL-IN_BOKKONTO =  kostregtemp.BOKKONTO 
      FILL-IN_FAKTNR = kostregtemp.FAKTNR
      FILL-IN_LEVKOD = kostregtemp.LEVKOD
      FILL-IN_MASKKOST = kostregtemp.MASKKOST
      FILL-IN_MOMS = kostregtemp.MOMS 
      FILL-IN_MTRL = kostregtemp.MTRL 
      FILL-IN_OVRKR = kostregtemp.OVRKR 
      FILL-IN_PERSKOST = kostregtemp.PERSKOST   
      FILL-IN_TRAKTKOST = kostregtemp.TRAKTKOST
      FILL-IN_FAKTURERAD = kostregtemp.FAKTURERAD
      FILL-IN_UPPLAGT = kostregtemp.KOSTAUTO.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

