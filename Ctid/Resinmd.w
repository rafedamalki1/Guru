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

  Created: 95/08/18 - 10:19 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER varifran AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{KLOCKBER.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{RESDEF.I}
&Scoped-define SHARED SHARED
{FLEXTAB.I}
{DIRDEF.I}
{PHMT.I}
DEFINE VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE dummyrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE varaonr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE vardelnr AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE avrdatum AS DATE NO-UNDO. 
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE VARIABLE aonrspar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE delnrspar AS INTEGER NO-UNDO. 
DEFINE VARIABLE nattspar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE overspar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE prisspar AS DECIMAL NO-UNDO. 
DEFINE VARIABLE pristypspar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE bilspar AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
DEFINE VARIABLE feltxt AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE utstart AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE hemslut AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjslut AS DECIMAL NO-UNDO.

DEFINE BUFFER respersbuff FOR respers.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_RESA-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES respers

/* Definitions for BROWSE BRW_RESA-2                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_RESA-2 respers.DATUM respers.DAG ~
respers.Kilometer respers.START respers.SLUT respers.AONR respers.DELNR ~
respers.BILFORARE respers.NATTRAKT respers.OVERTIDUTTAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_RESA-2 respers.Kilometer ~
respers.START respers.SLUT respers.AONR respers.DELNR respers.BILFORARE ~
respers.NATTRAKT respers.OVERTIDUTTAG 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_RESA-2 respers
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_RESA-2 respers
&Scoped-define QUERY-STRING-BRW_RESA-2 FOR EACH respers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_RESA-2 OPEN QUERY BRW_RESA-2 FOR EACH respers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_RESA-2 respers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_RESA-2 respers


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UPP BTN_BYT BTN_UPPD FBTN_REG BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD FILL-IN_FORNAMN-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BYT 
     LABEL "Byt aonr":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPPD 
     LABEL "Uppdela":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_REG 
     LABEL "Slutför":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 39.88 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_RESA-2 FOR 
      respers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_RESA-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_RESA-2 DIALOG-1 _STRUCTURED
  QUERY BRW_RESA-2 NO-LOCK DISPLAY
      respers.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      respers.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      respers.Kilometer COLUMN-LABEL "Kilo-!meter" FORMAT ">>>>9":U
      respers.START COLUMN-LABEL "Start tid" FORMAT "99.99":U
      respers.SLUT COLUMN-LABEL "Slut tid" FORMAT "99.99":U
      respers.AONR COLUMN-LABEL "Aonr" FORMAT "X(256)":U WIDTH 6
      respers.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      respers.BILFORARE COLUMN-LABEL "Bil-!förare" FORMAT "Ja/Nej":U
      respers.NATTRAKT COLUMN-LABEL "Natt-!trakt." FORMAT "Ja/Nej":U
      respers.OVERTIDUTTAG COLUMN-LABEL "Övertids!uttag" FORMAT "X(1)":U
  ENABLE
      respers.Kilometer
      respers.START
      respers.SLUT
      respers.AONR
      respers.DELNR
      respers.BILFORARE
      respers.NATTRAKT
      respers.OVERTIDUTTAG
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 56.5 BY 12.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-PKOD AT ROW 3.96 COL 12.13 COLON-ALIGNED
     FILL-IN_FORNAMN-2 AT ROW 3.96 COL 22.13 COLON-ALIGNED NO-LABEL
     BRW_RESA-2 AT ROW 6.08 COL 1.5
     BTN_UPP AT ROW 19.46 COL 1.5
     BTN_BYT AT ROW 19.46 COL 13.75
     BTN_UPPD AT ROW 19.46 COL 26
     FBTN_REG AT ROW 19.46 COL 43.75
     BTN_AVB AT ROW 19.46 COL 58
     "Restidsregistrering för:" VIEW-AS TEXT
          SIZE 37 BY 1.25 AT ROW 1.5 COL 1.5
          FONT 17
     "Obs! Endast restid utanför den ordinarie arbetstiden skall registreras" VIEW-AS TEXT
          SIZE 70.5 BY .67 AT ROW 2.96 COL 1.5
     SPACE(0.74) SKIP(17.15)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tjänsteresor":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db respers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_RESA-2 FILL-IN_FORNAMN-2 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_RESA-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_RESA-2:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_RESA-2:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

ASSIGN 
       BTN_UPPD:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       FBTN_REG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_RESA-2
/* Query rebuild information for BROWSE BRW_RESA-2
     _TblList          = "Temp-Tables.respers"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.respers.DATUM
"respers.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.respers.DAG
"respers.DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.respers.Kilometer
"respers.Kilometer" "Kilo-!meter" ">>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.respers.START
"respers.START" "Start tid" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.respers.SLUT
"respers.SLUT" "Slut tid" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.respers.AONR
"respers.AONR" "Aonr" "X(256)" "character" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.respers.DELNR
"respers.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.respers.BILFORARE
"respers.BILFORARE" "Bil-!förare" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.respers.NATTRAKT
"respers.NATTRAKT" "Natt-!trakt." "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.respers.OVERTIDUTTAG
"respers.OVERTIDUTTAG" "Övertids!uttag" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_RESA-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Tjänsteresor */
DO:
    {BORTBRWPROC.I}
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Tjänsteresor */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_RESA-2
&Scoped-define SELF-NAME BRW_RESA-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_RESA-2 DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_RESA-2 IN FRAME DIALOG-1
DO:

 /*  musz = musz
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.Kilometer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.Kilometer BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.Kilometer IN BROWSE BRW_RESA-2 /* Kilo-!meter */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.START IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.Kilometer BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.Kilometer IN BROWSE BRW_RESA-2 /* Kilo-!meter */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.Kilometer BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.Kilometer IN BROWSE BRW_RESA-2 /* Kilo-!meter */
DO:
   respers.KILOMETER = INPUT BROWSE BRW_RESA-2 respers.KILOMETER.
   IF respers.KILOMETER > 0 THEN DO:
      regdatum = respers.DATUM.
      RUN REGVEC.P.
      {SLUTARBW.I}     
      respers.START = klockan100(respers.START).
      respers.SLUT = klockan100(respers.SLUT).
      IF respers.SLUT LE klockan100(regstart) THEN DO:
         respers.START = respers.SLUT - respers.KILOMETER / 54.
      END.
      ELSE IF respers.START GE klockan100(regslut) THEN DO:
         respers.SLUT = respers.START + respers.KILOMETER / 54.
      END.
      respers.SLUT = klockan60(respers.SLUT).
      respers.START = klockan60(respers.START).   
      IF respers.SLUT > 24 THEN respers.SLUT = 24.
      IF respers.START < 0 THEN respers.START = 0.     
   END.
   RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.START BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.START IN BROWSE BRW_RESA-2 /* Start tid */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.SLUT IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.START BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.START IN BROWSE BRW_RESA-2 /* Start tid */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.START BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.START IN BROWSE BRW_RESA-2 /* Start tid */
DO:
   respers.START = INPUT BROWSE BRW_RESA-2 respers.START.
   DISPLAY respers.START WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.SLUT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.SLUT IN BROWSE BRW_RESA-2 /* Slut tid */
DO:   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.SLUT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.SLUT IN BROWSE BRW_RESA-2 /* Slut tid */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.SLUT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.SLUT IN BROWSE BRW_RESA-2 /* Slut tid */
DO:
   respers.SLUT = INPUT BROWSE BRW_RESA-2 respers.SLUT.
   DISPLAY respers.SLUT WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.AONR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.AONR IN BROWSE BRW_RESA-2 /* Aonr */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.DELNR IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.AONR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.AONR IN BROWSE BRW_RESA-2 /* Aonr */
DO:
  
   RUN visa_UI.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.AONR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.AONR IN BROWSE BRW_RESA-2 /* Aonr */
DO:
   respers.AONR = INPUT BROWSE BRW_RESA-2 respers.AONR.
   DISPLAY respers.AONR WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.DELNR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.DELNR IN BROWSE BRW_RESA-2 /* Delnr */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.KILOMETER IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.DELNR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.DELNR IN BROWSE BRW_RESA-2 /* Delnr */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.DELNR BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.DELNR IN BROWSE BRW_RESA-2 /* Delnr */
DO:
   respers.DELNR = INPUT BROWSE BRW_RESA-2 respers.DELNR.   
   RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.BILFORARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.BILFORARE BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.BILFORARE IN BROWSE BRW_RESA-2 /* Bil-!förare */
DO:
  IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.NATTRAKT IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END. 
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.BILFORARE BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.BILFORARE IN BROWSE BRW_RESA-2 /* Bil-!förare */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.BILFORARE BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.BILFORARE IN BROWSE BRW_RESA-2 /* Bil-!förare */
DO:
   respers.BILFORARE = INPUT BROWSE BRW_RESA-2 respers.BILFORARE.
   DISPLAY respers.BILFORARE WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.BILFORARE BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON MOUSE-SELECT-CLICK OF respers.BILFORARE IN BROWSE BRW_RESA-2 /* Bil-!förare */
DO:
   IF INPUT BROWSE BRW_RESA-2 respers.BILFORARE = TRUE THEN respers.BILFORARE = FALSE.
   IF INPUT respers.BILFORARE = FALSE THEN respers.BILFORARE = TRUE.
   DISPLAY respers.BILFORARE WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.NATTRAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.NATTRAKT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.NATTRAKT IN BROWSE BRW_RESA-2 /* Natt-!trakt. */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO respers.OVERTIDUTTAG IN BROWSE BRW_RESA-2.      
      RETURN NO-APPLY.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.NATTRAKT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.NATTRAKT IN BROWSE BRW_RESA-2 /* Natt-!trakt. */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.NATTRAKT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.NATTRAKT IN BROWSE BRW_RESA-2 /* Natt-!trakt. */
DO:
    respers.NATTRAKT = INPUT BROWSE BRW_RESA-2 respers.NATTRAKT.
   DISPLAY respers.NATTRAKT WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.NATTRAKT BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON MOUSE-SELECT-CLICK OF respers.NATTRAKT IN BROWSE BRW_RESA-2 /* Natt-!trakt. */
DO:
   IF INPUT BROWSE BRW_RESA-2 respers.NATTRAKT = TRUE THEN respers.NATTRAKT = FALSE.
   IF INPUT respers.NATTRAKT = FALSE THEN respers.NATTRAKT = TRUE.
   DISPLAY respers.NATTRAKT WITH BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME respers.OVERTIDUTTAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.OVERTIDUTTAG BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ANY-KEY OF respers.OVERTIDUTTAG IN BROWSE BRW_RESA-2 /* Övertids!uttag */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:         
      GET NEXT BRW_RESA-2 NO-LOCK.
      IF NOT AVAILABLE respers THEN GET FIRST BRW_RESA-2 NO-LOCK.
      IF AVAILABLE respers THEN DO: 
         RUN repo_UI (INPUT RECID(respers)).
         APPLY "ENTRY" TO respers.AONR IN BROWSE BRW_RESA-2.
         RETURN NO-APPLY.
      END.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.OVERTIDUTTAG BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENDKEY OF respers.OVERTIDUTTAG IN BROWSE BRW_RESA-2 /* Övertids!uttag */
DO:
   APPLY "ENTRY" TO respers.AONR IN BROWSE BRW_RESA-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.OVERTIDUTTAG BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON ENTRY OF respers.OVERTIDUTTAG IN BROWSE BRW_RESA-2 /* Övertids!uttag */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL respers.OVERTIDUTTAG BRW_RESA-2 _BROWSE-COLUMN DIALOG-1
ON LEAVE OF respers.OVERTIDUTTAG IN BROWSE BRW_RESA-2 /* Övertids!uttag */
DO:
   IF INPUT BROWSE BRW_RESA-2 respers.OVERTIDUTTAG = "Ö" OR INPUT BROWSE BRW_RESA-2 respers.OVERTIDUTTAG = "K"
   OR INPUT BROWSE BRW_RESA-2 respers.OVERTIDUTTAG = "I"
   THEN DO:
      respers.OVERTIDUTTAG = INPUT BROWSE BRW_RESA-2 respers.OVERTIDUTTAG.
      DISPLAY respers.OVERTIDUTTAG WITH BROWSE BRW_RESA-2.
   END.
   ELSE DO:
      MESSAGE "Måste vara 'Ö' eller 'K'" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
      
   MESSAGE "Vill du registrera dina uppgifter ? - tryck Ja "   SKIP
   "Annars - tryck Nej"    
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         APPLY "CHOOSE"  TO FBTN_REG.                          
      END.
      WHEN FALSE THEN DO:
          musz = TRUE.
         APPLY "GO" TO BTN_AVB.        
      END.
   END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON GO OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYT DIALOG-1
ON CHOOSE OF BTN_BYT IN FRAME DIALOG-1 /* Byt aonr */
DO:

    RUN valdbyt_UI.
   {muswait.i}
   IF musz = FALSE THEN DO:
      ASSIGN 
      varaonr = respers.AONR 
      vardelnr = respers.DELNR. 
      {AVBGOMD.I}
      RUN BYTAAORE.W (INPUT pkod).
      {AVBFRAMD.I}
      IF musz = FALSE THEN DO:
         antal_raknare = 1.
         DO WHILE antal_raknare LE antal_valda :                    
            status-ok = BRW_RESA-2:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.         
            ASSIGN 
            respers.AONR = varaonr 
            respers.DELNR = vardelnr.                            
            antal_raknare = antal_raknare + 1.
         END.
      END.
      avrdatum = brwavdatum.   
      IF musz = FALSE THEN DO:            
         RUN refreshbrw_UI IN brwproc[1].
      END.
   END.
   {musarrow.i}
   musz = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP DIALOG-1
ON CHOOSE OF BTN_UPP IN FRAME DIALOG-1 /* Ändra */
DO:
   RUN valdbyt_UI.
   IF musz = FALSE THEN DO:      
      dummyrec = RECID(respers).
      {muswait.i}
      {AVBGOMD.I}
      RUN RESAND.W (INPUT pkod).
      {AVBFRAMD.I}      
      OPEN QUERY BRW_RESA-2 FOR EACH respers USE-INDEX RESPERS.
      RUN repo_UI (INPUT dummyrec).  
      status-ok = BRW_RESA-2:SELECT-FOCUSED-ROW() NO-ERROR.                      
   END.
   {musarrow.i}
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPPD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPPD DIALOG-1
ON CHOOSE OF BTN_UPPD IN FRAME DIALOG-1 /* Uppdela */
DO:
   status-ok = BRW_RESA-2:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_RESA-2.
   aonrspar = respers.AONR. 
   delnrspar = respers.DELNR. 
   nattspar = respers.NATTRAKT. 
   overspar = respers.OVERTIDUTTAG. 
   prisspar = respers.PRIS. 
   pristypspar = respers.PRISTYP. 
   bilspar = respers.BILFORARE. 
   regdagnamn = respers.DAG. 
   regdatum = respers.DATUM. 
   regstart = respers.SLUT. 
   regvnr = respers.VECKONUMMER.
   dummyrec = RECID(respers).
   {muswait.i}
   CREATE respers.
   ASSIGN 
   respers.AONR = aonrspar
   respers.BILFORARE = bilspar
   respers.DAG = regdagnamn 
   respers.DATUM = regdatum
   respers.DELNR = delnrspar
   respers.NATTRAKT = nattspar
   respers.OVERTIDUTTAG = overspar
   respers.PRIS = prisspar
   respers.PRISTYP = pristypspar
   respers.SLUT = regstart
   respers.START = regstart
   respers.VECKONUMMER = regvnr
   respers.TIDREC = ?.
   OPEN QUERY BRW_RESA-2 FOR EACH respers USE-INDEX RESPERS.
   RUN repo_UI (INPUT dummyrec).                     
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_REG DIALOG-1
ON CHOOSE OF FBTN_REG IN FRAME DIALOG-1 /* Slutför */
DO:
   {muswait.i}  
   RUN klar_UI.
   RUN refreshbrw_UI IN brwproc[1].
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN repo_UI (INPUT RECID(respers)).
      IF feltxt NE "" THEN 
      MESSAGE feltxt VIEW-AS ALERT-BOX.
   END. 
   ELSE APPLY "GO" TO FBTN_REG.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_REG DIALOG-1
ON GO OF FBTN_REG IN FRAME DIALOG-1 /* Slutför */
DO:
   {BORTBRWPROC.I}
   RETURN.
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
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN-PKOD = personaltemp.PERSONALKOD
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   IF Guru.Konstanter:globforetag = "cELPA" THEN DO:
      respers.KILOMETER:VISIBLE IN BROWSE {&BROWSE-NAME} = TRUE.
   END.
   ELSE DO:
      respers.KILOMETER:VISIBLE IN BROWSE {&BROWSE-NAME} = FALSE.
   END.
   utstart = DECIMAL(SUBSTRING(vart,1,5)).
   hemslut = DECIMAL(SUBSTRING(vart,6,5)).      
   RUN nyupp_UI (INPUT 26).
   IF sok2 = 1 THEN DO:        
      ASSIGN respers.OVERTIDUTTAG:VISIBLE IN BROWSE {&BROWSE-NAME} = TRUE
      respers.BILFORARE:VISIBLE IN BROWSE {&BROWSE-NAME} = TRUE.
   END.
   ELSE DO:
      ASSIGN  respers.OVERTIDUTTAG:VISIBLE IN BROWSE {&BROWSE-NAME} = FALSE
      respers.BILFORARE:VISIBLE IN BROWSE {&BROWSE-NAME} = FALSE.   
   END.
   IF enflerdygns = TRUE THEN DO:   
      respers.NATTRAKT:VISIBLE IN BROWSE {&BROWSE-NAME} = FALSE.
      ASSIGN
      sok1 = ansttemp.KOD
      sok3 = "ENBIL".
      RUN nyupp_UI (INPUT 27).      
      IF sok2 = 1 THEN DO:
         ASSIGN
         respers.OVERTIDUTTAG:VISIBLE IN BROWSE {&BROWSE-NAME} = FALSE
         respers.BILFORARE:VISIBLE IN BROWSE {&BROWSE-NAME} = FALSE.                  
      END.
   END.   
   ELSE IF enflerdygns = FALSE THEN DO:
      ASSIGN
      sok1 = ansttemp.KOD
      sok3 = "FLBIL".
      RUN nyupp_UI (INPUT 27).      
      IF sok2 = 1 THEN DO:
         ASSIGN         
         respers.OVERTIDUTTAG:VISIBLE IN BROWSE {&BROWSE-NAME} = FALSE.
         respers.BILFORARE:VISIBLE IN BROWSE {&BROWSE-NAME} = FALSE.         
      END.      
   END.   
   RUN enable_UI.
   
   {FRMSIZED.I}      
   OPEN QUERY BRW_RESA-2 FOR EACH respers USE-INDEX RESPERS.
   ENABLE BRW_RESA-2 WITH FRAME {&FRAME-NAME}.
   status-ok = BRW_RESA-2:SELECT-FOCUSED-ROW() NO-ERROR.      
   respers.AONR:LABEL IN BROWSE BRW_RESA-2 = Guru.Konstanter:gaok.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" THEN respers.NATTRAKT:LABEL IN BROWSE BRW_RESA-2 ="Privat-!logi".
   BTN_BYT:LABEL = "Byt " + LC(Guru.Konstanter:gaok).  
   BTN_UPPD:HIDDEN = TRUE.    /*används inte för närvarande*/
   {musarrow.i}
   {DIA_M_SLUT.I}

   &Scoped-define FORMATNAMN respers.AONR
   &Scoped-define BROWSE-NAME BRW_RESA-2
   {AOFORMAT1.I}
   
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
   (INPUT BRW_RESA-2:HANDLE IN FRAME {&FRAME-NAME}).   
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dubbel_UI DIALOG-1 
PROCEDURE dubbel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   musz = TRUE.
   MESSAGE "Det finns redan en registrering med start "
            sok2 / 100 " och slut"    
            sok5 " den " regdatum "!"  
            VIEW-AS ALERT-BOX.   
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
  DISPLAY FILL-IN-PKOD FILL-IN_FORNAMN-2 
      WITH FRAME DIALOG-1.
  ENABLE BTN_UPP BTN_BYT BTN_UPPD FBTN_REG BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI DIALOG-1 
PROCEDURE klar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   feltxt = "".         
   IF enflerdygns = TRUE THEN DO:     
      IF bdatum = avdatum THEN DO:      
         FIND FIRST respers NO-LOCK NO-ERROR.
         regdatum = respers.DATUM.
         RUN REGVEC.P.
         {SLUTARBW.I}      
         /*kontroll utresans restid */
         FIND FIRST respers WHERE respers.DATUM = bdatum AND respers.START = utstart  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE respers THEN DO:
            IF utstart GE regstart AND utstart LE regslut THEN musz = musz.         
            ELSE DO:         
               feltxt = "Utresa är angiven till: " + STRING(utstart,">9.99") + " Ändra i startbilden om det är fel.". 
               musz = TRUE.
               RETURN.
            END.
         END.
         ELSE DO:
            IF utstart < regstart THEN DO:
               /*ingen restid i två reg tex 0-0  6.3-7*/
               FIND FIRST respersbuff WHERE respersbuff.DATUM = regdatum AND respersbuff.START > utstart AND
               respersbuff.START < regstart AND respersbuff.START NE respersbuff.SLUT NO-LOCK NO-ERROR.
               IF AVAILABLE respersbuff THEN DO:         
                  feltxt = "Utresa är angiven till: " + STRING(utstart,">9.99") + " Ändra i startbilden om det är fel.". 
                  musz = TRUE.
                  RETURN.
               END.
            END.
         END.
         FIND FIRST respers WHERE respers.DATUM = bdatum AND respers.SLUT = hemslut  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE respers THEN DO:
            IF hemslut GE regstart AND hemslut LE regslut THEN musz = musz.         
            ELSE DO:         
               feltxt = "Hemresa är angiven till: " + STRING(hemslut,">9.99") + " Ändra i startbilden om det är fel.". 
               musz = TRUE.
               RETURN.
            END.
         END.
         /* ingen restid efter angivet slut på resan*/
         FIND FIRST respersbuff WHERE respersbuff.DATUM = bdatum AND respersbuff.SLUT > hemslut AND respersbuff.START NE respersbuff.SLUT NO-LOCK NO-ERROR.
         IF AVAILABLE respersbuff THEN DO:         
            feltxt = "Hemresa är angiven till: " + STRING(hemslut,">9.99") + " Ändra i startbilden om det är fel.". 
            musz = TRUE.
            RETURN.         
         END.
      END.
   END.
   IF enflerdygns = FALSE THEN DO:     
      FIND FIRST respers NO-LOCK NO-ERROR.
      regdatum = respers.DATUM.
      RUN REGVEC.P.
      {SLUTARBW.I}      
      /*kontroll utresans restid */
      FIND FIRST respers WHERE respers.DATUM = regdatum AND respers.START = utstart  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE respers THEN DO:                  
         IF utstart GE regstart AND utstart LE regslut THEN musz = musz.         
         ELSE DO:         
            feltxt = "Utresa är angiven till: " + STRING(utstart,">9.99") + " Ändra i startbilden om det är fel.". 
            musz = TRUE.
            RETURN.
         END.
      END.
      ELSE DO:
         IF utstart < regstart THEN DO:
            /*ingen restid i två reg tex 0-0  6.3-7*/
            FIND FIRST respersbuff WHERE respersbuff.DATUM = regdatum AND respersbuff.START > utstart AND
            respersbuff.START < regstart AND respersbuff.START NE respersbuff.SLUT NO-LOCK NO-ERROR.
            IF AVAILABLE respersbuff THEN DO:         
               feltxt = "Utresa är angiven till: " + STRING(utstart,">9.99") + " Ändra i startbilden om det är fel.". 
               musz = TRUE.
               RETURN.
            END.
            FIND FIRST respersbuff WHERE respersbuff.DATUM = regdatum AND respersbuff.START = utstart AND
            respersbuff.START = respersbuff.SLUT NO-LOCK NO-ERROR.
            IF AVAILABLE respersbuff THEN DO:         
               feltxt = "Restid måste registreras utanför ordinarie arbetstid. Starttid kan inte vara samma som sluttid". 
               musz = TRUE.
               RETURN.
            END.
               
         END.
         IF utstart GE regslut  AND respers.START = respers.SLUT THEN DO:
            feltxt = "Utresa är angiven till: " + STRING(utstart,">9.99") + " Du måste även ange när utresan avlutades.". 
            musz = TRUE.
            RETURN.
         END.    
      END.
      /* ingen restid för angiven start på resan*/
      FIND FIRST respersbuff WHERE respersbuff.DATUM = regdatum AND  respersbuff.START < utstart AND respersbuff.START NE respersbuff.SLUT NO-LOCK NO-ERROR.
      IF AVAILABLE respersbuff THEN DO:         
         feltxt = "Utresa är angiven till: " + STRING(utstart,">9.99") + " Ändra i startbilden om det är fel.". 
         musz = TRUE.
         RETURN.         
      END.            
      FIND LAST respers NO-LOCK NO-ERROR.
      regdatum = respers.DATUM.      
      RUN REGVEC.P.
      {SLUTARBW.I}      
      FIND FIRST respers WHERE respers.DATUM = regdatum AND respers.SLUT = hemslut  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE respers THEN DO:
         IF hemslut GE regstart AND hemslut LE regslut THEN musz = musz.         
         ELSE DO:         
            feltxt = "Hemresa är angiven till: " + STRING(hemslut,">9.99") + " Ändra i startbilden om det är fel.". 
            musz = TRUE.
            RETURN.
         END.
      END.
      
      /* ingen restid efter angivet slut på resan*/
      FIND FIRST respersbuff WHERE respersbuff.DATUM = regdatum AND respersbuff.SLUT > hemslut AND respersbuff.START NE respersbuff.SLUT NO-LOCK NO-ERROR.
      IF AVAILABLE respersbuff THEN DO:         
         feltxt = "Hemresa är angiven till: " + STRING(hemslut,">9.99") + " Ändra i startbilden om det är fel.". 
         musz = TRUE.
         RETURN.         
      END.
      
      IF hemslut > regslut THEN DO:
         FIND FIRST respersbuff WHERE respersbuff.DATUM = regdatum AND respersbuff.SLUT = hemslut AND   respersbuff.START = respersbuff.SLUT NO-LOCK NO-ERROR.
         IF AVAILABLE respersbuff THEN DO:         
            feltxt = "Restid måste registreras utanför ordinarie arbetstid. Starttid kan inte vara samma som sluttid". 
            musz = TRUE.
            RETURN.
         END.
      END.      
   END.
   GET FIRST BRW_RESA-2.
   REPEAT:
      IF NOT AVAILABLE respers THEN DO:
         musz = FALSE.
         LEAVE.
      END.   
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = respers.AONR AND 
      utsokaonr.DELNR = respers.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE utsokaonr THEN DO:      
         feltxt = Guru.Konstanter:gaok + " " + respers.AONR + " " + STRING(respers.DELNR,Guru.Konstanter:varforetypchar[1]) + " finns inte.".
         musz = TRUE.
         RETURN.
      END.      
      ELSE DO:
         musz = TRUE.
         feltxt = "".
         regdatum = respers.DATUM.
         RUN REGVEC.P.
         {SLUTARBW.I}
         
         {AOKOLLERS.I}
         musz = FALSE.
         IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
         utsokaonr.AONRAVDATUM >= respers.DATUM THEN musz = musz.
         ELSE DO:
            feltxt = Guru.Konstanter:gaok + " " + respers.AONR + " " + STRING(respers.DELNR,Guru.Konstanter:varforetypchar[1]) + " är redan avslutat.".
            musz = TRUE.
            RETURN.
         END.
      END.
      IF utsokaonr.PRISTYP = "FRÅNVARO." THEN DO:     
         feltxt = "Restid kan inte registreras på frånvaro ".             
         musz = TRUE.
         RETURN.
      END.
      IF respers.START > 24.00 THEN DO:
         feltxt = "Orimligt klockslag.". 
         musz = TRUE.
         RETURN.
      END.      
      IF SUBSTRING(STRING(respers.START,"99.99"),4 ,2) > "59" THEN DO:
         feltxt = "Orimligt klockslag.".    
         musz = TRUE.
         RETURN.
      END.      
      IF respers.SLUT > 24.00 THEN DO:
         feltxt = "Orimligt klockslag.". 
         musz = TRUE.
         RETURN.
      END. 
      IF respers.SLUT < respers.START THEN DO:
         feltxt = "Start kan inte vara större än slut.". 
         musz = TRUE.
         RETURN.
      END. 
      IF SUBSTRING(STRING(respers.SLUT,"99.99"),4 ,2) > "59" THEN DO:
         feltxt = "Orimligt klockslag.".
         musz = TRUE.
         RETURN.      
      END.     
            
      IF respers.START > regstart AND respers.START < regslut AND respers.SLUT NE respers.START THEN DO:
         feltxt = "Restid under arbetstid registreras ej!". 
         musz = TRUE.
         RETURN.
      END. 
      IF respers.SLUT > regstart AND respers.SLUT < regslut AND respers.SLUT NE respers.START THEN DO:
         feltxt = "Restid under arbetstid registreras ej!". 
         musz = TRUE.
         RETURN.
      END. 
      IF respers.KILOMETER > 0 THEN DO:
         regdatum = respers.DATUM.
         RUN REGVEC.P.
         {SLUTARBW.I}
         ASSIGN
         hjstart = regstart
         hjslut = regslut.
         respers.START = klockan100(respers.START).
         respers.SLUT = klockan100(respers.SLUT).
         IF respers.SLUT LE klockan100(regstart) THEN DO:
            respers.START = respers.SLUT - respers.KILOMETER / 54.
         END.
         ELSE IF respers.START GE klockan100(regslut) THEN DO:
            respers.SLUT = respers.START + respers.KILOMETER / 54.
         END.
         respers.SLUT = klockan60(respers.SLUT).
         respers.START = klockan60(respers.START).   
         IF respers.SLUT > 24 THEN respers.SLUT = 24.
         IF respers.START < 0 THEN respers.START = 0.
      END. 
      regdatum = respers.DATUM.
      RUN REGVEC.P.
      {SLUTARBW.I}
      ASSIGN
      hjstart = regstart
      hjslut = regslut.
      IF respers.SLUT NE respers.START THEN DO:      
         ASSIGN
         regdatum = respers.DATUM
         regvnr = respers.VECKO
         regdagnamn = respers.DAG
         regstart = respers.START
         regslut = respers.SLUT.                                  
         IF respers.TIDREC = ? THEN DO:
            musz = FALSE.
            IF enflerdygns = TRUE THEN DO: 
               musz = TRUE.                         
               IF regstart < hjstart THEN DO:               
                  IF regslut > hjstart AND regslut < hjslut THEN  sok2 = hjstart * 100.
                  ELSE sok2 = regslut * 100.
                  ASSIGN
                  sok1 = pkod                  
                  sok4 = STRING(regdatum)
                  sok5 = regstart.                                                      
                  RUN nyupp_UI (INPUT 43).                         
                  IF sok3 = "xxx" THEN DO:
                     musz = FALSE.
                     RETURN.
                  END.
                  ELSE DO:                                     
                     musz = TRUE.
                     feltxt = "".
                     MESSAGE "Det finns redan en registrering med start "
                              sok2 / 100 " och slut " sok5 " den " regdatum "!"  
                              VIEW-AS ALERT-BOX.
                     RETURN.  
                  END.             
               END.            
            END.
            IF respers.START GE hjstart AND respers.SLUT LE hjslut THEN musz = FALSE.
            IF musz = TRUE THEN musz = FALSE.
            ELSE DO:            
               IF varifran NE 3 THEN DO: 
                  ASSIGN
                  sok1 = pkod
                  sok2 = regstart * 100
                  sok4 = STRING(regdatum)
                  sok5 = regslut.                  
                  RUN nyupp_UI (INPUT 34).                  
                  IF sok3 = "xxx" THEN DO:
                     musz = FALSE.
                  END.   
                  ELSE DO:
                     musz = TRUE.
                     MESSAGE "Det finns redan en registrering med start "
                     sok2 / 100 " och slut " sok5 " den " regdatum "!"  
                     VIEW-AS ALERT-BOX.
                     RETURN.
                  END.               
               END.
               ELSE DO: 
                  ASSIGN
                  sok1 = pkod
                  sok3 = "RESTID..." 
                  sok4 = STRING(regdatum)
                  sok5 = regstart.                  
                  RUN nyupp_UI (INPUT 31).                         
                  IF sok3 = "xxx" THEN DO:
                     musz = FALSE.
                     RETURN.
                  END.
                  ELSE DO:                                     
                     musz = TRUE.
                     feltxt = "".
                     MESSAGE "Det finns redan en registrering med start "
                              sok2 / 100 " och slut " sok5 " den " regdatum "!"  
                              VIEW-AS ALERT-BOX.
                     RETURN.  
                  END.
               END.
            END.
         END.
         IF musz = TRUE THEN DO:
            feltxt = "".
            RETURN.         
         END.      
         FIND FIRST respersbuff WHERE 
         respersbuff.DATUM = respers.DATUM AND respersbuff.START LE respers.START AND
         respersbuff.SLUT > respers.START AND RECID(respersbuff) NE RECID(respers)
         NO-LOCK NO-ERROR.
         IF AVAILABLE respersbuff THEN DO:                             
            musz = TRUE.
            feltxt = "Det finns redan en registrering med start " + STRING(respers.START,"99.99") + " och slut " + STRING(respers.SLUT,"99.99") + "!". 
            RETURN.
         END.       
         ASSIGN
         regdatum = respers.DATUM
         regstart = respers.START
         regslut = respers.SLUT.                                                    
         IF respers.TIDREC = ? THEN DO:
            musz = FALSE.
            IF enflerdygns = TRUE THEN DO:
               musz = TRUE.   
               IF regslut > hjslut THEN DO:          
                  IF varifran NE 3 THEN DO: 
                     ASSIGN
                     sok1 = pkod
                     sok2 = regstart * 100
                     sok4 = STRING(regdatum)
                     sok5 = regslut.                  
                     RUN nyupp_UI (INPUT 41).                  
                     IF sok3 = "xxx" THEN DO:
                        musz = FALSE.
                     END.   
                     ELSE DO:
                        musz = TRUE.
                        MESSAGE "Det finns redan en registrering med start "
                        sok2 / 100 " och slut " sok5 " den " regdatum "!"  
                        VIEW-AS ALERT-BOX.
                        RETURN.
                     END.
                  END.                                   
                  IF regstart > hjstart AND regstart < hjslut THEN  sok2 = hjslut * 100.
                  ELSE sok2 = regstart * 100.
                  ASSIGN
                  sok1 = pkod                  
                  sok4 = STRING(regdatum)
                  sok5 = regslut.                  
                  RUN nyupp_UI (INPUT 42).                         
                  IF sok3 = "xxx" THEN DO:
                     musz = FALSE.
                     RETURN.
                  END.
                  ELSE DO:                                     
                     musz = TRUE.
                     feltxt = "".
                     MESSAGE "Det finns redan en registrering med start "
                              sok2 / 100 " och slut " sok5 " den " regdatum "!"  
                              VIEW-AS ALERT-BOX.
                     RETURN.  
                  
                  
                  END.                               
               END.
            END.
            IF respers.START GE hjstart AND respers.SLUT LE hjslut THEN musz = FALSE.
            IF musz = TRUE THEN musz = FALSE.
            ELSE DO:            
               IF varifran NE 3 THEN DO: 
                  ASSIGN
                  sok1 = pkod
                  sok2 = regstart * 100
                  sok4 = STRING(regdatum)
                  sok5 = regslut.                  
                  RUN nyupp_UI (INPUT 41).                  
                  IF sok3 = "xxx" THEN DO:
                     musz = FALSE.
                  END.   
                  ELSE DO:
                     musz = TRUE.
                     MESSAGE "Det finns redan en registrering med start "
                     sok2 / 100 " och slut " sok5 " den " regdatum "!"  
                     VIEW-AS ALERT-BOX.
                     RETURN.
                  END.               
               END.
               ELSE DO:                       
                  IF musz = FALSE THEN DO:                /*EJ DYGNSBRYT*/
                     ASSIGN
                     sok1 = pkod
                     sok2 = regstart * 100
                     sok3 = "RESTID..." 
                     sok4 = STRING(regdatum)
                     sok5 = regslut
                     musz = FALSE.                     
                     RUN nyupp_UI (INPUT 32).                     
                     IF sok3 = "xxx" THEN DO:
                        musz = FALSE.
                        RETURN.   
                     END.  
                     ELSE DO:
                        RUN dubbel_UI.
                        RETURN. 
                     END.   
                  END.   
                  ELSE DO:
                     ASSIGN
                     sok1 = pkod
                     sok2 = regstart * 100
                     sok3 = "RESTID..." 
                     sok4 = STRING(regdatum)
                     sok5 = regslut
                     musz = FALSE.                     
                     RUN nyupp_UI (INPUT 33).                     
                     IF sok3 = "xxx" THEN DO:
                        musz = FALSE.      
                        RETURN.
                     END.
                     ELSE DO:
                        RUN dubbel_UI.
                        RETURN.                   
                     END.                    
                  END.
               END.
            END.
         END.
         IF musz = TRUE THEN DO:
            feltxt = "".
            RETURN.
         END.
         FIND FIRST respersbuff WHERE 
         respersbuff.DATUM = respers.DATUM AND respersbuff.START < respers.SLUT AND
         respersbuff.SLUT > respers.SLUT AND RECID(respersbuff) NE RECID(respers)
         NO-ERROR.
         IF AVAILABLE respersbuff THEN DO:
            musz = TRUE.
            feltxt = "Det finns redan en registrering med start " + STRING(respers.START,"99.99") + " och slut " + STRING(respers.SLUT,"99.99") + "!".           
            RETURN.      
         END.      
      END.   
      GET NEXT BRW_RESA-2.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyupp_UI DIALOG-1 
PROCEDURE nyupp_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER sok0 AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT sok0,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT sok0,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI DIALOG-1 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.   
   BRW_RESA-2:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION BRW_RESA-2 TO RECID browrec.  
   status-ok = BRW_RESA-2:SELECT-FOCUSED-ROW() NO-ERROR.                   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdbyt_UI DIALOG-1 
PROCEDURE valdbyt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   antal_valda = BRW_RESA-2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.      
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen registrering är markerad." VIEW-AS ALERT-BOX.    
      musz = TRUE.
      RETURN.                
   END.
   ELSE DO:  
      antal_raknare = 1.
      avrdatum = brwbdatum.
      DO WHILE antal_raknare LE antal_valda :                  
         status-ok = BRW_RESA-2:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.                  
         IF avrdatum < respers.DATUM THEN avrdatum = respers.DATUM.
         antal_raknare = antal_raknare + 1.
      END.          
  END.             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI DIALOG-1 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   &Scoped-define FORMATNAMN respers.AONR
   &Scoped-define BROWSE-NAME BRW_RESA-2
   {&FORMATNAMN}:FORMAT IN BROWSE {&BROWSE-NAME} =  "X(" + STRING(Guru.Konstanter:varforetypval[8]) + ")".
   DISPLAY
   respers.AONR 
   respers.BILFORARE 
   respers.DELNR respers.Kilometer respers.SLUT respers.START   
   respers.NATTRAKT respers.OVERTIDUTTAG WITH BROWSE BRW_RESA-2.      
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

