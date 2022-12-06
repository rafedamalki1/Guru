&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3


/* Temp-Table and Buffer definitions                                    */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:befprislisttemp.BEFATTNING = KALKYLPRISER.BENAMNING
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
{GLOBVAR2DEL1.I}
DEFINE INPUT-OUTPUT PARAMETER defprisid AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
DEFINE SHARED VARIABLE defprisproch AS HANDLE NO-UNDO. /* Procedure handle, DEFPRISAPP.P */
DEFINE SHARED VARIABLE laddaproch AS HANDLE NO-UNDO. /* DYNLADDATEMP.P */
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE tempprisid AS CHARACTER NO-UNDO.
DEFINE VARIABLE kvall  AS DECIMAL FORMAT "99.99" INITIAL 16.00 NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
{SOKDEF.I}

DEFINE NEW SHARED TEMP-TABLE befprislisttemp NO-UNDO 
   FIELD NAMN        AS CHARACTER FORMAT "x(20)" LABEL "Befattning"
   FIELD PRISID      AS CHARACTER FORMAT "X(8)"
   FIELD BEFATTNING  AS CHARACTER FORMAT "x(20)"
   FIELD PRISA       AS DECIMAL FORMAT "->>>>9.99"
   FIELD PRISRES     AS DECIMAL FORMAT "->>>>9.99"
   INDEX PRISID AS PRIMARY PRISID BEFATTNING.
DEFINE TEMP-TABLE ebefprislisttemp NO-UNDO LIKE befprislisttemp.
DEFINE NEW SHARED TEMP-TABLE otextpristemp NO-UNDO 
   FIELD PRISID      AS CHARACTER FORMAT "X(8)"
   FIELD START       AS DECIMAL FORMAT "99.99" INITIAL 7.00
   FIELD SLUT        AS DECIMAL FORMAT "99.99" INITIAL 16.00
   FIELD BEFATTNING  AS CHARACTER FORMAT "x(20)"
   FIELD EQDAG       AS INTEGER FORMAT "9"
   FIELD PRISA       AS DECIMAL FORMAT "->>>>9.99"
   FIELD DAGTYP      AS CHARACTER FORMAT "X(8)"
   FIELD OTEXT       AS CHARACTER FORMAT "X(25)"
   FIELD OTEXTID     AS CHARACTER FORMAT "X(8)"
   INDEX PRISID AS PRIMARY PRISID BEFATTNING EQDAG START SLUT.
DEFINE TEMP-TABLE eotextpristemp NO-UNDO LIKE otextpristemp.
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED
{BEFTEMP.I}
{OVERTEXTEMP.I}
DEFINE NEW SHARED TEMP-TABLE extraopltemp NO-UNDO LIKE overprislisttemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define BROWSE-NAME BRW_BEF

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES befprislisttemp otextpristemp

/* Definitions for BROWSE BRW_BEF                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BEF befprislisttemp.NAMN ~
befprislisttemp.PRISA befprislisttemp.PRISRES 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEF befprislisttemp.PRISA ~
befprislisttemp.PRISRES 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BEF befprislisttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BEF befprislisttemp
&Scoped-define QUERY-STRING-BRW_BEF FOR EACH befprislisttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BEF OPEN QUERY BRW_BEF FOR EACH befprislisttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BEF befprislisttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEF befprislisttemp


/* Definitions for BROWSE BRW_KUND                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KUND otextpristemp.OTEXT ~
otextpristemp.DAGTYP otextpristemp.START otextpristemp.SLUT ~
otextpristemp.PRISA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KUND otextpristemp.OTEXT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KUND otextpristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KUND otextpristemp
&Scoped-define QUERY-STRING-BRW_KUND FOR EACH otextpristemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KUND OPEN QUERY BRW_KUND FOR EACH otextpristemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KUND otextpristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KUND otextpristemp


/* Definitions for DIALOG-BOX DIALOG-3                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-PRIS FILL-IN-TEXT BTN_NVE ~
FILL-IN-START FILL-IN-LIST TOG_SKAP BTN_FVE FILL-IN_MIL FILL-IN_MTRLPA ~
FILL-IN_ENTRAK FILL-IN_FRTJPA FILL-IN_FLERTRAK BRW_KUND BRW_BEF FBTN_OK ~
BTN_NY BTN_AND BTN_KOP BTN_BORT BTN_KOP-2 BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PRIS FILL-IN-TEXT FILL-IN-START ~
FILL-IN-LIST TOG_SKAP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_KOP 
     LABEL "Kopiera priser" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KOP-2 
     LABEL "Kopiera":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-LIST AS LOGICAL FORMAT "Övertid/Normaltid":U INITIAL NO 
     LABEL "Listtyp" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Prislista" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-START AS DATE FORMAT "99/99/99":U 
     LABEL "Startdatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(20)":U 
     LABEL "Text" 
     VIEW-AS FILL-IN 
     SIZE 42.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ENTRAK AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Endagsförrättning i kr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_FLERTRAK AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Flerdygnsförrättning i kr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_FRTJPA AS DECIMAL FORMAT "->>9" INITIAL 0 
     LABEL "Fr. tjänst i %" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.

DEFINE VARIABLE FILL-IN_MIL AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Milersättning kr/mil" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_MTRLPA AS DECIMAL FORMAT "->>9" INITIAL 0 
     LABEL "Materiel i %" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.

DEFINE VARIABLE TOG_SKAP AS LOGICAL INITIAL no 
     LABEL "Skapa prislista" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BEF FOR 
      befprislisttemp SCROLLING.

DEFINE QUERY BRW_KUND FOR 
      otextpristemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEF DIALOG-3 _STRUCTURED
  QUERY BRW_BEF NO-LOCK DISPLAY
      befprislisttemp.NAMN FORMAT "x(40)":U
      befprislisttemp.PRISA COLUMN-LABEL "Tim pris" FORMAT ">>>>9.99":U
      befprislisttemp.PRISRES COLUMN-LABEL "Res pris" FORMAT ">>>>9.99":U
  ENABLE
      befprislisttemp.PRISA
      befprislisttemp.PRISRES
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52 BY 9.63.

DEFINE BROWSE BRW_KUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KUND DIALOG-3 _STRUCTURED
  QUERY BRW_KUND NO-LOCK DISPLAY
      otextpristemp.OTEXT COLUMN-LABEL "Övertidstext" FORMAT "X(25)":U
      otextpristemp.DAGTYP COLUMN-LABEL "Dagtyp" FORMAT "X(8)":U
            WIDTH 10
      otextpristemp.START COLUMN-LABEL "Start tid" FORMAT "99.99":U
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      otextpristemp.SLUT COLUMN-LABEL "Slut tid" FORMAT "99.99":U
            COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
      otextpristemp.PRISA COLUMN-LABEL "Pris" FORMAT ">>>>9.99":U
  ENABLE
      otextpristemp.OTEXT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 65.63 BY 9.58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     FILL-IN-PRIS AT ROW 1.5 COL 13.75 COLON-ALIGNED
     FILL-IN-TEXT AT ROW 1.5 COL 37.38 COLON-ALIGNED
     BTN_NVE AT ROW 2.5 COL 25.88
     FILL-IN-START AT ROW 2.75 COL 13.63 COLON-ALIGNED
     FILL-IN-LIST AT ROW 2.75 COL 37.38 COLON-ALIGNED
     TOG_SKAP AT ROW 2.83 COL 68.13 RIGHT-ALIGNED
     BTN_FVE AT ROW 3.33 COL 25.88
     FILL-IN_MIL AT ROW 4.58 COL 27.88 COLON-ALIGNED
     FILL-IN_MTRLPA AT ROW 4.58 COL 59.13 COLON-ALIGNED
     FILL-IN_ENTRAK AT ROW 5.75 COL 27.88 COLON-ALIGNED
     FILL-IN_FRTJPA AT ROW 5.75 COL 59.13 COLON-ALIGNED
     FILL-IN_FLERTRAK AT ROW 6.88 COL 27.88 COLON-ALIGNED
     BRW_KUND AT ROW 8.25 COL 1.5
     BRW_BEF AT ROW 8.25 COL 8.38
     FBTN_OK AT ROW 16.96 COL 67.88
     BTN_NY AT ROW 18.04 COL 9
     BTN_AND AT ROW 18.04 COL 22.13
     BTN_KOP AT ROW 18.04 COL 25.75
     BTN_BORT AT ROW 18.04 COL 35.25
     BTN_KOP-2 AT ROW 18.04 COL 48.5
     BTN_AVBRYT AT ROW 18.04 COL 67.88
     SPACE(0.24) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Prislistor":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: befprislisttemp T "?" NO-UNDO temp-db befprislisttemp
      TABLE: otextpristemp T "?" NO-UNDO temp-db otextpristemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_KUND FILL-IN_FLERTRAK DIALOG-3 */
/* BROWSE-TAB BRW_BEF BRW_KUND DIALOG-3 */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

ASSIGN 
       BRW_BEF:HIDDEN  IN FRAME DIALOG-3                = TRUE.

ASSIGN 
       BRW_KUND:HIDDEN  IN FRAME DIALOG-3                = TRUE.

ASSIGN 
       BTN_AND:HIDDEN IN FRAME DIALOG-3           = TRUE.

ASSIGN 
       BTN_BORT:HIDDEN IN FRAME DIALOG-3           = TRUE.

ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DIALOG-3           = TRUE.

ASSIGN 
       BTN_KOP:HIDDEN IN FRAME DIALOG-3           = TRUE.

ASSIGN 
       BTN_KOP-2:HIDDEN IN FRAME DIALOG-3           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DIALOG-3           = TRUE.

ASSIGN 
       BTN_NY:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ENTRAK IN FRAME DIALOG-3
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN_ENTRAK:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FLERTRAK IN FRAME DIALOG-3
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN_FLERTRAK:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FRTJPA IN FRAME DIALOG-3
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN_FRTJPA:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MIL IN FRAME DIALOG-3
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN_MIL:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MTRLPA IN FRAME DIALOG-3
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN_MTRLPA:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_SKAP IN FRAME DIALOG-3
   ALIGN-R                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEF
/* Query rebuild information for BROWSE BRW_BEF
     _TblList          = "Temp-Tables.befprislisttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.befprislisttemp.NAMN
"befprislisttemp.NAMN" ? "x(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.befprislisttemp.PRISA
"befprislisttemp.PRISA" "Tim pris" ">>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.befprislisttemp.PRISRES
"befprislisttemp.PRISRES" "Res pris" ">>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KUND
/* Query rebuild information for BROWSE BRW_KUND
     _TblList          = "Temp-Tables.otextpristemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.otextpristemp.OTEXT
"otextpristemp.OTEXT" "Övertidstext" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.otextpristemp.DAGTYP
"otextpristemp.DAGTYP" "Dagtyp" ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.otextpristemp.START
"otextpristemp.START" "Start tid" ? "decimal" ? 1 ? ? 1 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.otextpristemp.SLUT
"otextpristemp.SLUT" "Slut tid" ? "decimal" ? 3 ? ? 3 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.otextpristemp.PRISA
"otextpristemp.PRISA" "Pris" ">>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KUND */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Prislistor */
DO:
   RUN avb_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Prislistor */
DO:    
   RUN avb_UI.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&Scoped-define SELF-NAME BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEF DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF BRW_BEF IN FRAME DIALOG-3
DO:
  /* RUN andrapris_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KUND
&Scoped-define SELF-NAME BRW_KUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KUND DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF BRW_KUND IN FRAME DIALOG-3
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND DIALOG-3
ON CHOOSE OF BTN_AND IN FRAME DIALOG-3 /* Ändra */
DO:
  RUN andra_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* Avbryt */
DO:
   RUN avb_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-3
ON CHOOSE OF BTN_BORT IN FRAME DIALOG-3 /* Ta bort */
DO:
  RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE DIALOG-3
ON CHOOSE OF BTN_FVE IN FRAME DIALOG-3 /* - */
DO: 
   ASSIGN
   FILL-IN-START = INPUT FILL-IN-START.   
   FILL-IN-START = FILL-IN-START - 1.      
   DISPLAY FILL-IN-START WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOP DIALOG-3
ON CHOOSE OF BTN_KOP IN FRAME DIALOG-3 /* Kopiera priser */
DO:
   RUN kop_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOP-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOP-2 DIALOG-3
ON CHOOSE OF BTN_KOP-2 IN FRAME DIALOG-3 /* Kopiera */
DO:
   RUN kop2_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE DIALOG-3
ON CHOOSE OF BTN_NVE IN FRAME DIALOG-3 /* + */
DO:   
   ASSIGN
   FILL-IN-START = INPUT FILL-IN-START.   
   FILL-IN-START = FILL-IN-START + 1.        
   DISPLAY FILL-IN-START WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-3
ON CHOOSE OF BTN_NY IN FRAME DIALOG-3 /* Ny */
DO:
  RUN ny_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK DIALOG-3
ON CHOOSE OF FBTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}    
  
   musz = FALSE.
   IF defprisid = "" AND TOG_SKAP = FALSE THEN DO:
      MESSAGE "Du måste skapa en prislista." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FILL-IN-TEXT = INPUT FILL-IN-TEXT.
   IF FILL-IN-TEXT = "" THEN DO:
      MESSAGE "Texten får ej vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF Guru.Konstanter:globforetag = "cAKEA" THEN DO:
      FIND FIRST defpristemp WHERE defpristemp.PRISID = defprisid NO-LOCK NO-ERROR.
      ASSIGN  
      defpristemp.OVER = INPUT FILL-IN-LIST
      defpristemp.PRISNUM = INTEGER(defprisid)
      defpristemp.STARTDATUM = INPUT FILL-IN-START
      defpristemp.PRISTEXT = INPUT FILL-IN-TEXT.
      RUN KalPrisSpar_UI IN defprisproch
      (INPUT defprisid, INPUT TABLE defpristemp, INPUT TABLE befprislisttemp).
      
   END.
   ELSE DO:   
      FIND FIRST defpristemp WHERE defpristemp.PRISID = defprisid NO-LOCK NO-ERROR.
      ASSIGN 
      defpristemp.OVER = INPUT FILL-IN-LIST
      defpristemp.PRISNUM = INPUT FILL-IN-PRIS
      defpristemp.STARTDATUM = INPUT FILL-IN-START
      defpristemp.PRISTEXT = INPUT FILL-IN-TEXT.
      IF defpristemp.OVER = FALSE THEN DO:   
         FIND FIRST prislotemp WHERE prislotemp.PRISID = defprisid
         EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE prislotemp THEN DO:
            CREATE prislotemp.
            prislotemp.PRISID = defprisid.
         END.
         ASSIGN
         prislotemp.ENTRAK = INPUT FILL-IN_ENTRAK
         prislotemp.FLERTRAK = INPUT FILL-IN_FLERTRAK
         prislotemp.FRTJPA = INPUT FILL-IN_FRTJPA
         prislotemp.MIL = INPUT FILL-IN_MIL
         prislotemp.MTRLPA = INPUT FILL-IN_MTRLPA.
      END.
      IF defpristemp.OVER = TRUE THEN DO:
         musz = FALSE.
         koll1:
         DO:
            FOR EACH eotextpristemp,
            EACH otextpristemp WHERE otextpristemp.BEFATTNING = eotextpristemp.BEFATTNING AND 
            otextpristemp.DAGTYP = eotextpristemp.DAGTYP:
               IF otextpristemp.START NE eotextpristemp.START THEN musz = TRUE.
               IF otextpristemp.SLUT NE eotextpristemp.SLUT THEN musz = TRUE.
               IF otextpristemp.PRISA NE eotextpristemp.PRISA THEN musz = TRUE.
               IF musz = TRUE THEN LEAVE koll1.
            END.
         END.      
      END.
      ELSE DO:
         koll2:
         DO:
            FOR EACH ebefprislisttemp,
            EACH befprislisttemp WHERE befprislisttemp.BEFATTNING = ebefprislisttemp.BEFATTNING:         
               IF befprislisttemp.PRISRES NE ebefprislisttemp.PRISRES THEN musz = TRUE.
               IF befprislisttemp.PRISA NE ebefprislisttemp.PRISA THEN musz = TRUE.
               IF musz = TRUE THEN LEAVE koll2.
            END.
         END.
      END.
      IF musz = TRUE THEN DO:
         IF defpristemp.STARTDATUM < TODAY THEN defpristemp.STARTDATUM = TODAY. 
      END.
   
      RUN btnokdprisny_UI IN defprisproch
      (INPUT defprisid, INPUT TABLE defpristemp, INPUT TABLE prislotemp, INPUT TABLE befprislisttemp).
   END.
   APPLY "GO" TO FRAME {&FRAME-NAME}.             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LIST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LIST DIALOG-3
ON MOUSE-SELECT-CLICK OF FILL-IN-LIST IN FRAME DIALOG-3 /* Listtyp */
DO:
   IF INPUT FILL-IN-LIST = FALSE THEN ASSIGN FILL-IN-LIST = TRUE.
   IF INPUT FILL-IN-LIST = TRUE THEN ASSIGN FILL-IN-LIST = FALSE.
   DISPLAY FILL-IN-LIST  WITH FRAME {&FRAME-NAME}.     
   MESSAGE "Är du säker på att du vill skapa en ny prislista av typen " STRING(FILL-IN-LIST,"Övertid/Normaltid")  " ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE FILL-IN-TEXT
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:
      TOG_SKAP = TRUE.
      RUN nylista_UI.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRIS DIALOG-3
ON LEAVE OF FILL-IN-PRIS IN FRAME DIALOG-3 /* Prislista */
DO:
   FILL-IN-PRIS = INPUT FILL-IN-PRIS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-START DIALOG-3
ON LEAVE OF FILL-IN-START IN FRAME DIALOG-3 /* Startdatum */
DO:
   FILL-IN-START = INPUT FILL-IN-START.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-START DIALOG-3
ON MOUSE-MENU-CLICK OF FILL-IN-START IN FRAME DIALOG-3 /* Startdatum */
DO:
   ASSIGN 
   FILL-IN-START = INPUT FILL-IN-START
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-START.
   RUN AlmanBtn.w.
   FILL-IN-START = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-START WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TEXT DIALOG-3
ON LEAVE OF FILL-IN-TEXT IN FRAME DIALOG-3 /* Text */
DO:
  FILL-IN-TEXT = INPUT FILL-IN-TEXT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_SKAP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_SKAP DIALOG-3
ON VALUE-CHANGED OF TOG_SKAP IN FRAME DIALOG-3 /* Skapa prislista */
DO:
   TOG_SKAP = INPUT TOG_SKAP.
   FILL-IN-LIST = INPUT FILL-IN-LIST.
   IF Guru.Konstanter:globforetag = "cAKEA" THEN DO:
      MESSAGE "Är du säker på att du vill skakapa en ny prislista av typen " STRING(FILL-IN-LIST,"Allmänt/Kalkyl")  " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE FILL-IN-TEXT UPDATE answer AS LOGICAL.
   END.
   ELSE DO: 
      MESSAGE "Är du säker på att du vill skakapa en ny prislista av typen " STRING(FILL-IN-LIST,"Övertid/Normaltid")  " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE FILL-IN-TEXT UPDATE answer .
   END.
   IF answer THEN RUN nylista_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
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
   tempprisid = defprisid.
   RUN enable_UI. 
    
   {FRMSIZED.I}
   ASSIGN
   BRW_KUND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE            
   BRW_BEF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_KOP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_AND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE            
   BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_KOP-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_ENTRAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_FLERTRAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_FRTJPA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_MIL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_MTRLPA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_SKAP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
  
   IF defprisid NE "" THEN DO:
      FIND FIRST defpristemp WHERE defpristemp.PRISID = defprisid NO-LOCK NO-ERROR.
      IF AVAILABLE defpristemp THEN DO:
         ASSIGN
         FILL-IN-LIST = defpristemp.OVER
         FILL-IN-PRIS = defpristemp.PRISNUM
         FILL-IN-START = defpristemp.STARTDATUM
         FILL-IN-TEXT = defpristemp.PRISTEXT.        
         IF defpristemp.OVER = TRUE THEN DO:
            RUN openbdynspec_UI IN brwproc[2].
            ASSIGN
            BRW_KUND:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            BTN_AND:HIDDEN IN FRAME {&FRAME-NAME} = FALSE            
            BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            BTN_KOP-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.
         ELSE DO:
            RUN openbdynspec_UI IN brwproc[1].
            ASSIGN
            BRW_BEF:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            BTN_KOP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            FILL-IN_ENTRAK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
            FILL-IN_FLERTRAK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            FILL-IN_FRTJPA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            FILL-IN_MIL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            FILL-IN_MTRLPA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            FIND FIRST prislotemp WHERE prislotemp.PRISID = defpristemp.PRISID NO-LOCK NO-ERROR.
            IF NOT AVAILABLE prislotemp THEN DO TRANSACTION:
               CREATE prislotemp.
               prislotemp.PRISID = defpristemp.PRISID.
            END.
            ASSIGN
            FILL-IN_ENTRAK = prislotemp.ENTRAK
            FILL-IN_FLERTRAK = prislotemp.FLERTRAK
            FILL-IN_FRTJPA = prislotemp.FRTJPA
            FILL-IN_MIL = prislotemp.MIL
            FILL-IN_MTRLPA = prislotemp.MTRLPA.
            DISPLAY FILL-IN_ENTRAK FILL-IN_FLERTRAK FILL-IN_FRTJPA FILL-IN_MIL
            FILL-IN_MTRLPA WITH FRAME {&FRAME-NAME}.
         END.
         DISABLE FILL-IN-LIST WITH FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE DO:
      FILL-IN-START = TODAY.
      ENABLE FILL-IN-LIST WITH FRAME {&FRAME-NAME}.
      TOG_SKAP:HIDDEN = FALSE.
   END.
   DISPLAY FILL-IN-LIST FILL-IN-PRIS FILL-IN-START FILL-IN-TEXT WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "cAKEA" THEN DO:
      FILL-IN-START:HIDDEN = TRUE.
      BTN_NVE:HIDDEN = TRUE.
      BTN_FVE:HIDDEN = TRUE.
      FILL-IN_MIL:HIDDEN = TRUE.
      FILL-IN_ENTRAK:HIDDEN = TRUE.
      FILL-IN_FLERTRAK:HIDDEN = TRUE.
      FILL-IN_MTRLPA:HIDDEN = TRUE.
      FILL-IN_FRTJPA:HIDDEN = TRUE.
      FILL-IN-PRIS:HIDDEN = TRUE.
      befprislisttemp.PRISA:LABEL IN BROWSE BRW_BEF  = "Tim pris" .
      befprislisttemp.PRISRES:LABEL IN BROWSE BRW_BEF  = "Faktor" .
      FILL-IN-LIST = FALSE.
      DISPLAY  FILL-IN-LIST FORMAT "Allmänt/Kalkyl" WITH FRAME DIALOG-3 .
      DISABLE  FILL-IN-LIST  WITH FRAME DIALOG-3 . 
   END.   
      
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
   otextpristemp.OTEXT:READ-ONLY IN BROWSE BRW_KUND = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_BEF:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_KUND:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN laddajointables_UI IN defprisproch
      (INPUT defprisid, OUTPUT TABLE befprislisttemp, OUTPUT TABLE otextpristemp).
   FOR EACH otextpristemp:
      CREATE eotextpristemp.
      BUFFER-COPY otextpristemp TO ebefprislisttemp.
   END.
   FOR EACH befprislisttemp:
      CREATE ebefprislisttemp.
      BUFFER-COPY befprislisttemp TO  ebefprislisttemp.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI DIALOG-3 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   BRW_KUND:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE otextpristemp THEN DO:
      FIND FIRST extraopltemp NO-LOCK NO-ERROR.
      IF AVAILABLE extraopltemp THEN DELETE extraopltemp.
      CREATE extraopltemp.
      BUFFER-COPY otextpristemp TO extraopltemp.
      musz = FALSE.
      RUN NYDEFPDO.W.    
      IF musz = FALSE THEN DO:       
         FIND FIRST extraopltemp NO-LOCK NO-ERROR.
         IF AVAILABLE extraopltemp THEN DO:
            RUN laddajointables_UI IN defprisproch
               (INPUT extraopltemp.PRISID, OUTPUT TABLE befprislisttemp, OUTPUT TABLE otextpristemp).
            RUN openbdynspec_UI IN brwproc[2].
            FIND FIRST otextpristemp WHERE otextpristemp.EQDAG = extraopltemp.EQDAG AND
               otextpristemp.START = extraopltemp.START AND otextpristemp.SLUT = extraopltemp.SLUT AND 
               otextpristemp.BEFATTNING = extraopltemp.BEFATTNING NO-LOCK NO-ERROR.
            IF AVAILABLE otextpristemp THEN DO:
               RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(otextpristemp)).
               RUN lastselectdyn_UI IN brwproc[2].
            END.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI DIALOG-3 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   musz = TRUE.
   IF tempprisid = "" THEN DO:
      RUN btnavbprisny_UI IN defprisproch (INPUT defprisid).      
   END.
   {BORTBRWPROC.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI DIALOG-3 
PROCEDURE bort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   BRW_KUND:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   MESSAGE "Vill du ta bort denna post ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE defpristemp.PRISTEXT
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:
      IF BRW_KUND:NUM-ITERATIONS = 1 THEN DO:
         MESSAGE "Det måste finnas minst en post kvar. Sätt start och slut tid lika." 
         VIEW-AS ALERT-BOX.                       
      END.
      ELSE DO:
         {muswait.i}   
         FIND FIRST extraopltemp NO-LOCK NO-ERROR.
         IF AVAILABLE extraopltemp THEN DELETE extraopltemp.
         CREATE extraopltemp.          
         BUFFER-COPY otextpristemp TO extraopltemp.          
         DELETE otextpristemp.
         RUN selnextprevrow_UI IN brwproc[2].
         RUN openbdynspec_UI IN brwproc[2].
         RUN lastselectdyn_UI IN brwproc[2].
         RUN tabortnydef_UI IN defprisproch (INPUT TABLE extraopltemp).
      END.       
      {musarrow.i}
   END.
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
  DISPLAY FILL-IN-PRIS FILL-IN-TEXT FILL-IN-START FILL-IN-LIST TOG_SKAP 
      WITH FRAME DIALOG-3.
  ENABLE FILL-IN-PRIS FILL-IN-TEXT BTN_NVE FILL-IN-START FILL-IN-LIST TOG_SKAP 
         BTN_FVE FILL-IN_MIL FILL-IN_MTRLPA FILL-IN_ENTRAK FILL-IN_FRTJPA 
         FILL-IN_FLERTRAK BRW_KUND BRW_BEF FBTN_OK BTN_NY BTN_AND BTN_KOP 
         BTN_BORT BTN_KOP-2 BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kop2_UI DIALOG-3 
PROCEDURE kop2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   musz = TRUE.
   RUN DEFKUNDO.W (INPUT defprisid).
   IF musz = FALSE THEN DO:     
      RUN openbdynspec_UI IN brwproc[2].
   END.
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kop_UI DIALOG-3 
PROCEDURE kop_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   musz = TRUE.
   RUN DEFKOPKP.W (INPUT defprisid).
   IF musz = FALSE THEN DO:
      /*Denna kodsnutt kan förbättras genom att returnera parametrar*/
      tthandle = TEMP-TABLE prislotemp:HANDLE.
      RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "PRISLISTOVRIGT", INPUT "").
      FIND FIRST prislotemp WHERE prislotemp.PRISID = defprisid NO-LOCK NO-ERROR.
      IF NOT AVAILABLE prislotemp THEN DO TRANSACTION:
         CREATE prislotemp.
         prislotemp.PRISID = defpristemp.PRISID.
      END.
      ASSIGN
      FILL-IN_ENTRAK = prislotemp.ENTRAK
      FILL-IN_FLERTRAK = prislotemp.FLERTRAK
      FILL-IN_FRTJPA = prislotemp.FRTJPA
      FILL-IN_MIL = prislotemp.MIL
      FILL-IN_MTRLPA = prislotemp.MTRLPA.   
      RUN laddabefprislisttemp_UI IN defprisproch
      (INPUT defprisid, OUTPUT TABLE befprislisttemp).
      RUN openbdynspec_UI IN brwproc[1].
      DISPLAY FILL-IN_ENTRAK FILL-IN_FLERTRAK FILL-IN_FRTJPA FILL-IN_MIL FILL-IN_MTRLPA WITH FRAME {&FRAME-NAME}.
   END.
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nylista_UI DIALOG-3 
PROCEDURE nylista_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   FILL-IN-LIST = INPUT FRAME {&FRAME-NAME} FILL-IN-LIST.
   DISABLE FILL-IN-LIST WITH FRAME {&FRAME-NAME}.
   TOG_SKAP = TRUE.
   TOG_SKAP:HIDDEN = TRUE.
   {muswait.i}
   IF defprisid = "" THEN DO TRANSACTION:
      RUN nyprislista_UI IN defprisproch (OUTPUT defprisid).
      CREATE defpristemp.
      ASSIGN 
      defpristemp.PRISID = defprisid
      defpristemp.STARTDATUM = TODAY.
   END.
   IF Guru.Konstanter:globforetag = "cAKEA" THEN DO:
      RUN PrisFakt_UI IN defprisproch (OUTPUT TABLE befprislisttemp).
      BRW_BEF:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RUN openbdynspec_UI IN brwproc[1].
      RETURN. 
   END.   
   RUN nylistdprisny_UI IN defprisproch (INPUT defprisid, INPUT FILL-IN-LIST, INPUT Guru.Konstanter:varforetypval[4]).
   RUN laddajointables_UI IN defprisproch
      (INPUT defprisid, OUTPUT TABLE befprislisttemp, OUTPUT TABLE otextpristemp).
   IF FILL-IN-LIST = TRUE THEN DO:
      RUN openbdynspec_UI IN brwproc[2].
      ASSIGN
      BRW_KUND:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_AND:HIDDEN IN FRAME {&FRAME-NAME} = FALSE            
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_KOP-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      RUN openbdynspec_UI IN brwproc[1].
      ASSIGN
      BRW_BEF:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_KOP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_ENTRAK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN_FLERTRAK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_FRTJPA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_MIL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_MTRLPA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      FIND FIRST prislotemp WHERE prislotemp.PRISID = defpristemp.PRISID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE prislotemp THEN DO TRANSACTION:
         CREATE prislotemp.
         prislotemp.PRISID = defpristemp.PRISID.
      END.
      ASSIGN
      FILL-IN_ENTRAK = prislotemp.ENTRAK
      FILL-IN_FLERTRAK = prislotemp.FLERTRAK
      FILL-IN_FRTJPA = prislotemp.FRTJPA
      FILL-IN_MIL = prislotemp.MIL
      FILL-IN_MTRLPA = prislotemp.MTRLPA.
      DISPLAY FILL-IN_ENTRAK FILL-IN_FLERTRAK FILL-IN_FRTJPA FILL-IN_MIL
      FILL-IN_MTRLPA WITH FRAME {&FRAME-NAME}.
   END.   
   {musarrow.i}      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI DIALOG-3 
PROCEDURE ny_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   FIND FIRST extraopltemp NO-ERROR.
   IF AVAILABLE extraopltemp THEN DELETE extraopltemp.
   CREATE extraopltemp.
   ASSIGN
   extraopltemp.PRISID = defprisid
   extraopltemp.EQDAG = 0.          /* Ny post */
   RELEASE extraopltemp.
   musz = FALSE.
   RUN NYDEFPDO.W.    
   IF musz = FALSE THEN DO:       
      FIND FIRST extraopltemp NO-LOCK NO-ERROR.
      IF AVAILABLE extraopltemp THEN DO:
         RUN laddajointables_UI IN defprisproch
            (INPUT extraopltemp.PRISID, OUTPUT TABLE befprislisttemp, OUTPUT TABLE otextpristemp).
         RUN openbdynspec_UI IN brwproc[2].
         FIND FIRST otextpristemp WHERE otextpristemp.EQDAG = extraopltemp.EQDAG AND
            otextpristemp.START = extraopltemp.START AND otextpristemp.SLUT = extraopltemp.SLUT AND 
            otextpristemp.BEFATTNING = extraopltemp.BEFATTNING NO-LOCK NO-ERROR.
         IF AVAILABLE otextpristemp THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(otextpristemp)).
            RUN lastselectdyn_UI IN brwproc[2].
         END.
      END.
   END.   
   musz = FALSE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

