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

  Created: 95/05/18 -  9:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LAGERTRUM.I}
&Scoped-define NEW 
&Scoped-define SHARED 
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR   elagertrum.
DEFINE INPUT  PARAMETER kvarmeter AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER lplats AS INTEGER.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}


DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO. 
DEFINE VARIABLE varant AS INTEGER NO-UNDO.  
DEFINE VARIABLE varenr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varben AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varlev AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varbestnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE varpris AS DECIMAL NO-UNDO. 
DEFINE VARIABLE varenh AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varbestd AS DATE NO-UNDO. 
DEFINE VARIABLE varbestall AS CHARACTER NO-UNDO.  
DEFINE VARIABLE vardep AS INTEGER NO-UNDO. 
DEFINE VARIABLE varbered AS LOGICAL NO-UNDO.
DEFINE VARIABLE lagerpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE leveransapph AS HANDLE NO-UNDO.
DEFINE VARIABLE enrdepsaldo AS INTEGER NO-UNDO.
DEFINE VARIABLE hjsaldo AS INTEGER NO-UNDO.
DEFINE VARIABLE nytrum AS LOGICAL NO-UNDO.

DEFINE VARIABLE nylagpl AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.


DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_TOMTRUM

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tomlagertrum

/* Definitions for BROWSE BRW_TOMTRUM                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_TOMTRUM tomlagertrum.TRUMMAPLATS ~
tomlagertrum.enr tomlagertrum.stmeter tomlagertrum.lagmeter 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TOMTRUM 
&Scoped-define QUERY-STRING-BRW_TOMTRUM FOR EACH tomlagertrum NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TOMTRUM OPEN QUERY BRW_TOMTRUM FOR EACH tomlagertrum NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TOMTRUM tomlagertrum
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TOMTRUM tomlagertrum


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG-ANENR BRW_TOMTRUM FILL-IN-VILAGERPLATS ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BEST ~
FILL-IN-BANT btn_ok BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG-ANENR FILL-IN-LAGERPLATS ~
FILL-IN-VILAGERPLATS FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS ~
FILL-IN-BEST FILL-IN-BANT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON btn_ok 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BANT AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Meter" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ben?mning" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEST AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
     VIEW-AS FILL-IN 
     SIZE 11.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LAGERPLATS AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VILAGERPLATS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Trumma namn" 
     VIEW-AS FILL-IN 
     SIZE 15.5 BY 1 NO-UNDO.

DEFINE VARIABLE TOG-ANENR AS LOGICAL INITIAL no 
     LABEL "Visa tomma trummor oavsett enr" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.5 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TOMTRUM FOR 
      tomlagertrum SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TOMTRUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TOMTRUM DIALOG-1 _STRUCTURED
  QUERY BRW_TOMTRUM NO-LOCK DISPLAY
      tomlagertrum.TRUMMAPLATS COLUMN-LABEL "Trumma!namn" FORMAT "X(8)":U
      tomlagertrum.enr COLUMN-LABEL "Enr" FORMAT "X(256)":U WIDTH 10
      tomlagertrum.stmeter COLUMN-LABEL "Start!lager" FORMAT ">>>>>9":U
      tomlagertrum.lagmeter COLUMN-LABEL "Lager" FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 41.5 BY 9.25
         TITLE "Dubbelklicka f?r att v?lja Trumma namn fr?n en tom trumma".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     TOG-ANENR AT ROW 1 COL 54 WIDGET-ID 4
     FILL-IN-LAGERPLATS AT ROW 1.5 COL 12 COLON-ALIGNED
     BRW_TOMTRUM AT ROW 2 COL 53.5 WIDGET-ID 200
     FILL-IN-VILAGERPLATS AT ROW 2.79 COL 12 COLON-ALIGNED
     FILL-IN-ENR AT ROW 4.08 COL 12 COLON-ALIGNED
     FILL-IN-BEN AT ROW 5.38 COL 12 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 6.71 COL 12 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 8 COL 12 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-BEST AT ROW 9.33 COL 12 COLON-ALIGNED
     FILL-IN-BANT AT ROW 9.33 COL 34.38 COLON-ALIGNED
     btn_ok AT ROW 11.75 COL 63.5
     BTN_AVB AT ROW 11.75 COL 78.5
     SPACE(5.74) SKIP(0.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrera trumma":L
         DEFAULT-BUTTON btn_ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: tomlagertrum T "?" NO-UNDO temp-db tomlagertrum
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_TOMTRUM FILL-IN-LAGERPLATS DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-LAGERPLATS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TOMTRUM
/* Query rebuild information for BROWSE BRW_TOMTRUM
     _TblList          = "Temp-Tables.tomlagertrum"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tomlagertrum.TRUMMAPLATS
"tomlagertrum.TRUMMAPLATS" "Trumma!namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tomlagertrum.enr
"tomlagertrum.enr" "Enr" "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tomlagertrum.stmeter
"tomlagertrum.stmeter" "Start!lager" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tomlagertrum.lagmeter
"tomlagertrum.lagmeter" "Lager" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TOMTRUM */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Registrera trumma */
DO:
  {muswait.i}
   IF VALID-HANDLE(leveransapph) THEN DELETE PROCEDURE leveransapph.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   musz = TRUE.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TOMTRUM
&Scoped-define SELF-NAME BRW_TOMTRUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TOMTRUM DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_TOMTRUM IN FRAME DIALOG-1 /* Dubbelklicka f?r att v?lja Trumma namn fr?n en tom trumma */
DO:
   status-ok = BRW_TOMTRUM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   FILL-IN-VILAGERPLATS = tomlagertrum.TRUMMAPLATS.
   DISPLAY FILL-IN-VILAGERPLATS WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO: 
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* Ok */
DO:   
   DEFINE VARIABLE lagerpok AS LOGICAL NO-UNDO.
   
   ASSIGN
   FILL-IN-LAGERPLATS = INPUT FILL-IN-LAGERPLATS
   FILL-IN-VILAGERPLATS = INPUT FILL-IN-VILAGERPLATS
   FILL-IN-ENR = INPUT FILL-IN-ENR
   FILL-IN-BEN = INPUT FILL-IN-BEN
   FILL-IN-ENHET = INPUT FILL-IN-ENHET
   FILL-IN-BEST = INPUT FILL-IN-BEST
   FILL-IN-BANT = INPUT FILL-IN-BANT
   FILL-IN-PRIS = INPUT FILL-IN-PRIS.
   
   /* heltokigt -bort 20190618
   nylagpl = INTEGER(FILL-IN-VILAGERPLATS) NO-ERROR.
   IF nylagpl > 0 THEN FILL-IN-LAGERPLATS = INTEGER(FILL-IN-VILAGERPLATS).*/ 
   IF FILL-IN-VILAGERPLATS = "" OR FILL-IN-VILAGERPLATS = "E" THEN DO:
      MESSAGE "Trummanamn f?r inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF Guru.Konstanter:globforetag = "snat" THEN DO:
      /*EXXX= el  SXXX= fiber DXXX= underh?ll*/
      IF SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "E" OR SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "D" OR SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "S"  OR SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "F" 
      OR SUBSTRING(FILL-IN-VILAGERPLATS,1,1) = "G" THEN.
      ELSE DO:
         MESSAGE "Trummanamn ska vara Exxx f?r eltrummor Sxxx f?r fibertrummor Dxxx f?r underh?ll Fxxx f?r felavhj?lpning och Gxxx f?r f?retagstrummor"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
      IF LENGTH(FILL-IN-VILAGERPLATS) = 4  THEN.
      ELSE DO:
         MESSAGE "Trummanamn ska vara Exxx f?r eltrummor Sxxx f?r fibertrummor Dxxx f?r underh?ll Fxxx f?r felavhj?lpning  och Gxxx f?r f?retagstrummor"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
      
   END. 
   IF FILL-IN-ENR = "" THEN DO:
      MESSAGE "Enr f?r inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-BEN = "" THEN DO:
      MESSAGE "Ben?mning f?r inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-ENHET = "" THEN DO:
      MESSAGE "Enhet f?r inte vara blank"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENHET IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-BEST = 0 THEN DO:
      MESSAGE "Antal f?r inte vara 0"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEST IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   IF FILL-IN-BANT:HIDDEN = FALSE THEN DO:
      IF kvarmeter > 0 AND FILL-IN-BEST > kvarmeter THEN DO:
         MESSAGE "Antal kan inte vara st?rre ?n best?llningsantal /returantal " + STRING(FILL-IN-BANT) + "m"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEST IN FRAME {&FRAME-NAME}.
         RETURN.
      END.   
      
      IF FILL-IN-BEST > FILL-IN-BANT THEN DO:
         MESSAGE "Antal kan inte vara st?rre ?n best?llningsantal /returantal " + STRING(FILL-IN-BANT) + "m"
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEST IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
   END.
   IF nytrum = TRUE THEN DO:
      RUN hsalenr_UI IN leveransapph (INPUT FILL-IN-ENR,INPUT Guru.GlobalaVariabler:GuruVdepnr, OUTPUT enrdepsaldo).
      hjsaldo = 0.
      FOR EACH lagertrum WHERE lagertrum.ENR = FILL-IN-ENR AND lagertrum.TRUMMA = "S" NO-LOCK:
         hjsaldo = hjsaldo + lagertrum.LAGMETER.
      END.      
      hjsaldo = hjsaldo + FILL-IN-BEST.
      
      IF hjsaldo > enrdepsaldo THEN DO:
         MESSAGE "Saldo i lager f?r enr " + FILL-IN-ENR + "  ?r: " + STRING(enrdepsaldo) + ". Med denna trumma skulle saldot bli: " + STRING (hjsaldo)
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
   END.
   FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS AND lagertrum.TRUMMANR NE FILL-IN-LAGERPLATS AND lagertrum.LAGMETER > 0
   AND lagertrum.TRUMMA = "S"  USE-INDEX TRUMMANR NO-ERROR.
   /*det ska g? att l?gga upp samma trumma om det ?r 0 i lagmeter, dvs inventeing ?r gjordLena 20191105*/
   IF AVAILABLE lagertrum THEN DO:
      MESSAGE "Trumma namn " + FILL-IN-VILAGERPLATS  + " finns redan upplagd p? trumma " + STRING(lagertrum.TRUMMANR)
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.   
   ELSE DO: 
     /* Det ska g? att l?gga upp ett annat enr p? en t?md trumma Lena 20200203
     FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS AND lagertrum.TRUMMANR NE FILL-IN-LAGERPLATS AND lagertrum.LAGMETER = 0
      AND lagertrum.TRUMMA = "S"  USE-INDEX TRUMMANR NO-ERROR.
      /*det ska g? att l?gga upp samma trumma om det ?r 0 i lagmeter, dvs inventeing ?r gjordLena 20191105*/
      IF AVAILABLE lagertrum THEN DO:
         IF lagertrum.ENR NE FILL-IN-ENR  THEN DO:
            MESSAGE "Trumma namn " + FILL-IN-VILAGERPLATS  + " ?r upplagd med enr " + lagertrum.ENR + " . Nyuppl?gg med detta Trumma namn m?ste ha samma enr." 
            VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
            RETURN.
            END.
      END.*/   
   END.   
   FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS AND lagertrum.TRUMMANR NE FILL-IN-LAGERPLATS AND lagertrum.LAGMETER > 0
   AND lagertrum.TRUMMA = "S"  USE-INDEX TRUMMANR NO-ERROR.
   /*det ska g? att l?gga upp samma trumma om det ?r 0 i lagmeter, dvs inventeing ?r gjordLena 20191105*/
   IF AVAILABLE lagertrum THEN DO:
      MESSAGE "Trumma namn " + FILL-IN-VILAGERPLATS  + " finns redan upplagd p? trumma " + STRING(lagertrum.TRUMMANR)
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
     
   /*FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = FILL-IN-LAGERPLATS USE-INDEX TRUMMANR NO-ERROR.
   IF AVAILABLE lagertrum THEN DO:
      MESSAGE "Trumma nr " + FILL-IN-LAGERPLATS +  " " + FILL-IN-VILAGERPLATS + " finns redan upplagd"
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
      RETURN.
   END.*/
   lagerpok = FALSE. 
   IF elagertrum.ENR = ""  THEN DO:
      ASSIGN
      elagertrum.ENR = FILL-IN-ENR
      elagertrum.BENAMNING = FILL-IN-BEN
      elagertrum.ENHET = FILL-IN-ENHET
      elagertrum.STMETER = FILL-IN-BEST
      elagertrum.LAGMETER = FILL-IN-BEST.
      IF FILL-IN-PRIS > 0 THEN elagertrum.PRIS = FILL-IN-PRIS.
      ELSE elagertrum.PRIS = lagerpris.
   END.   
   ELSE DO:
      ASSIGN
      elagertrum.STMETER = FILL-IN-BEST
      elagertrum.LAGMETER = FILL-IN-BEST.
   END.
   ASSIGN 
   elagertrum.TRUMMA = "S"
   elagertrum.TRUMMANR = FILL-IN-LAGERPLATS
   elagertrum.TRUMMAPLATS = FILL-IN-VILAGERPLATS
   elagertrum.DEPNR = Guru.GlobalaVariabler:GuruVdepnr
   elagertrum.ANVANDARE = Guru.Konstanter:globanv.
   lplats = FILL-IN-LAGERPLATS.
   FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = elagertrum.TRUMMANR AND lagertrum.TRUMMA = "S" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE lagertrum THEN CREATE lagertrum.
   BUFFER-COPY elagertrum TO lagertrum. 
   RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
   APPLY "GO" TO FRAME {&FRAME-NAME}.     
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR DIALOG-1
ON LEAVE OF FILL-IN-ENR IN FRAME DIALOG-1 /* Enr */
DO:
   
   FILL-IN-ENR = INPUT FILL-IN-ENR.
   IF FILL-IN-BEN =  ""  THEN DO:   
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
      EMPTY TEMP-TABLE elagertrum2 NO-ERROR. 
      IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:            
         IF SUBSTRING(FILL-IN-ENR,1,1) NE "E" THEN FILL-IN-ENR = "E" + FILL-IN-ENR.
         DISPLAY  FILL-IN-ENR WITH FRAME {&FRAME-NAME}.                                     
      END.
      RUN henr_UI IN leveransapph (INPUT FILL-IN-ENR,INPUT Guru.GlobalaVariabler:GuruVdepnr, OUTPUT TABLE elagertrum2, OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX .
         DELETE felmeddtemp.
         RETURN NO-APPLY.
      END.
      FIND FIRST  elagertrum2 NO-LOCK NO-ERROR.
      IF AVAILABLE elagertrum2 THEN DO:
         ASSIGN
         FILL-IN-BEN = elagertrum2.BENAMNING
         FILL-IN-ENHET = elagertrum2.ENHET
         FILL-IN-PRIS = elagertrum2.PRIS.
         lagerpris = elagertrum2.PRIS.
         DISPLAY FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRIS DIALOG-1
ON LEAVE OF FILL-IN-PRIS IN FRAME DIALOG-1 /* Pris */
DO:
  FILL-IN-PRIS = INPUT FILL-IN-PRIS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG-ANENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG-ANENR DIALOG-1
ON VALUE-CHANGED OF TOG-ANENR IN FRAME DIALOG-1 /* Tomma trummor ?ven andra enr */
DO:
   TOG-ANENR = INPUT TOG-ANENR.
   
   IF TOG-ANENR = TRUE THEN DO:
      EMPTY TEMP-TABLE tomlagertrum NO-ERROR.    
      FOR EACH lagertrum WHERE /*lagertrum.ENR = FILL-IN-ENR AND*/  lagertrum.LAGMETER = 0 AND lagertrum.TRUMMA = "S"   NO-LOCK:
         CREATE tomlagertrum.
         BUFFER-COPY lagertrum TO tomlagertrum.
      END.      
      /*om det finns b?de t?mda trummor och trummor med meter kvar med samma trumma namn = TRUMMAPLATS*/
      FOR EACH tomlagertrum :
         FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = tomlagertrum.TRUMMAPLATS AND  lagertrum.LAGMETER > 0 AND lagertrum.TRUMMA = "S"  NO-LOCK NO-ERROR.
         IF AVAILABLE lagertrum THEN DO:
            DELETE tomlagertrum.   
         END.   
      END.          
      FIND FIRST tomlagertrum   NO-LOCK NO-ERROR.
      IF AVAILABLE tomlagertrum THEN DO:
         BRW_TOMTRUM:HIDDEN = FALSE.
         RUN openbdynspec_UI IN brwproc[1].
      END.   
      ELSE DO:
         BRW_TOMTRUM:HIDDEN = true.
      END.       
  END.   
  ELSE DO:
     EMPTY TEMP-TABLE tomlagertrum NO-ERROR.
      IF FILL-IN-ENR NE "" THEN DO: 
         FOR EACH lagertrum WHERE lagertrum.ENR = FILL-IN-ENR AND  lagertrum.LAGMETER = 0 AND lagertrum.TRUMMA = "S"   NO-LOCK:
            CREATE tomlagertrum.
            BUFFER-COPY lagertrum TO tomlagertrum.
         END.      
         /*om det finns b?de t?mda trummor och trummor med meter kvar med samma trumma namn = TRUMMAPLATS*/
         FOR EACH tomlagertrum :
            FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = tomlagertrum.TRUMMAPLATS AND  lagertrum.LAGMETER > 0 AND lagertrum.TRUMMA = "S"  NO-LOCK NO-ERROR.
            IF AVAILABLE lagertrum THEN DO:
               DELETE tomlagertrum.   
            END.   
         END.
      END.   
             
      FIND FIRST tomlagertrum   NO-LOCK NO-ERROR.
      IF AVAILABLE tomlagertrum THEN DO:
         BRW_TOMTRUM:HIDDEN = FALSE.
         RUN openbdynspec_UI IN brwproc[1].
      END.   
      ELSE DO:
         BRW_TOMTRUM:HIDDEN = true.
      END. 
  END.   
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
   
   lplats = 0.
   FILL-IN-BANT:LABEL = "Totalt antal".
   nytrum = FALSE.
   FIND FIRST elagertrum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE elagertrum THEN DO:
      nytrum = TRUE. 
      CREATE elagertrum.
      ASSIGN 
      elagertrum.DATUM = TODAY.      
      /*IF Guru.Konstanter:globforetag = "snat" THEN FILL-IN-VILAGERPLATS = "E".*/
   END.
      
   
   ASSIGN
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk
   FILL-IN-BEN = elagertrum.BENAMNING
   FILL-IN-ENHET = elagertrum.ENHET
   FILL-IN-ENR = elagertrum.ENR  
   FILL-IN-BEST = kvarmeter    
   FILL-IN-BANT = elagertrum.STMETER
   FILL-IN-PRIS = elagertrum.PRIS    
   lagerpris = elagertrum.PRIS
   /*FILL-IN-LAGERPLATS = 0*/
   varant = elagertrum.STMETER.
   musz = FALSE. 
   FIND LAST lagertrum  USE-INDEX TRUMMANR NO-ERROR.
   IF AVAILABLE lagertrum THEN FILL-IN-LAGERPLATS = lagertrum.TRUMMANR + 1.
   ELSE FILL-IN-LAGERPLATS = 1.
   /*IF Guru.Konstanter:globforetag = "snat" AND FILL-IN-VILAGERPLATS = "" THEN FILL-IN-VILAGERPLATS = "E".         
   IF FILL-IN-VILAGERPLATS = "" THEN FILL-IN-VILAGERPLATS = STRING(FILL-IN-LAGERPLATS).*/
   EMPTY TEMP-TABLE tomlagertrum NO-ERROR.
   IF FILL-IN-ENR NE "" THEN DO: 
      FOR EACH lagertrum WHERE lagertrum.ENR = FILL-IN-ENR AND  lagertrum.LAGMETER = 0 AND lagertrum.TRUMMA = "S"   NO-LOCK:
         CREATE tomlagertrum.
         BUFFER-COPY lagertrum TO tomlagertrum.
      END.      
      /*om det finns b?de t?mda trummor och trummor med meter kvar med samma trumma namn = TRUMMAPLATS*/
      FOR EACH tomlagertrum :
         FIND FIRST lagertrum WHERE lagertrum.TRUMMAPLATS = tomlagertrum.TRUMMAPLATS AND  lagertrum.LAGMETER > 0 AND lagertrum.TRUMMA = "S"  NO-LOCK NO-ERROR.
         IF AVAILABLE lagertrum THEN DO:
            DELETE tomlagertrum.   
         END.   
      END.
   END.   
   RUN enable_UI.       
   FIND FIRST tomlagertrum   NO-LOCK NO-ERROR.
   IF AVAILABLE tomlagertrum THEN DO:
      BRW_TOMTRUM:HIDDEN = FALSE.
      RUN openbdynspec_UI IN brwproc[1].
   END.   
   ELSE DO:
      BRW_TOMTRUM:HIDDEN = true.
   END.   
   IF FILL-IN-ENR = "" THEN DO:
      ENABLE FILL-IN-ENR FILL-IN-BEST WITH FRAME {&FRAME-NAME}.
      DISABLE  FILL-IN-BEN FILL-IN-ENHET WITH FRAME {&FRAME-NAME}.
      FILL-IN-BANT:HIDDEN = TRUE.
   END.
   ELSE DO:      
      DISABLE  FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BANT WITH FRAME {&FRAME-NAME}.
   END.
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   APPLY "ENTRY" TO FILL-IN-VILAGERPLATS IN FRAME {&FRAME-NAME}.
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
      (INPUT BRW_TOMTRUM:HANDLE IN FRAME {&FRAME-NAME}). 
   IF Guru.Konstanter:appcon THEN DO:
      RUN LEVAPPV.P PERSISTENT SET leveransapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN LEVAPPV.P PERSISTENT SET leveransapph.
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
  DISPLAY TOG-ANENR FILL-IN-LAGERPLATS FILL-IN-VILAGERPLATS FILL-IN-ENR 
          FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BEST FILL-IN-BANT 
      WITH FRAME DIALOG-1.
  ENABLE TOG-ANENR BRW_TOMTRUM FILL-IN-VILAGERPLATS FILL-IN-ENR FILL-IN-BEN 
         FILL-IN-ENHET FILL-IN-PRIS FILL-IN-BEST FILL-IN-BANT btn_ok BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

