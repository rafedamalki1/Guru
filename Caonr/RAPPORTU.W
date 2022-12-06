&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/15 -  2:57 pm

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
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED 
{DIRDEF.I}
{REGVAR.I}
{AONRDEF.I}
{OMRTEMPW.I}
{AVTAONRTEMP.I}
{AUTOMREGTEMP.I}
{ANSPROJBER.I}
{EVAOKONT.I}
{AVTPLANTEMP.I}   
DEFINE  SHARED VARIABLE RAD_SORT AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE qsort AS CHARACTER FORMAT "x(65)" NO-UNDO.
DEFINE SHARED VARIABLE listnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE FILL-IN-PRISTYP AS CHARACTER FORMAT "X(9)":U INITIAL ? NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN_ANLNR AS CHARACTER FORMAT "x(15)". 
DEFINE SHARED VARIABLE FILL-IN_ARBANSVARIG AS CHARACTER FORMAT "x(5)". 
DEFINE SHARED VARIABLE FILL-IN_ARBARTKOD AS INTEGER FORMAT ">>>" INITIAL 0 .    
DEFINE SHARED VARIABLE FILL-IN_BEREDARE AS CHARACTER FORMAT "x(5)" .     
DEFINE SHARED VARIABLE FILL-IN_OMRADE AS CHARACTER FORMAT "x(6)" .     
DEFINE SHARED VARIABLE FILL-IN_PKOD AS INTEGER FORMAT ">>" INITIAL 0 .     
DEFINE SHARED VARIABLE FILL-IN_SLUTVNR AS INTEGER FORMAT ">>>" INITIAL 0 .   
DEFINE SHARED VARIABLE FILL-IN_STARTVNR AS INTEGER FORMAT ">>>" INITIAL 0 .
DEFINE SHARED VARIABLE TOG_ANL AS LOGICAL INITIAL no 
     LABEL "ALLA ANLÄGGNINGAR" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE eqsort AS CHARACTER NO-UNDO.
DEFINE VARIABLE varomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE rappeapph AS HANDLE NO-UNDO.
{TIDUTTTNEW.I}
DEFINE TEMP-TABLE listtemp 
   FIELD SORTERING AS INTEGER 
   FIELD Alistnr AS INTEGER 
   FIELD Askrivut AS LOGICAL 
   FIELD Aaonrrec AS RECID 
   FIELD Amusz AS LOGICAL          
   FIELD Aregvnr AS INTEGER FORMAT "999" 
   FIELD Aregdagnamn AS CHARACTER FORMAT "X(3)"         
   FIELD Aregdatum AS DATE 
   FIELD Abdatum AS DATE 
   FIELD Aavdatum AS DATE 
   FIELD APRISTYP AS CHARACTER FORMAT "X(9)" 
   FIELD AANLNR AS CHARACTER FORMAT "x(15)"
   FIELD AARBANSVARIG AS CHARACTER FORMAT "x(5)"
   FIELD AARBARTKOD AS INTEGER FORMAT ">>"     
   FIELD ABEREDARE AS CHARACTER FORMAT "x(5)"     
   FIELD AOMRADE AS CHARACTER FORMAT "x(6)"      
   FIELD APKOD AS INTEGER FORMAT ">>"      
   FIELD ASLUTVNR AS INTEGER FORMAT ">>>"    
   FIELD ASTARTVNR AS INTEGER FORMAT ">>>" 
   FIELD AANL AS LOGICAL
   FIELD ARAD_FAST AS LOGICAL
   FIELD ASEL_LIST AS CHARACTER  
   FIELD ASEL_OMR AS CHARACTER 
   FIELD ATOG_AVS AS LOGICAL 
   FIELD ATOG_BEN AS LOGICAL 
   FIELD ATOG_EJAV AS LOGICAL 
   FIELD ATOG_KONTO AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-TIDS
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-TIDS                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-TIDS ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_UT BTN_SKRIV BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 32.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_LIST AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 32.5 BY 2.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_OMR AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 21.5 BY 3 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_AVS AS LOGICAL INITIAL no 
     LABEL "Avslutade":L 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_BEN AS LOGICAL INITIAL no 
     LABEL "Visa benämning" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.5 BY .83 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EJAV AS LOGICAL INITIAL no 
     LABEL "Ej avslutade":L 
     VIEW-AS TOGGLE-BOX
     SIZE 18.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_KONTO AS LOGICAL INITIAL no 
     LABEL "Visa konto" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .92 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 107.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     RAD_FAST AT ROW 6.75 COL 4 NO-LABEL
     TOG_AVS AT ROW 7.25 COL 5
     SEL_OMR AT ROW 7.75 COL 4 NO-LABEL
     SEL_LIST AT ROW 7.75 COL 4 NO-LABEL
     TOG_EJAV AT ROW 9 COL 6.5
     TOG_BEN AT ROW 10.75 COL 4.5
     TOG_KONTO AT ROW 10.75 COL 23
     BTN_SKRIV AT ROW 28 COL 80
     BTN_AVS AT ROW 28 COL 95
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.13 BY 28.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Aonr listor "
         HEIGHT             = 28.42
         WIDTH              = 108.13
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 108.13
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 108.13
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-TIDS
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-TIDS */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-TIDS                = TRUE
       BRW_UT:MAX-DATA-GUESS IN FRAME FRAME-TIDS         = 1000.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_LIST IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_LIST:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_OMR IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_OMR:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_AVS IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_AVS:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_BEN IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_BEN:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_EJAV IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_EJAV:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_KONTO IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_KONTO:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.tidut.ut
"tidut.ut" ? "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDS
/* Query rebuild information for FRAME FRAME-TIDS
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_UT
&Scoped-define SELF-NAME BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UT WINDOW-2
ON ROW-DISPLAY OF BRW_UT IN FRAME FRAME-TIDS
DO:
   IF listnr = 17 THEN DO:
      tidut.UT:FORMAT IN BROWSE BRW_UT = "X(132)".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-TIDS /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO: 
   IF listnr = 17  THEN RUN SKRIVVAL.W (INPUT TRUE).   
   ELSE RUN SKRIVVAL.W (INPUT FALSE).   
   IF musz = TRUE THEN musz = FALSE.
   ELSE IF listnr = 17 THEN RUN EKLOGL.P.   
   ELSE RUN EKLOGS.P.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_BEN WINDOW-2
ON VALUE-CHANGED OF TOG_BEN IN FRAME FRAME-TIDS /* Visa benämning */
DO:
   TOG_BEN = INPUT TOG_BEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KONTO WINDOW-2
ON VALUE-CHANGED OF TOG_KONTO IN FRAME FRAME-TIDS /* Visa konto */
DO:
    TOG_KONTO = INPUT TOG_KONTO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


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
   {WIN_M_START.I}
   {muswait.i}   
   {ALLSTARTDYN.I} 
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   IF listnr = 18 THEN DO:
      IF TOG_KONTO = FALSE THEN RUN allaarb_UI.
      ELSE RUN allaarb2_UI.
   END.
   ELSE DO:
      CREATE listtemp.   
      ASSIGN         
      listtemp.SORTERING = RAD_SORT
      listtemp.Alistnr = listnr 
      listtemp.Askrivut = skrivut 
      listtemp.Aaonrrec = aonrrec 
      listtemp.Amusz = musz 
      listtemp.Aregvnr = regvnr 
      listtemp.Aregdagnamn = regdagnamn 
      listtemp.Aregdatum = regdatum 
      listtemp.Abdatum = bdatum 
      listtemp.Aavdatum = avdatum 
      listtemp.APRISTYP = FILL-IN-PRISTYP 
      listtemp.AANLNR = FILL-IN_ANLNR 
      listtemp.AARBANSVARIG = FILL-IN_ARBANSVARIG 
      listtemp.AARBARTKOD = FILL-IN_ARBARTKOD 
      listtemp.ABEREDARE = FILL-IN_BEREDARE 
      listtemp.AOMRADE = FILL-IN_OMRADE 
      listtemp.APKOD = FILL-IN_PKOD 
      listtemp.ASLUTVNR = FILL-IN_SLUTVNR 
      listtemp.ASTARTVNR = FILL-IN_STARTVNR 
      listtemp.AANL = TOG_ANL
      listtemp.ARAD_FAST = RAD_FAST 
      listtemp.ASEL_LIST = SEL_LIST 
      listtemp.ASEL_OMR = SEL_OMR 
      listtemp.ATOG_AVS = TOG_AVS 
      listtemp.ATOG_BEN = TOG_BEN 
      listtemp.ATOG_EJAV = TOG_EJAV 
      listtemp.ATOG_KONTO = TOG_KONTO.      
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN RAPPAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE listtemp, OUTPUT TABLE tidut).
      END.
      ELSE DO:
         RUN RAPPAPP.P  
         (INPUT TABLE listtemp, OUTPUT TABLE tidut).                  
      END.
   END.
   IF listnr = 17 THEN DO:
      tidut.UT:WIDTH-CHARS IN BROWSE BRW_UT = 132.
   END.        
   IF skrivut = FALSE THEN DO: 
      BRW_UT:HIDDEN = FALSE.
   END.
   ELSE DO:      
      IF musz = TRUE THEN musz = FALSE.
      ELSE IF listnr = 17 THEN RUN EKLOGL.P.
      ELSE RUN EKLOGS.P.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}.     
      LEAVE MAIN-BLOCK. 
   END.
   ASSIGN WINDOW-2:TITLE = Guru.Konstanter:gaol + " listor".   
   RUN enable_UI.   
   {FRMSIZE.I}     
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
   {musarrow.i}   
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allaarb_UI WINDOW-2 
PROCEDURE allaarb_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE VARIABLE orgtabh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE orginaltab AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nyfalth AS HANDLE NO-UNDO EXTENT 10.
   EMPTY TEMP-TABLE evaldaao NO-ERROR. 
   FOR EACH valdaao:      
      CREATE evaldaao.
      BUFFER-COPY valdaao TO evaldaao.
   END.   
   CREATE tidut.                        
   ASSIGN
   SUBSTRING(tidut.UT,50) = STRING(TODAY)
   SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS"). 
   CREATE tidut.                         
   ASSIGN
   SUBSTRING(tidut.UT,1) = CAPS(SEL_LIST).
   CREATE tidut.                         
   ASSIGN
   SUBSTRING(tidut.UT,1) = SUBSTRING(CAPS(Guru.Konstanter:gaol),1,12)
   SUBSTRING(tidut.UT,14) = CAPS(Guru.Konstanter:gomrk)      
   SUBSTRING(tidut.UT,21) = CAPS(Guru.Konstanter:gaonamnk)
   SUBSTRING(tidut.UT,70) = "AVSLUTAT".
   CREATE tidut.       
   tidut.UT = "============.======.================================================.========.".       
   eqsort = qsort.
   orginaltab = "".
   orginaltab = "evaldaao".       
   eqsort = REPLACE(eqsort,"valdaao","evaldaao").      
   eqsort = REPLACE(eqsort,"temp","").      
   orgtabh = TEMP-TABLE evaldaao:DEFAULT-BUFFER-HANDLE.  
   nyfalth[1] = orgtabh:BUFFER-FIELD("aonr").
   nyfalth[2] = orgtabh:BUFFER-FIELD("delnr").
   nyfalth[3] = orgtabh:BUFFER-FIELD("omrade").
   nyfalth[4] = orgtabh:BUFFER-FIELD("ort").
   nyfalth[5] = orgtabh:BUFFER-FIELD("AONRAVDATUM").

   CREATE QUERY qh.
   qh:SET-BUFFERS(orgtabh).
   qh:QUERY-PREPARE(eqsort).
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE orgtabh:AVAILABLE:       
      CREATE tidut.     
      ASSIGN
      SUBSTRING(tidut.UT,1) = nyfalth[1]:BUFFER-VALUE
      SUBSTRING(tidut.UT,8) = STRING(nyfalth[2]:BUFFER-VALUE,Guru.Konstanter:varforetypchar[1])  
      SUBSTRING(tidut.UT,14) = nyfalth[3]:BUFFER-VALUE . 
      SUBSTRING(tidut.UT,21) = nyfalth[4]:BUFFER-VALUE . 
      IF nyfalth[5]:BUFFER-VALUE NE 01/01/91 THEN DO:                             
         SUBSTRING(tidut.UT,70) = STRING(nyfalth[5]:BUFFER-VALUE).
      END. 
     qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allaarb2_UI WINDOW-2 
PROCEDURE allaarb2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE VARIABLE orgtabh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE orginaltab AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nyfalth AS HANDLE NO-UNDO EXTENT 10.
   EMPTY TEMP-TABLE evaldaao NO-ERROR. 
   FOR EACH valdaao:      
      CREATE evaldaao.
      BUFFER-COPY valdaao TO evaldaao.
   END.   
   EMPTY TEMP-TABLE evaokont NO-ERROR. 
   EMPTY TEMP-TABLE kbenamntemp  NO-ERROR. 
   RUN ekonto_UI IN rappeapph (INPUT TABLE evaldaao, OUTPUT TABLE  evaokont,OUTPUT TABLE kbenamntemp).   
   CREATE tidut.                        
   ASSIGN
   SUBSTRING(tidut.UT,50) = STRING(TODAY)
   SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS"). 
   CREATE tidut.                         
   ASSIGN
   SUBSTRING(tidut.UT,1) = CAPS(SEL_LIST).
   FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.
   CREATE tidut.                         
   ASSIGN
   SUBSTRING(tidut.UT,1) = SUBSTRING(CAPS(Guru.Konstanter:gaol),1,12)
   SUBSTRING(tidut.UT,14) = CAPS(Guru.Konstanter:gomrk)      
   SUBSTRING(tidut.UT,21) = kbenamntemp.K1
   SUBSTRING(tidut.UT,28) = kbenamntemp.K2
   SUBSTRING(tidut.UT,35) = kbenamntemp.K3
   SUBSTRING(tidut.UT,42) = kbenamntemp.K4
   SUBSTRING(tidut.UT,49) = kbenamntemp.K5
   SUBSTRING(tidut.UT,56) = " %".

   
   CREATE tidut.       
   tidut.UT = "============.======.======.======.======.======.======.===.=========================".   
   eqsort = qsort.
   orginaltab = "".
   orginaltab = "evaokont".       
   eqsort = REPLACE(eqsort,"valdaao","evaokont").      
   eqsort = REPLACE(eqsort,"temp","").      
   orgtabh = TEMP-TABLE evaokont:DEFAULT-BUFFER-HANDLE.  
   nyfalth[1] = orgtabh:BUFFER-FIELD("aonr").
   nyfalth[2] = orgtabh:BUFFER-FIELD("delnr").
   nyfalth[3] = orgtabh:BUFFER-FIELD("K1").
   nyfalth[4] = orgtabh:BUFFER-FIELD("K2").
   nyfalth[5] = orgtabh:BUFFER-FIELD("K3").
   nyfalth[6] = orgtabh:BUFFER-FIELD("K4").
   nyfalth[7] = orgtabh:BUFFER-FIELD("K5").  
   nyfalth[8] = orgtabh:BUFFER-FIELD("SATS%").
   CREATE QUERY qh.
   qh:SET-BUFFERS(orgtabh).
   qh:QUERY-PREPARE(eqsort).
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE orgtabh:AVAILABLE:       
      CREATE tidut.     
      ASSIGN
      SUBSTRING(tidut.UT,1) = nyfalth[1]:BUFFER-VALUE
      SUBSTRING(tidut.UT,8) = STRING(nyfalth[2]:BUFFER-VALUE,Guru.Konstanter:varforetypchar[1])  
      SUBSTRING(tidut.UT,21) = nyfalth[3]:BUFFER-VALUE . 
      SUBSTRING(tidut.UT,28) = nyfalth[4]:BUFFER-VALUE .       
      SUBSTRING(tidut.UT,35) = nyfalth[5]:BUFFER-VALUE .       
      SUBSTRING(tidut.UT,42) = nyfalth[6]:BUFFER-VALUE .       
      SUBSTRING(tidut.UT,49) = nyfalth[7]:BUFFER-VALUE .       
      SUBSTRING(tidut.UT,56) = nyfalth[8]:BUFFER-VALUE .       
     qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
         RUN RAPPEXTRA.P PERSISTENT SET rappeapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      
   END.
   ELSE DO:
      RUN RAPPEXTRA.P PERSISTENT SET rappeapph.         
   END.
  
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  ENABLE BRW_UT BTN_SKRIV BTN_AVS 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

