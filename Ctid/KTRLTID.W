&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2


/* Temp-Table and Buffer definitions                                    */




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
{EXTRADATA.I}
&Scoped-define NEW
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.   

DEFINE VARIABLE periodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pekodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pertot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE pkodtot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE difftot AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE VARIABLE persant AS INTEGER  NO-UNDO.
DEFINE VARIABLE gkand AS LOGICAL NO-UNDO.
DEFINE VARIABLE exdatahmth AS HANDLE NO-UNDO.   
DEFINE VARIABLE asfaktapph AS HANDLE NO-UNDO.
DEFINE VARIABLE pkbef AS CHARACTER NO-UNDO.
DEFINE VARIABLE pkanst AS CHARACTER NO-UNDO.
DEFINE VARIABLE ejmed AS LOGICAL NO-UNDO.
DEFINE VARIABLE atkman AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE invartemp   
   FIELD GA AS CHARACTER
   FIELD GM AS LOGICAL 
   FIELD SK AS LOGICAL 
   FIELD TI AS RECID 
   FIELD PER AS RECID 
   FIELD PER2 AS RECID 
   FIELD MU AS LOGICAL    
   FIELD REGST AS DECIMAL 
   FIELD REGSU AS DECIMAL
   FIELD RV AS INTEGER FORMAT "999" 
   FIELD RDAG AS CHARACTER FORMAT "X(3)"         
   FIELD RD AS DATE 
   FIELD RM AS INTEGER FORMAT "99" 
   FIELD RMN AS CHARACTER  
   FIELD REGA AS INTEGER FORMAT "99" 
   FIELD RT AS DECIMAL
   FIELD BD AS DATE 
   FIELD AD AS DATE 
   FIELD NY AS DECIMAL 
   FIELD SEK AS INTEGER FORMAT "-9999999" 
   FIELD RSEK AS INTEGER 
   FIELD REGS AS INTEGER 
   FIELD GL AS CHARACTER.
   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(90)" NO-UNDO.
{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
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
&Scoped-Define ENABLED-OBJECTS FILL-IN-ATK FILL-IN-FRISK FILL-IN-LONVATK ~
BTN_ATKFRISK BTN_SKRIV BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ATKFRISK 
     LABEL "Uppdat val ATK/Friskv" 
     SIZE 15.38 BY 1 TOOLTIP "Uppdatera ditt val av ATK och friskv?rd f?r n?sta ?r".

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KFLEX 
     LABEL "Kontroll Flex" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ATK AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "ATK i tid n?sta ?r?" 
     VIEW-AS FILL-IN 
     SIZE 4.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS DATE FORMAT "99/99/99":U 
     LABEL "ATK/Friskv?rd senast uppdaterad" 
     VIEW-AS FILL-IN 
     SIZE 9.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-EJREG AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19.13 BY 1
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FILL-IN-FRISK AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Friskv?rd i tid n?sta ?r?" 
     VIEW-AS FILL-IN 
     SIZE 4.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LONVATK AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "L?nev?xla ATK?" 
     VIEW-AS FILL-IN 
     SIZE 4.5 BY 1 TOOLTIP "JA inneb?r pensionsavs?ttning/Tj?nstepension och NEJ utbetalning l?n" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 101.5 BY 24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     FILL-IN-EJREG AT ROW 25.54 COL 3.38 NO-LABEL WIDGET-ID 10
     FILL-IN-DATUM AT ROW 25.54 COL 53.25 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-ATK AT ROW 26.75 COL 29.13 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-FRISK AT ROW 26.75 COL 61.63 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-LONVATK AT ROW 27.96 COL 29.13 COLON-ALIGNED WIDGET-ID 12
     BTN_ATKFRISK AT ROW 28.25 COL 44 WIDGET-ID 2
     BTN_KFLEX AT ROW 28.25 COL 60.5
     BTN_SKRIV AT ROW 28.25 COL 75
     BTN_AVB AT ROW 28.25 COL 89.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103 BY 28.42.


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
         TITLE              = "Kontroll av registrerad tid"
         HEIGHT             = 28.42
         WIDTH              = 103
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 103
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 103
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_UT 1 FRAME-TIDS */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-TIDS
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-TIDS                = TRUE.

ASSIGN 
       BTN_ATKFRISK:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR BUTTON BTN_KFLEX IN FRAME FRAME-TIDS
   NO-ENABLE                                                            */
ASSIGN 
       BTN_KFLEX:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ATK IN FRAME FRAME-TIDS
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-ATK:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DATUM IN FRAME FRAME-TIDS
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-DATUM:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-EJREG IN FRAME FRAME-TIDS
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN-EJREG:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-FRISK IN FRAME FRAME-TIDS
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-FRISK:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-LONVATK IN FRAME FRAME-TIDS
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-LONVATK:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "temp-db.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = temp-db.tidut.ut
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

&Scoped-define SELF-NAME BTN_ATKFRISK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ATKFRISK WINDOW-2
ON CHOOSE OF BTN_ATKFRISK IN FRAME FRAME-TIDS /* Uppdat val ATK/Friskv */
DO:
   {muswait.i}  
   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "ATKFRISK"                   
   inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.                    
   inextradatatemp.HUVUDINT = YEAR(bdatum) + 1.      
   inextradatatemp.SOKLOG[1] = FILL-IN-ATK .   
   inextradatatemp.SOKDAT[1] = TODAY.              
   inextradatatemp.SOKLOG[3] = FALSE.  /*GODK?ND*/   
   inextradatatemp.SOKCHAR[1] = Guru.Konstanter:globanv.
   IF Guru.Konstanter:globforetag = "gkal" THEN DO:
      inextradatatemp.SOKLOG[2] = FILL-IN-FRISK .
      inextradatatemp.SOKLOG[5] = FILL-IN-LONVATK .
   END.
   IF Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "SUND"  OR Guru.Konstanter:globforetag = "MISV" THEN DO:      
      inextradatatemp.SOKLOG[5] = FILL-IN-LONVATK .
   END.      
   RUN extraspar_UI IN exdatahmth (INPUT TABLE inextradatatemp).
   FILL-IN-DATUM = TODAY.
   FILL-IN-EJREG = "Registrerad " + STRING(YEAR(bdatum)+ 1)  + " f?r " + tidpers.PERSONALKOD.
   IF Guru.Konstanter:globforetag = "gkal" THEN   DISPLAY FILL-IN-EJREG FILL-IN-ATK FILL-IN-FRISK FILL-IN-DATUM  WITH FRAME FRAME-TIDS.
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "misv"  OR Guru.Konstanter:globforetag = "snat" THEN   DISPLAY FILL-IN-EJREG FILL-IN-ATK FILL-IN-DATUM  WITH FRAME FRAME-TIDS. 
  
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-TIDS /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KFLEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KFLEX WINDOW-2
ON CHOOSE OF BTN_KFLEX IN FRAME FRAME-TIDS /* Kontroll Flex */
DO:
  {muswait.i}  
 
  IF Guru.Konstanter:appcon THEN DO:                           
      RUN NATTMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE tidpers).
   END.
   ELSE DO:
      RUN NATTMAN.P 
      (INPUT TABLE tidpers).                  
   END.
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   
   IF Guru.Konstanter:appcon THEN DO:                                 
      RUN APKTRL2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE invartemp, INPUT TABLE tidpers, OUTPUT TABLE tidut).
   END.
   ELSE DO:
      RUN APKTRL2.P 
      (INPUT TABLE invartemp, INPUT TABLE tidpers, OUTPUT TABLE tidut).
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
      FOR EACH tidut WHERE SUBSTRING(tidut.UT,57,11) = "P?minnelse!" :                
         DELETE tidut.
      END.
      FOR EACH tidut WHERE SUBSTRING(tidut.UT,57,8) = "Varning!" :                  
         DELETE tidut.
      END.
   END.
   FIND FIRST tidut WHERE tidut.UT = str NO-LOCK NO-ERROR. 
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF AVAILABLE tidut THEN DO:    
      ENABLE BRW_UT WITH FRAME FRAME-TIDS.
      BRW_UT:HIDDEN = FALSE.
   END.
   ELSE DO:
      MESSAGE "Alla s?kta personers tider under m?nad " MONTH(avdatum) "-" YEAR(avdatum) " st?mmer med deras arbetstider."
      VIEW-AS ALERT-BOX.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      
   END.
   OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).
   RUN EKLOGS.P.
   skrivut = FALSE.
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


&Scoped-define SELF-NAME FILL-IN-ATK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ATK WINDOW-2
ON LEAVE OF FILL-IN-ATK IN FRAME FRAME-TIDS /* ATK i tid n?sta ?r? */
DO:
   FILL-IN-ATK = INPUT FILL-IN-ATK.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ATK WINDOW-2
ON MOUSE-SELECT-CLICK OF FILL-IN-ATK IN FRAME FRAME-TIDS /* ATK i tid n?sta ?r? */
DO:
   IF INPUT FILL-IN-ATK = TRUE THEN FILL-IN-ATK = FALSE.
   IF INPUT FILL-IN-ATK = FALSE THEN FILL-IN-ATK = TRUE.
   DISPLAY FILL-IN-ATK WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
      IF FILL-IN-ATK = TRUE  THEN DO:
          FILL-IN-LONVATK = FALSE.
          FILL-IN-LONVATK:HIDDEN                    = TRUE.
      END.        
      ELSE FILL-IN-LONVATK:HIDDEN                    = FALSE .
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM WINDOW-2
ON LEAVE OF FILL-IN-DATUM IN FRAME FRAME-TIDS /* ATK/Friskv?rd senast uppdaterad */
DO:
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FRISK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FRISK WINDOW-2
ON LEAVE OF FILL-IN-FRISK IN FRAME FRAME-TIDS /* Friskv?rd i tid n?sta ?r? */
DO:
   FILL-IN-FRISK = INPUT FILL-IN-FRISK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FRISK WINDOW-2
ON MOUSE-SELECT-CLICK OF FILL-IN-FRISK IN FRAME FRAME-TIDS /* Friskv?rd i tid n?sta ?r? */
DO:
  IF INPUT FILL-IN-FRISK = TRUE THEN FILL-IN-FRISK = FALSE.
   IF INPUT FILL-IN-FRISK = FALSE THEN FILL-IN-FRISK = TRUE.
   DISPLAY FILL-IN-FRISK WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LONVATK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LONVATK WINDOW-2
ON LEAVE OF FILL-IN-LONVATK IN FRAME FRAME-TIDS /* L?nev?xla ATK? */
DO:
 FILL-IN-LONVATK = INPUT FILL-IN-LONVATK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LONVATK WINDOW-2
ON MOUSE-SELECT-CLICK OF FILL-IN-LONVATK IN FRAME FRAME-TIDS /* L?nev?xla ATK? */
DO:
   IF INPUT FILL-IN-LONVATK = TRUE THEN FILL-IN-LONVATK = FALSE.
   IF INPUT FILL-IN-LONVATK = FALSE THEN FILL-IN-LONVATK = TRUE.
   DISPLAY FILL-IN-LONVATK WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(exdatahmth) THEN DELETE PROCEDURE exdatahmth NO-ERROR.
   IF VALID-HANDLE(asfaktapph) THEN DELETE PROCEDURE asfaktapph NO-ERROR.   
   RUN disable_UI.
END.
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
   {ALLSTARTDYN.I} 
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   ASSIGN   str=                                                                              
"===========================================================================================================".   
         
   {muswait.i} 
   CREATE invartemp.
   ASSIGN
   invartemp.GA = Guru.Konstanter:globanv
   invartemp.GM = gvisatidpermanad
   invartemp.SK = skrivut 
   invartemp.TI = tidtabrec 
   invartemp.PER = persrec 
   invartemp.PER2 = persrec2 
   invartemp.MU = musz    
   invartemp.REGST = regstart  
   invartemp.REGSU = regslut 
   invartemp.RV = regvnr 
   invartemp.RDAG = regdagnamn         
   invartemp.RD = regdatum 
   invartemp.RM = regmnr 
   invartemp.RMN = regmannamn 
   invartemp.REGA = regar 
   invartemp.RT = regtotalt       
   invartemp.BD = bdatum 
   invartemp.AD = avdatum 
   invartemp.NY = nytid 
   invartemp.SEK = sekunder 
   invartemp.RSEK = regstartsek 
   invartemp.REGS = regslutsek 
   invartemp.GL = Guru.Konstanter:globforetag.

   IF Guru.Konstanter:appcon THEN DO:                                 
      RUN APKTRL2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE invartemp, INPUT TABLE tidpers, OUTPUT TABLE tidut).
   END.
   ELSE DO:
      RUN APKTRL2.P 
      (INPUT TABLE invartemp, INPUT TABLE tidpers, OUTPUT TABLE tidut).
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR Guru.Konstanter:globforetag = "celpa" THEN DO:      
      FOR EACH tidut WHERE SUBSTRING(tidut.UT,57,11) = "P?minnelse!" :         
         MESSAGE SUBSTRING(tidut.UT,57) + " " + SUBSTRING(tidut.UT,44,8) + " f?r " + SUBSTRING(tidut.UT,1,6)  VIEW-AS ALERT-BOX.
         DELETE tidut.
      END.
      FOR EACH tidut WHERE SUBSTRING(tidut.UT,57,8) = "Varning!" :         
         MESSAGE SUBSTRING(tidut.UT,57) + " G?ller " + SUBSTRING(tidut.UT,1,6)  VIEW-AS ALERT-BOX.
         DELETE tidut.
      END.
   END.
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
      FOR EACH tidut WHERE SUBSTRING(tidut.UT,57,11) = "P?minnelse!" :         
         MESSAGE SUBSTRING(tidut.UT,57) + " f?r " + SUBSTRING(tidut.UT,1,6) + "?"  VIEW-AS ALERT-BOX.
         DELETE tidut.
      END.
      FOR EACH tidut WHERE SUBSTRING(tidut.UT,57,8) = "Varning!" :         
         MESSAGE SUBSTRING(tidut.UT,57) + " G?ller " + SUBSTRING(tidut.UT,1,6)  VIEW-AS ALERT-BOX.
         DELETE tidut.
      END.
   END.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
      FOR EACH tidut WHERE SUBSTRING(tidut.UT,57,11) = "Dubbelcheck" :         
         MESSAGE SUBSTRING(tidut.UT,57) + " " + SUBSTRING(tidut.UT,44,8) + " f?r " + SUBSTRING(tidut.UT,1,6)  VIEW-AS ALERT-BOX.
         DELETE tidut.
      END.
      FOR EACH tidut WHERE SUBSTRING(tidut.UT,57,7) = "Varning" :         
         MESSAGE SUBSTRING(tidut.UT,57) + " G?ller " + SUBSTRING(tidut.UT,1,6)  VIEW-AS ALERT-BOX.
         DELETE tidut.
      END.      
   END.      
   FIND FIRST tidut WHERE tidut.UT = str NO-LOCK NO-ERROR. 
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF AVAILABLE tidut THEN DO:
      IF SUBSTRING(tidut.UT,57,10) = "P?minnelse" THEN DO:
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            FIND FIRST tidut WHERE SUBSTRING(tidut.UT,57,10) = "P?minnelse" NO-LOCK NO-ERROR.
            DELETE tidut.
            MESSAGE "Alla s?kta personers tider under m?nad " MONTH(avdatum) "-" YEAR(avdatum) " st?mmer med deras arbetstider."
            VIEW-AS ALERT-BOX.      
            status-mus2 = SESSION:SET-WAIT-STATE("").
         END.
         ELSE DO:
            ENABLE BRW_UT WITH FRAME FRAME-TIDS.
            BRW_UT:HIDDEN = FALSE.
         END.      
      END.
      ELSE DO:                    
         ENABLE BRW_UT WITH FRAME FRAME-TIDS.
         BRW_UT:HIDDEN = FALSE.
      END.   
   END.      
   ELSE DO:
      MESSAGE "Alla s?kta personers tider under m?nad " MONTH(avdatum) "-" YEAR(avdatum) " st?mmer med deras arbetstider."
      VIEW-AS ALERT-BOX.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      LEAVE MAIN-BLOCK.     
   END.
   
   FIND FIRST tidpers WHERE tidpers.TIDPERSREC = persrec NO-LOCK NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "ATKFRISK"                   
   inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.                    
   inextradatatemp.HUVUDINT = YEAR(bdatum) + 1.                    
   RUN etabhamt_UI IN exdatahmth (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp APPEND).
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   FIND FIRST extradatatemp NO-ERROR.
   
   RUN enable_UI.   
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "misv"  OR Guru.Konstanter:globforetag = "snat" THEN  BTN_ATKFRISK:LABEL = "Uppdat val ATK". 
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "misv" OR Guru.Konstanter:globforetag = "snat" THEN FILL-IN-DATUM:LABEL = "ATK-val senast uppdaterad".
   IF Guru.Konstanter:globforetag = "sund" OR  Guru.Konstanter:globforetag = "misv" THEN  DO:
      /*FILL-IN-LONVATK:LABEL = "Pensionsvalet ATK".*/
      FILL-IN-LONVATK:LABEL = "Pensionsavs?ttning= Ja / Utbetalning l?n =Nej".
      FILL-IN-ATK:LABEL = "Atk i tid =Ja / Atk i pengar =Nej".
   END.   
     
   IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      ENABLE BTN_KFLEX WITH FRAME FRAME-TIDS.
   END.
         
   FILL-IN-EJREG = "Ej registrerad f?r " + tidpers.PERSONALKOD.
   FILL-IN-EJREG:HIDDEN IN FRAME FRAME-TIDS           = TRUE.
   BTN_ATKFRISK:HIDDEN IN FRAME FRAME-TIDS           = TRUE.
   FILL-IN-DATUM:HIDDEN IN FRAME FRAME-TIDS           = TRUE.
   FILL-IN-ATK:HIDDEN IN FRAME FRAME-TIDS           = TRUE.
   FILL-IN-FRISK:HIDDEN IN FRAME FRAME-TIDS           = TRUE.
   FILL-IN-LONVATK:HIDDEN IN FRAME FRAME-TIDS           = TRUE.
   
   IF Guru.Konstanter:globforetag = "cGKAL"   THEN DO:
      /*kalmar ska inte l?ngre v?lja atk. De ska s?ttas till att alla f?r ta ut atk i tid vid ?rsskiftet 20230101 Lena 20220615*/
      RUN atkdat_UI IN asfaktapph (OUTPUT atkman).
      RUN pbef_UI IN asfaktapph (INPUT tidpers.PERSONALKOD, OUTPUT pkbef, OUTPUT pkanst, OUTPUT ejmed).
      /*IF pkbef = "INHYRD PERSONAL" OR pkanst = "ENTREP.AVTAL" OR pkanst = "Extern konsult"  THEN .*/     
      IF ejmed = TRUE THEN.
      ELSE IF MONTH(bdatum) GE atkman THEN DO:        
         FILL-IN-EJREG:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         BTN_ATKFRISK:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         FILL-IN-DATUM:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         FILL-IN-ATK:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         FILL-IN-LONVATK:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         IF Guru.Konstanter:globforetag = "GKAL" THEN  FILL-IN-FRISK:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         IF AVAILABLE extradatatemp  THEN DO:
            FILL-IN-ATK = extradatatemp.SOKLOG[1].
            FILL-IN-FRISK = extradatatemp.SOKLOG[2].
            FILL-IN-LONVATK = extradatatemp.SOKLOG[5].
            FILL-IN-DATUM = extradatatemp.SOKDAT[1].
            FILL-IN-EJREG = "Registrerad " + STRING(extradatatemp.HUVUDINT) + " f?r " + tidpers.PERSONALKOD.               
         END.
         ELSE DO:
            FILL-IN-ATK = TRUE.
            FILL-IN-FRISK = TRUE.
            FILL-IN-LONVATK = FALSE.            
            FILL-IN-EJREG = "Ej registrerad " + STRING(YEAR(bdatum)+ 1) + " f?r " + tidpers.PERSONALKOD.
         END.              
         DISPLAY FILL-IN-EJREG FILL-IN-ATK FILL-IN-FRISK FILL-IN-LONVATK FILL-IN-DATUM  WITH FRAME FRAME-TIDS.
         IF FILL-IN-ATK = TRUE  THEN DO:
             FILL-IN-LONVATK = FALSE.
             FILL-IN-LONVATK:HIDDEN                    = TRUE.
         END.        
         ELSE FILL-IN-LONVATK:HIDDEN                    = FALSE .
            
         IF AVAILABLE extradatatemp  THEN DO:
            /*GODK?ND*/
            IF extradatatemp.SOKLOG[3] = TRUE THEN DO:
               DISABLE FILL-IN-ATK  FILL-IN-FRISK FILL-IN-LONVATK BTN_ATKFRISK WITH FRAME FRAME-TIDS.
            END.
         END.    
      END.   
   END.
   IF  Guru.Konstanter:globforetag = "misv"  THEN DO:      
      RUN atkdat_UI IN asfaktapph (OUTPUT atkman).
      RUN pbef_UI IN asfaktapph (INPUT tidpers.PERSONALKOD, OUTPUT pkbef, OUTPUT pkanst, OUTPUT ejmed).
      /*IF  pkanst = "ENTREP.AVTAL" OR pkanst = "Extern konsult"  THEN .*/     
      IF ejmed = TRUE THEN.
      ELSE IF MONTH(bdatum) GE atkman THEN DO:        
         FILL-IN-EJREG:HIDDEN IN FRAME FRAME-TIDS           = FALSE.

         BTN_ATKFRISK:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         FILL-IN-DATUM:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         FILL-IN-ATK:HIDDEN IN FRAME FRAME-TIDS           = FALSE.    
         FILL-IN-LONVATK:HIDDEN IN FRAME FRAME-TIDS           = FALSE.              
         IF AVAILABLE extradatatemp  THEN DO:
            FILL-IN-ATK = extradatatemp.SOKLOG[1].
            FILL-IN-LONVATK = extradatatemp.SOKLOG[5].                        
            FILL-IN-DATUM = extradatatemp.SOKDAT[1].
            FILL-IN-EJREG = "Registrerad " + STRING(extradatatemp.HUVUDINT) + " f?r " + tidpers.PERSONALKOD.               
         END.
         ELSE DO:
            FILL-IN-ATK = TRUE.
            FILL-IN-LONVATK = FALSE.          
            FILL-IN-EJREG = "Ej registrerad " + STRING(YEAR(bdatum)+ 1) + " f?r " + tidpers.PERSONALKOD.
         END.              
          
         DISPLAY FILL-IN-EJREG FILL-IN-ATK /*FILL-IN-FRISK*/ FILL-IN-LONVATK FILL-IN-DATUM  WITH FRAME FRAME-TIDS.
         IF FILL-IN-ATK = TRUE  THEN DO:
             FILL-IN-LONVATK = FALSE.
             FILL-IN-LONVATK:HIDDEN                    = TRUE.
         END.        
         ELSE FILL-IN-LONVATK:HIDDEN                    = FALSE .
            
         IF AVAILABLE extradatatemp  THEN DO:
            /*GODK?ND*/
            IF extradatatemp.SOKLOG[3] = TRUE THEN DO:
               DISABLE FILL-IN-ATK  /*FILL-IN-FRISK*/ FILL-IN-LONVATK BTN_ATKFRISK WITH FRAME FRAME-TIDS.
            END.
         END.     
          
      END.   
   END.
   IF Guru.Konstanter:globforetag = "snat"  OR Guru.Konstanter:globforetag = "SUND" THEN DO:      
      RUN atkdat_UI IN asfaktapph (OUTPUT atkman).
      RUN pbef_UI IN asfaktapph (INPUT tidpers.PERSONALKOD, OUTPUT pkbef, OUTPUT pkanst, OUTPUT ejmed).          
      IF ejmed = TRUE THEN.
      ELSE IF MONTH(bdatum) GE atkman THEN DO:        
         FILL-IN-EJREG:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         BTN_ATKFRISK:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         FILL-IN-DATUM:HIDDEN IN FRAME FRAME-TIDS           = FALSE.
         FILL-IN-ATK:HIDDEN IN FRAME FRAME-TIDS           = FALSE.      
         FILL-IN-LONVATK:HIDDEN IN FRAME FRAME-TIDS           = FALSE.         
         IF AVAILABLE extradatatemp  THEN DO:
            FILL-IN-ATK = extradatatemp.SOKLOG[1].         
            FILL-IN-LONVATK = extradatatemp.SOKLOG[5].
            FILL-IN-DATUM = extradatatemp.SOKDAT[1].
            FILL-IN-EJREG = "Registrerad " + STRING(extradatatemp.HUVUDINT) + " f?r " + tidpers.PERSONALKOD.               
         END.
         ELSE DO:
            FILL-IN-ATK = TRUE.            
            FILL-IN-LONVATK = FALSE.            
            FILL-IN-EJREG = "Ej registrerad " + STRING(YEAR(bdatum)+ 1) + " f?r " + tidpers.PERSONALKOD.
         END.                       
          DISPLAY FILL-IN-EJREG FILL-IN-ATK /*FILL-IN-FRISK*/ FILL-IN-LONVATK FILL-IN-DATUM  WITH FRAME FRAME-TIDS.
          IF FILL-IN-ATK = TRUE  THEN DO:
             FILL-IN-LONVATK = FALSE.
             FILL-IN-LONVATK:HIDDEN                    = TRUE.
         END.        
         ELSE FILL-IN-LONVATK:HIDDEN                    = FALSE .            
         IF AVAILABLE extradatatemp  THEN DO:
            /*GODK?ND*/
            IF extradatatemp.SOKLOG[3] = TRUE THEN DO:
               DISABLE FILL-IN-ATK  /*FILL-IN-FRISK*/ FILL-IN-LONVATK BTN_ATKFRISK WITH FRAME FRAME-TIDS.
            END.
         END.    
      END.   
   END.   
   {FRMSIZE.I}            
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVB:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "snat" THEN DO:
      ASSIGN
      Guru.GlobalaVariabler:colrighth = BTN_KFLEX:HANDLE.      
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   Guru.GlobalaVariabler:colrighth = BTN_ATKFRISK:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   
   {musarrow.i}    
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
      RUN EXTRADATAHMT.P PERSISTENT SET exdatahmth ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:      
      RUN EXTRADATAHMT.P PERSISTENT SET exdatahmth.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN ASFAKTAPP.P PERSISTENT SET asfaktapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:      
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN ASFAKTAPP.P PERSISTENT SET asfaktapph.
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
  ENABLE FILL-IN-ATK FILL-IN-FRISK FILL-IN-LONVATK BTN_ATKFRISK BTN_SKRIV 
         BTN_AVB 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

