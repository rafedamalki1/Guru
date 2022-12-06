&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-2



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/28 -  9:26 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}

{AVTAONRTEMP.I}
&Scoped-define SHARED SHARED

{AONRDEF.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{KOSTREG.I}
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO. 
DEFINE VARIABLE str AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE VARIABLE pkost AS DECIMAL NO-UNDO.
DEFINE VARIABLE tkost AS DECIMAL NO-UNDO.
DEFINE VARIABLE mkost AS DECIMAL NO-UNDO.
DEFINE VARIABLE mtrlkost AS DECIMAL NO-UNDO.
DEFINE VARIABLE okost AS DECIMAL NO-UNDO.       
DEFINE VARIABLE momskost AS DECIMAL NO-UNDO.    
DEFINE VARIABLE inkost AS DECIMAL NO-UNDO.
DEFINE VARIABLE kostandapph AS HANDLE NO-UNDO.

/* DEFINE QUERY kostq FOR KOSTREG. */
{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-2
&Scoped-define BROWSE-NAME BRW_KOST

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kostregtemp

/* Definitions for BROWSE BRW_KOST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KOST kostregtemp.LEVKOD ~
kostregtemp.BENAMNING kostregtemp.FAKTNR kostregtemp.FAKTURERAD ~
kostregtemp.BOKKONTO kostregtemp.REGDATUM kostregtemp.INKOMST ~
kostregtemp.PERSKOST kostregtemp.TRAKTKOST kostregtemp.MASKKOST ~
kostregtemp.MTRL kostregtemp.OVRKR kostregtemp.MOMS kostregtemp.BETDATUM ~
kostregtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOST kostregtemp.LEVKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KOST kostregtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KOST kostregtemp
&Scoped-define QUERY-STRING-BRW_KOST FOR EACH kostregtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOST OPEN QUERY BRW_KOST FOR EACH kostregtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOST kostregtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOST kostregtemp


/* Definitions for DIALOG-BOX DIALOG-2                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-21 BTN_NY BTN_UPP BTN_BORT BTN_SKR ~
BTN_NVE FILL-IN_BENAMNING FILL-IN_REGDATUM BTN_AVB BTN_FVE 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_AONR FILL-IN_DELNR FILL-IN_ORT ~
FILL-IN_BENAMNING FILL-IN_REGDATUM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Det finns ingen kostnad registrerad" 
     VIEW-AS FILL-IN 
     SIZE 39.5 BY 1.5
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_BENAMNING AS CHARACTER FORMAT "x(15)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 16.75 BY .92.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "Ort/Benämning" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN_REGDATUM AS DATE FORMAT "99/99/99" 
     LABEL "Bokf.datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 2.42
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KOST FOR 
      kostregtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOST DIALOG-2 _STRUCTURED
  QUERY BRW_KOST NO-LOCK DISPLAY
      kostregtemp.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U
      kostregtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(20)":U
      kostregtemp.FAKTNR COLUMN-LABEL "Ref.nr" FORMAT "x(10)":U
      kostregtemp.FAKTURERAD COLUMN-LABEL "Fakt." FORMAT "Ja/Nej":U
      kostregtemp.BOKKONTO COLUMN-LABEL "Bokför!konto" FORMAT "X(6)":U
      kostregtemp.REGDATUM COLUMN-LABEL "Bokf.!datum" FORMAT "99/99/99":U
      kostregtemp.INKOMST COLUMN-LABEL "Intäkter" FORMAT "->>>>>>>>9":U
            COLUMN-FGCOLOR 9 LABEL-FGCOLOR 9
      kostregtemp.PERSKOST COLUMN-LABEL "Pers.!kost" FORMAT "->>>>>>>9":U
      kostregtemp.TRAKTKOST COLUMN-LABEL "Trakt.!kost" FORMAT "->>>>>>>9":U
      kostregtemp.MASKKOST COLUMN-LABEL "Mask.!kost" FORMAT "->>>>>>>9":U
      kostregtemp.MTRL COLUMN-LABEL "Mtrl.!kr" FORMAT "->>>>>>>9":U
      kostregtemp.OVRKR COLUMN-LABEL "Övrig.!kost" FORMAT "->>>>>>>9":U
      kostregtemp.MOMS COLUMN-LABEL "Moms" FORMAT "->>>>>>9":U
      kostregtemp.BETDATUM COLUMN-LABEL "Inläst.!datum" FORMAT "99/99/99":U
      kostregtemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
  ENABLE
      kostregtemp.LEVKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 122.88 BY 14.13.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     FILL-IN_AONR AT ROW 3.63 COL 25 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 3.63 COL 42.5 COLON-ALIGNED
     FILL-IN_ORT AT ROW 5.13 COL 25 COLON-ALIGNED
     BRW_KOST AT ROW 6.58 COL 1.5
     FILL-IN-TEXT AT ROW 13.13 COL 30.5 COLON-ALIGNED NO-LABEL
     BTN_NY AT ROW 21.25 COL 35.13
     BTN_UPP AT ROW 21.25 COL 47.63
     BTN_BORT AT ROW 21.25 COL 60.13
     BTN_SKR AT ROW 21.25 COL 72.63
     BTN_NVE AT ROW 23.29 COL 86.5
     FILL-IN_BENAMNING AT ROW 23.67 COL 45.5 COLON-ALIGNED
     FILL-IN_REGDATUM AT ROW 23.67 COL 74.63 COLON-ALIGNED
     BTN_AVB AT ROW 24.08 COL 111
     BTN_FVE AT ROW 24.17 COL 86.5
     "Kostnadsregistrering för:" VIEW-AS TEXT
          SIZE 44 BY 1.5 AT ROW 1.63 COL 10
          FONT 17
     "Sök på:" VIEW-AS TEXT
          SIZE 8.13 BY .83 AT ROW 23.75 COL 26.88
     RECT-21 AT ROW 22.75 COL 26
     SPACE(34.99) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kostnadsregistrering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: kostregtemp T "?" NO-UNDO temp-db kostregtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_KOST FILL-IN_ORT DIALOG-2 */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_KOST IN FRAME DIALOG-2
   NO-ENABLE                                                            */
ASSIGN 
       BRW_KOST:HIDDEN  IN FRAME DIALOG-2                = TRUE
       BRW_KOST:MAX-DATA-GUESS IN FRAME DIALOG-2         = 300
       BRW_KOST:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-2 = TRUE
       BRW_KOST:COLUMN-RESIZABLE IN FRAME DIALOG-2       = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-TEXT:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AONR IN FRAME DIALOG-2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME DIALOG-2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ORT IN FRAME DIALOG-2
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOST
/* Query rebuild information for BROWSE BRW_KOST
     _TblList          = "Temp-Tables.kostregtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kostregtemp.LEVKOD
"kostregtemp.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kostregtemp.BENAMNING
"kostregtemp.BENAMNING" "Benämning" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kostregtemp.FAKTNR
"kostregtemp.FAKTNR" "Ref.nr" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kostregtemp.FAKTURERAD
"kostregtemp.FAKTURERAD" "Fakt." ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kostregtemp.BOKKONTO
"kostregtemp.BOKKONTO" "Bokför!konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.kostregtemp.REGDATUM
"kostregtemp.REGDATUM" "Bokf.!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.kostregtemp.INKOMST
"kostregtemp.INKOMST" "Intäkter" "->>>>>>>>9" "decimal" ? 9 ? ? 9 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.kostregtemp.PERSKOST
"kostregtemp.PERSKOST" "Pers.!kost" "->>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.kostregtemp.TRAKTKOST
"kostregtemp.TRAKTKOST" "Trakt.!kost" "->>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.kostregtemp.MASKKOST
"kostregtemp.MASKKOST" "Mask.!kost" "->>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > Temp-Tables.kostregtemp.MTRL
"kostregtemp.MTRL" "Mtrl.!kr" "->>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > Temp-Tables.kostregtemp.OVRKR
"kostregtemp.OVRKR" "Övrig.!kost" "->>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > Temp-Tables.kostregtemp.MOMS
"kostregtemp.MOMS" "Moms" "->>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > Temp-Tables.kostregtemp.BETDATUM
"kostregtemp.BETDATUM" "Inläst.!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[15]   > Temp-Tables.kostregtemp.ANVANDARE
"kostregtemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-2
/* Query rebuild information for DIALOG-BOX DIALOG-2
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON END-ERROR OF FRAME DIALOG-2 /* Kostnadsregistrering */
DO:
   IF VALID-HANDLE(kostandapph) THEN DELETE PROCEDURE kostandapph. 
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON ENDKEY OF FRAME DIALOG-2 /* Kostnadsregistrering */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KOST
&Scoped-define SELF-NAME BRW_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOST DIALOG-2
ON MOUSE-SELECT-DBLCLICK OF BRW_KOST IN FRAME DIALOG-2
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   rad = kostregtemp.RADNR.
   {muswait.i}
   vart = "AND".
   
   RUN ANDRAKOST.W (INPUT aonrvar,INPUT delnrvar,INPUT rad).
   
   IF musz = FALSE THEN DO:
      RUN refreshbrw_UI IN brwproc[1].
      FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND
      kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad USE-INDEX KOST NO-LOCK. 
      IF AVAILABLE kostregtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kostregtemp)).
         RUN lastselectdyn_UI IN brwproc[1]. 
      END.
   END.
   {musarrow.i}
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* Avsluta */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-2
ON CHOOSE OF BTN_BORT IN FRAME DIALOG-2 /* Ta bort */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF Guru.Konstanter:globniv NE 0 THEN DO:
      IF kostregtemp.KOSTAUTO = TRUE THEN DO:
         MESSAGE "Du kan inte ta bort automatiskt inlagda registreringar!" 
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   ASSIGN
   rad = kostregtemp.RADNR.
   {muswait.i}
   RUN BORTKOSTU.W (INPUT aonrvar,INPUT delnrvar,INPUT rad).   
   IF musz = FALSE THEN DO:
      DELETE kostregtemp.                                  
      RUN selnextprevrow_UI IN brwproc[1].
      RUN bortkost IN kostandapph (INPUT aonrvar,INPUT delnrvar,INPUT rad).
      RUN refreshbrw_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1]. 
      IF NOT AVAILABLE kostregtemp THEN DO:
         ASSIGN
         FILL-IN-TEXT:HIDDEN = FALSE
         BRW_KOST:HIDDEN = TRUE.
         DISABLE FILL-IN_BENAMNING FILL-IN_REGDATUM BTN_BORT BTN_UPP BTN_SKR WITH FRAME {&FRAME-NAME}.    
      END.      
   END.
   {musarrow.i}
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE DIALOG-2
ON CHOOSE OF BTN_FVE IN FRAME DIALOG-2 /* - */
DO: 
   ASSIGN
   FILL-IN_REGDATUM = INPUT FILL-IN_REGDATUM.   
   FILL-IN_REGDATUM = FILL-IN_REGDATUM - 1.      
   DISPLAY FILL-IN_REGDATUM WITH FRAME {&FRAME-NAME}. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE DIALOG-2
ON CHOOSE OF BTN_NVE IN FRAME DIALOG-2 /* + */
DO:   
   ASSIGN
   FILL-IN_REGDATUM = INPUT FILL-IN_REGDATUM.   
   FILL-IN_REGDATUM = FILL-IN_REGDATUM + 1.        
   DISPLAY FILL-IN_REGDATUM WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-2
ON CHOOSE OF BTN_NY IN FRAME DIALOG-2 /* Ny */
DO:
   
   {muswait.i}
   FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND 
   kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad NO-LOCK NO-ERROR.
   IF AVAILABLE kostregtemp THEN DO:
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      ASSIGN
      rad = kostregtemp.RADNR.      
   END.
   vart = "NYA". 
   RUN nytid_UI.
   RUN ANDRAKOST.W (INPUT aonrvar,INPUT delnrvar,INPUT rad).
   
   {musarrow.i} 
   IF musz = FALSE THEN DO:
      ASSIGN
      FILL-IN-TEXT:HIDDEN = TRUE
      BRW_KOST:HIDDEN = FALSE.
      ENABLE FILL-IN_BENAMNING FILL-IN_REGDATUM BTN_BORT BTN_UPP BTN_SKR WITH FRAME {&FRAME-NAME}. 
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND
      kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad USE-INDEX KOST NO-LOCK. 
      IF AVAILABLE kostregtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kostregtemp)).
         RUN lastselectdyn_UI IN brwproc[1]. 
      END.
   END.
   IF musz = TRUE THEN DO:
      RUN bortkost IN kostandapph (INPUT aonrvar,INPUT delnrvar,INPUT rad).
      FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND
      kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad USE-INDEX KOST NO-LOCK. 
      DELETE kostregtemp.          
   END.
   
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-2
ON CHOOSE OF BTN_SKR IN FRAME DIALOG-2 /* Skriv ut */
DO:    
   {muswait.i}
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT TRUE).
   IF musz = TRUE THEN DO:
      ASSIGN
      musz = FALSE
      skrivut = FALSE.  
   END.
   ELSE DO:
      RUN ut_UI. 
   END.  
   skrivut = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-2
ON MOUSE-MENU-CLICK OF BTN_SKR IN FRAME DIALOG-2 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP DIALOG-2
ON CHOOSE OF BTN_UPP IN FRAME DIALOG-2 /* Ändra */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   rad = kostregtemp.RADNR.
   {muswait.i}
   vart = "AND".
   
   RUN ANDRAKOST.W (INPUT aonrvar,INPUT delnrvar,INPUT rad).
   
   IF musz = FALSE THEN DO:
      RUN refreshbrw_UI IN brwproc[1].
      FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND
      kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad USE-INDEX KOST NO-LOCK. 
      IF AVAILABLE kostregtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kostregtemp)).
         RUN lastselectdyn_UI IN brwproc[1]. 
      END.
   END.
   {musarrow.i}
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BENAMNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BENAMNING DIALOG-2
ON ANY-KEY OF FILL-IN_BENAMNING IN FRAME DIALOG-2 /* Benämning */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_BENAMNING IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BENAMNING DIALOG-2
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_BENAMNING IN FRAME DIALOG-2 /* Benämning */
DO:
   FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING.
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "BENAMNING", INPUT FILL-IN_BENAMNING).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_REGDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_REGDATUM DIALOG-2
ON ANY-KEY OF FILL-IN_REGDATUM IN FRAME DIALOG-2 /* Bokf.datum */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_REGDATUM IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_REGDATUM DIALOG-2
ON MOUSE-MENU-CLICK OF FILL-IN_REGDATUM IN FRAME DIALOG-2 /* Bokf.datum */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_REGDATUM DIALOG-2
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_REGDATUM IN FRAME DIALOG-2 /* Bokf.datum */
DO:   
   FILL-IN_REGDATUM = INPUT FILL-IN_REGDATUM.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   rad = kostregtemp.RADNR.
   IF FILL-IN_REGDATUM = ? THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_REGDATUM IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.          
   FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND 
   kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad NO-LOCK NO-ERROR.
   FIND NEXT kostregtemp WHERE kostregtemp.AONR = aonrvar AND kostregtemp.DELNR = delnrvar AND
   kostregtemp.REGDATUM = FILL-IN_REGDATUM
   USE-INDEX KOST NO-LOCK NO-ERROR.       
   IF NOT AVAILABLE kostregtemp THEN DO:     
      FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND kostregtemp.DELNR = delnrvar AND
      kostregtemp.REGDATUM = FILL-IN_REGDATUM
      USE-INDEX KOST NO-LOCK NO-ERROR.       
      IF NOT AVAILABLE kostregtemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_REGDATUM IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE kostregtemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kostregtemp)).
      RUN lastselectdyn_UI IN brwproc[1].   
   END.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-2 


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
   FIND FIRST aonrtemp WHERE aonrtemp.AONR = aonrvar AND 
   aonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.

   RUN ladda IN kostandapph (INPUT aonrvar,INPUT delnrvar,OUTPUT TABLE kostregtemp).
   
   FILL-IN_ORT:LABEL = Guru.Konstanter:gaonamnk.
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   ASSIGN
   vart = "AND"
   FILL-IN_REGDATUM = TODAY
   FILL-IN_AONR = aonrtemp.AONR
   FILL-IN_DELNR = aonrtemp.DELNR
   FILL-IN_ORT = aonrtemp.ORT.
   RUN enable_UI.       
   {FRMSIZED.I}        
  
   IF Guru.Konstanter:varforetypval[3] = 2 OR Guru.Konstanter:varforetypval[3] = 3 THEN kostregtemp.FAKTURERAD:VISIBLE IN BROWSE BRW_KOST = TRUE.
   ELSE kostregtemp.FAKTURERAD:VISIBLE IN BROWSE BRW_KOST = FALSE.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   ENABLE BRW_KOST WITH FRAME {&FRAME-NAME}.
   GET LAST BRW_KOST. 
   DISPLAY FILL-IN-TEXT WITH FRAME {&FRAME-NAME}.
   IF NOT AVAILABLE kostregtemp THEN DO:
      ASSIGN
      FILL-IN-TEXT:HIDDEN = FALSE
      BRW_KOST:HIDDEN = TRUE.
      DISABLE FILL-IN_BENAMNING FILL-IN_REGDATUM BTN_BORT BTN_UPP BTN_SKR WITH FRAME {&FRAME-NAME}. 
   END.
   ELSE DO :
      ASSIGN
      FILL-IN-TEXT:HIDDEN = TRUE
      rad = kostregtemp.RADNR.
      BRW_KOST:HIDDEN = FALSE.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kostregtemp)).
      RUN lastselectdyn_UI IN brwproc[1].    
   END.
   ASSIGN
   kostregtemp.REGDATUM :LABEL IN BROWSE BRW_KOST = "Bokf.!datum"
   kostregtemp.BETDATUM :LABEL IN BROWSE BRW_KOST = "Inläst.!datum".
   {musarrow.i}  
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   kostregtemp.LEVKOD:READ-ONLY IN BROWSE BRW_KOST = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_KOST:HANDLE IN FRAME {&FRAME-NAME}).
   /*RUN setcolindex_UI IN brwproc[1] (INPUT "RADNR").*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN KOSTANDAPP.P PERSISTENT SET kostandapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KOSTANDAPP.P PERSISTENT SET kostandapph.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-2  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-2  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_AONR FILL-IN_DELNR FILL-IN_ORT FILL-IN_BENAMNING 
          FILL-IN_REGDATUM 
      WITH FRAME DIALOG-2.
  ENABLE RECT-21 BTN_NY BTN_UPP BTN_BORT BTN_SKR BTN_NVE FILL-IN_BENAMNING 
         FILL-IN_REGDATUM BTN_AVB BTN_FVE 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytid_UI DIALOG-2 
PROCEDURE nytid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF vart = "NYA" THEN DO: 
      RUN nytid IN kostandapph (INPUT aonrvar,INPUT delnrvar,OUTPUT rad,
                                OUTPUT TABLE kostregtemp APPEND).
      FIND FIRST kostregtemp WHERE kostregtemp.AONR = aonrvar AND 
      kostregtemp.DELNR = delnrvar AND kostregtemp.RADNR = rad NO-LOCK NO-ERROR.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI DIALOG-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN   
   pkost = 0
   tkost = 0
   mkost = 0
   mtrlkost = 0
   okost = 0         
   inkost = 0.         
   str = 
   "======.===============.========.========.=========.=========.=========.=========.=========.=========.=========.=========".
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   CREATE tidut.
   CREATE tidut.
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "KOSTNADSTREGISTRERING"
   SUBSTRING(tidut.UT,30) = STRING(TODAY).
   CREATE tidut.
   CREATE tidut.     
   ASSIGN
   SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok) + ":"
   SUBSTRING(tidut.UT,14) = FILL-IN_AONR                                
   SUBSTRING(tidut.UT,21) = STRING(FILL-IN_DELNR,Guru.Konstanter:varforetypchar[1]).
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaonamnk) + ":"
   SUBSTRING(tidut.UT,17) = FILL-IN_ORT.   
   CREATE tidut.
   ASSIGN  
   SUBSTRING(tidut.UT,24) = "REG."
   SUBSTRING(tidut.UT,33) = "BETALT"                             
   SUBSTRING(tidut.UT,42) = "PERSONAL"
   SUBSTRING(tidut.UT,52) = "TRAKT."
   SUBSTRING(tidut.UT,62) = "MASKIN"
   SUBSTRING(tidut.UT,72) = "MATERIEL"
   SUBSTRING(tidut.UT,82) = "ÖVRIG".   
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = "LEV-ID"
   SUBSTRING(tidut.UT,8) = "REF.NR"
   SUBSTRING(tidut.UT,24) = "DATUM"
   SUBSTRING(tidut.UT,33) = "DATUM"
   SUBSTRING(tidut.UT,42) = "KOSTNAD"
   SUBSTRING(tidut.UT,52) = "KOSTNAD"
   SUBSTRING(tidut.UT,62) = "KOSTNAD"
   SUBSTRING(tidut.UT,72) = "KOSTNAD"
   SUBSTRING(tidut.UT,82) = "KOSTNAD"   
   SUBSTRING(tidut.UT,92) = "MOMS"
   SUBSTRING(tidut.UT,102) = "INTÄKTER"
   SUBSTRING(tidut.UT,112) = "ANVÄNDARE".
   CREATE tidut.
   ASSIGN tidut.UT = str.
   OPEN QUERY kostq FOR EACH kostregtemp WHERE kostregtemp.AONR = FILL-IN_AONR AND
   kostregtemp.DELNR = FILL-IN_DELNR USE-INDEX KOST NO-LOCK.
   GET FIRST kostq NO-LOCK.
   DO WHILE AVAILABLE(kostregtemp):                  
      CREATE tidut.
      ASSIGN              
      SUBSTRING(tidut.UT,1) = kostregtemp.LEVKOD
      SUBSTRING(tidut.UT,8) = kostregtemp.FAKTNR.
      IF kostregtemp.REGDATUM NE ? THEN                     
      ASSIGN
      SUBSTRING(tidut.UT,24) = STRING(kostregtemp.REGDATUM).
      IF kostregtemp.BETDATUM NE ? THEN                     
      ASSIGN
      SUBSTRING(tidut.UT,33) = STRING(kostregtemp.BETDATUM).
      ASSIGN 
      SUBSTRING(tidut.UT,42) = STRING(kostregtemp.PERSKOST,"->>>>>>>9")
      SUBSTRING(tidut.UT,52) = STRING(kostregtemp.TRAKTKOST,"->>>>>>>9")
      SUBSTRING(tidut.UT,62) = STRING(kostregtemp.MASKKOST,"->>>>>>>9")
      SUBSTRING(tidut.UT,72) = STRING(kostregtemp.MTRL,"->>>>>>>9")
      SUBSTRING(tidut.UT,82) = STRING(kostregtemp.OVRKR,"->>>>>>>9")
      SUBSTRING(tidut.UT,92) = STRING(kostregtemp.MOMS,"->>>>>>>9")
      SUBSTRING(tidut.UT,102) = STRING(kostregtemp.INKOMST,"->>>>>>>9")
      SUBSTRING(tidut.UT,112) = SUBSTRING(kostregtemp.ANVANDARE,1,12). 
      ASSIGN   
      pkost = pkost + kostregtemp.PERSKOST
      tkost = tkost + kostregtemp.TRAKTKOST
      mkost = mkost + kostregtemp.MASKKOST
      mtrlkost = mtrlkost + kostregtemp.MTRL
      okost = okost + kostregtemp.OVRKR    
      momskost = momskost + kostregtemp.MOMS     
      inkost = inkost + kostregtemp.INKOMST.         
      GET NEXT kostq NO-LOCK.
   END.                       
   
   CREATE tidut.  
   CREATE tidut. 
   ASSIGN    
   SUBSTRING(tidut.UT,33) = "SUMMA"
   SUBSTRING(tidut.UT,42) = STRING(pkost,"->>>>>>>9")
   SUBSTRING(tidut.UT,52) = STRING(tkost,"->>>>>>>9")
   SUBSTRING(tidut.UT,62) = STRING(mkost,"->>>>>>>9")
   SUBSTRING(tidut.UT,72) = STRING(mtrlkost,"->>>>>>>9")
   SUBSTRING(tidut.UT,82) = STRING(okost,"->>>>>>>9")
   SUBSTRING(tidut.UT,92) = STRING(momskost,"->>>>>>>9")
   SUBSTRING(tidut.UT,102) = STRING(inkost,"->>>>>>>9").  
   CREATE tidut.  
   CREATE tidut. 
   ASSIGN                    
   SUBSTRING(tidut.UT,33) = "SUMMA TOTALT :" 
   SUBSTRING(tidut.UT,48) = STRING(pkost + tkost + mkost + mtrlkost + okost + momskost).
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN EKLOGL.P.                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

