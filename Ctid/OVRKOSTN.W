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

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER varaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vardelnr AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
{RESDEF.I}
&Scoped-define SHARED SHARED
{DIRDEF.I}
{FLEXTAB.I}
{PHMT.I}
{LONTILLAGG.I}
DEFINE SHARED VARIABLE ovrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE ovrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE SHARED VARIABLE tjan AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE overant AS INTEGER NO-UNDO.
DEFINE VARIABLE otim AS INTEGER NO-UNDO.
DEFINE VARIABLE otim2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
{OKOSTLON.I}

&SCOPED-DEFINE NEW 
&SCOPED-DEFINE SHARED 
{TIDALLT.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_OKOST

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES okostlon

/* Definitions for BROWSE BRW_OKOST                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_OKOST okostlon.DATUM okostlon.VILART ~
okostlon.LONTILLANTAL okostlon.LONKODTEXT okostlon.MOMS okostlon.AONR ~
okostlon.DELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OKOST okostlon.DATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_OKOST okostlon
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_OKOST okostlon
&Scoped-define QUERY-STRING-BRW_OKOST FOR EACH okostlon NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OKOST OPEN QUERY BRW_OKOST FOR EACH okostlon NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OKOST okostlon
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OKOST okostlon


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_OKOST BTN_MALL BTN_NY BTN_UPP BTN_BORT ~
FBTN_REG BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-RUBRIK FILL-IN-PKOD ~
FILL-IN_FORNAMN-2 

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

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MALL 
     LABEL "Personligt utlägg mall":L 
     SIZE 14 BY 1.5.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_REG AUTO-GO 
     LABEL "Fortsätt":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RUBRIK AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55.38 BY 2
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 48.88 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_OKOST FOR 
      okostlon SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_OKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OKOST DIALOG-1 _STRUCTURED
  QUERY BRW_OKOST NO-LOCK DISPLAY
      okostlon.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      okostlon.VILART COLUMN-LABEL "Lart" FORMAT "X(8)":U
      okostlon.LONTILLANTAL COLUMN-LABEL "Antal" FORMAT "->>>>9.99":U
      okostlon.LONKODTEXT COLUMN-LABEL "Text" FORMAT "X(20)":U
      okostlon.MOMS COLUMN-LABEL "Moms" FORMAT "->>>>9.99":U
      okostlon.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      okostlon.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U WIDTH 3
  ENABLE
      okostlon.DATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 74.63 BY 7.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-RUBRIK AT ROW 1.29 COL 2.88 NO-LABEL
     FILL-IN-PKOD AT ROW 3.38 COL 12.5 COLON-ALIGNED
     FILL-IN_FORNAMN-2 AT ROW 3.38 COL 21.13 COLON-ALIGNED NO-LABEL
     BRW_OKOST AT ROW 4.79 COL 1.5
     BTN_MALL AT ROW 4.79 COL 76.75
     BTN_NY AT ROW 13.04 COL 1.63
     BTN_UPP AT ROW 13.04 COL 15.63
     BTN_BORT AT ROW 13.04 COL 29.63
     FBTN_REG AT ROW 13.04 COL 62.25
     BTN_AVB AT ROW 13.04 COL 76.75
     SPACE(0.99) SKIP(0.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrera övriga ersättningar":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: okostlon T "?" NO-UNDO temp-db okostlon
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_OKOST FILL-IN_FORNAMN-2 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-RUBRIK IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OKOST
/* Query rebuild information for BROWSE BRW_OKOST
     _TblList          = "Temp-Tables.okostlon"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.okostlon.DATUM
"okostlon.DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.okostlon.VILART
"okostlon.VILART" "Lart" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.okostlon.LONTILLANTAL
"okostlon.LONTILLANTAL" "Antal" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.okostlon.LONKODTEXT
"okostlon.LONKODTEXT" "Text" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.okostlon.MOMS
"okostlon.MOMS" "Moms" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.okostlon.AONR
"okostlon.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.okostlon.DELNR
"okostlon.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OKOST */
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
ON END-ERROR OF FRAME DIALOG-1 /* Registrera övriga ersättningar */
DO:   
   {BORTBRWPROC.I}
   musz = TRUE.
   EMPTY TEMP-TABLE okost NO-ERROR.    
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Registrera övriga ersättningar */
DO:  
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-1
ON CHOOSE OF BTN_BORT IN FRAME DIALOG-1 /* Ta bort */
DO:      
   status-ok = BRW_OKOST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ovrec = RECID(okostlon).
   ovrec2 = ovrec.
   MESSAGE "Vill du verkligen ta bort" okostlon.VILART " ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av övriga kostnader"
   UPDATE answer AS LOGICAL.    
   IF answer THEN DO:      
      status-ok = BRW_OKOST:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN ovrec2 = RECID(okostlon).
      ELSE DO:          
         status-ok = BRW_OKOST:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
         IF status-ok = TRUE THEN ovrec2 = RECID(okostlon).          
         ELSE DO:
            ASSIGN
            BTN_UPP:HIDDEN = TRUE
            BTN_BORT:HIDDEN = TRUE
            BTN_MALL:HIDDEN = TRUE
            BRW_OKOST:HIDDEN = TRUE.
            ovrec2 = ovrec.
         END.
      END.      
      DO TRANSACTION:
         FIND okostlon WHERE RECID(okostlon) = ovrec EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE okostlon THEN DO:
            FIND FIRST okost WHERE RECID(okost) = okostlon.OKOSTREC EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE okost THEN DELETE okost.  
            DELETE okostlon.           
         END.
      END.
   END.   
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   {musarrow.i}
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MALL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MALL DIALOG-1
ON CHOOSE OF BTN_MALL IN FRAME DIALOG-1 /* Personligt utlägg mall */
DO:
   
   IF AVAILABLE okostlon THEN DO:
      EMPTY TEMP-TABLE etidallt NO-ERROR. 
      CREATE etidallt.
      ASSIGN 
      etidallt.PERSONALKOD =   okostlon.PERSONALKOD
      etidallt.DATUM =   okostlon.DATUM
      etidallt.AONR =   okostlon.AONR
      etidallt.DELNR =   okostlon.DELNR
      etidallt.LONTILLANTAL =   okostlon.LONTILLANTAL
      etidallt.LONTILLAGG =   okostlon.LONTILLAGG.      
      RUN LONBLANKETT.P (INPUT TABLE etidallt).       
   END.
   
   {musarrow.i}
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-1
ON CHOOSE OF BTN_NY IN FRAME DIALOG-1 /* Ny */
DO:
   RUN nyskap_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP DIALOG-1
ON CHOOSE OF BTN_UPP IN FRAME DIALOG-1 /* Ändra */
DO:
   status-ok = BRW_OKOST:SELECT-FOCUSED-ROW() NO-ERROR.
   {muswait.i}
   vart = "AND".
   FIND FIRST okost WHERE RECID(okost) = okostlon.OKOSTREC NO-LOCK NO-ERROR.
   ovrec = RECID(okost).
   ovrec2 = ovrec.
   RUN ANDOKO.W (INPUT pkod, INPUT okost.AONR,INPUT okost.DELNR).
   FIND FIRST okostlon WHERE okostlon.OKOSTREC = ovrec NO-LOCK NO-ERROR.
   FIND FIRST okost WHERE RECID(okost) = okostlon.OKOSTREC NO-LOCK NO-ERROR.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod
   NO-LOCK NO-ERROR. 
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.  
   FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND                   
   lontilltemp.LONTILLAGG = okost.LONTILLAGG NO-LOCK.
   IF AVAILABLE lontilltemp THEN DO:
      BUFFER-COPY okost TO okostlon.
      ASSIGN
      okostlon.VILART = lontilltemp.VILART
      okostlon.KOD = lontilltemp.KOD.      
   END.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   status-ok = BRW_OKOST:SELECT-FOCUSED-ROW() NO-ERROR.
   {musarrow.i}
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_REG DIALOG-1
ON CHOOSE OF FBTN_REG IN FRAME DIALOG-1 /* Fortsätt */
DO:
   {muswait.i}         
   ASSIGN
   musz = FALSE. 
   {BORTBRWPROC.I}
   RETURN.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OKOST
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


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
   BTN_MALL:LABEL IN FRAME {&FRAME-NAME} = "  Personligt~nutlägg mall".
   RUN anst_UI.
   FIND FIRST okost NO-LOCK NO-ERROR.
   IF NOT AVAILABLE okost THEN musz = musz.              
   ELSE DO: 
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod
      NO-LOCK NO-ERROR. 
      FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.  
      FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND                   
      lontilltemp.LONTILLAGG = okost.LONTILLAGG NO-LOCK.
      IF AVAILABLE lontilltemp THEN DO:
         CREATE okostlon.
         BUFFER-COPY okost TO okostlon.
         ASSIGN
         okostlon.VILART = lontilltemp.VILART
         okostlon.KOD = lontilltemp.KOD
         okostlon.OKOSTREC = RECID(okost).
      END.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
   END.
   RUN enable_UI.       
   {FRMSIZED.I}      
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 30,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 30,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   IF sok2 = 0 THEN DO:
      LEAVE MAIN-BLOCK.
   END.    
   IF tjan = "RES" THEN DO:   
      FIND FIRST okostlon NO-LOCK NO-ERROR.
      IF NOT AVAILABLE okostlon THEN DO:
         ASSIGN
         BRW_OKOST:HIDDEN = TRUE
         BTN_UPP:HIDDEN = TRUE 
         BTN_MALL:HIDDEN = TRUE
         BTN_BORT:HIDDEN = TRUE.
      END.
   END.
   ELSE DO:
      FIND FIRST okostlon NO-LOCK NO-ERROR.
      IF NOT AVAILABLE okostlon THEN DO:
         ASSIGN
         BRW_OKOST:HIDDEN = TRUE
         BTN_UPP:HIDDEN = TRUE 
         BTN_MALL:HIDDEN = TRUE
         BTN_BORT:HIDDEN = TRUE.
      END.
   END.
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN.
   ELSE BTN_MALL:HIDDEN = TRUE.
   {musarrow.i}
   APPLY "ENTRY" TO FILL-IN-RUBRIK IN FRAME {&FRAME-NAME}.
   {DIA_M_SLUT.I}
   DISPLAY BTN_MALL WITH FRAME {&FRAME-NAME}. 
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN.
   ELSE BTN_MALL:HIDDEN = TRUE.
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_MALL:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_MALL:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   BTN_MALL:HIDDEN = TRUE.
   
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
   okostlon.DATUM:READ-ONLY IN BROWSE BRW_OKOST = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_OKOST:HANDLE IN FRAME {&FRAME-NAME}).               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anst_UI DIALOG-1 
PROCEDURE anst_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod
   NO-LOCK NO-ERROR. 
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.       
   FIND FIRST utryckningtemp WHERE  utryckningtemp.KOD = ansttemp.KOD
   USE-INDEX UT NO-LOCK NO-ERROR.     
   ASSIGN  
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
   FILL-IN-PKOD = personaltemp.PERSONALKOD
   FILL-IN-RUBRIK = "Övriga ersättningar".
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN LONKODHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE lontilltemp,OUTPUT TABLE lonkorttemp).
   END.
   ELSE DO:
      RUN LONKODHMT.P  
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE lontilltemp,OUTPUT TABLE lonkorttemp).
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
  DISPLAY FILL-IN-RUBRIK FILL-IN-PKOD FILL-IN_FORNAMN-2 
      WITH FRAME DIALOG-1.
  ENABLE BRW_OKOST BTN_MALL BTN_NY BTN_UPP BTN_BORT FBTN_REG BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyskap_UI DIALOG-1 
PROCEDURE nyskap_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   ASSIGN
   vart = "NYA".  
   RUN nytid_UI.
   RUN ANDOKO.W (INPUT pkod, INPUT okost.AONR,INPUT okost.DELNR).
   IF musz = FALSE THEN DO:     
      BRW_OKOST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ENABLE BRW_OKOST WITH FRAME {&FRAME-NAME}.
      ENABLE BTN_BORT BTN_UPP WITH FRAME {&FRAME-NAME}. 
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
         ENABLE BTN_MALL WITH FRAME {&FRAME-NAME}.
      END.
      ELSE IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN DO:
         ENABLE BTN_MALL WITH FRAME {&FRAME-NAME}.
      END.
      FIND okost WHERE RECID(okost) = ovrec NO-LOCK NO-ERROR.
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod
      NO-LOCK NO-ERROR. 
      FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.   
      FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND                   
      lontilltemp.LONTILLAGG = okost.LONTILLAGG NO-LOCK.
      IF AVAILABLE lontilltemp THEN DO:
         CREATE okostlon.
         BUFFER-COPY okost TO okostlon.
         ASSIGN
         okostlon.VILART = lontilltemp.VILART
         okostlon.KOD = lontilltemp.KOD
         okostlon.OKOSTREC = RECID(okost).
      END.      
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(okostlon)).
      RUN lastselectdyn_UI IN brwproc[1].
   END.
   IF musz = TRUE THEN DO TRANSACTION:                 
      FIND okost WHERE RECID(okost) = ovrec EXCLUSIVE-LOCK NO-ERROR.   
      DELETE okost.
      /*FIND okost WHERE RECID(okost) = ovrec2 NO-LOCK NO-ERROR.*/
      FIND LAST okost NO-LOCK NO-ERROR.
      IF NOT AVAILABLE okost THEN DO:   
         DISABLE BTN_BORT BTN_UPP BTN_MALL WITH FRAME {&FRAME-NAME}. 
         BRW_OKOST:HIDDEN = TRUE.       
      END.
      ELSE DO:         
         ovrec = ovrec2.
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         FIND FIRST okostlon WHERE okostlon.OKOSTREC = ovrec NO-LOCK NO-ERROR.
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(okostlon)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
      APPLY "ENTRY" TO FILL-IN-RUBRIK IN FRAME {&FRAME-NAME}.
   END.
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytid_UI DIALOG-1 
PROCEDURE nytid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF vart = "NYA" THEN DO:  
      CREATE okost. 
      ovrec = RECID(okost).
      ASSIGN 
      okost.PERSONALKOD = personaltemp.PERSONALKOD
      okost.DAG = regdagnamn
      okost.VECKONUMMER = regvnr
      okost.DATUM = regdatum
      okost.AONR = varaonr
      okost.DELNR = vardelnr.          
   END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

