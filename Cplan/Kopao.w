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
DEFINE INPUT PARAMETER plannrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artalvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER delvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}

{AONRDEF.I}
/* {AONRTEMP.I} */
&Scoped-define SHARED SHARED
{AVTPLANTEMP.I}
{PLANNRTEMP.I}
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrec AS RECID NO-UNDO.

DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandovar AS CHARACTER NO-UNDO.
DEFINE VARIABLE datumvar AS DATE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES aonrtemp

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR aonrtemp.OMRADE aonrtemp.AONR ~
aonrtemp.DELNR aonrtemp.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR aonrtemp.AONR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AONR aonrtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AONR aonrtemp
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH aonrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH aonrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AONR aonrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR aonrtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 FILL-IN_AONR FILL-IN_DELNR RAD_FAST ~
FILL-IN_AOSOK FILL-IN_ORT2 BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_PLANNR FILL-IN_ARTAL FILL-IN_ORT ~
FILL-IN_AONR FILL-IN_DELNR RAD_FAST FILL-IN_AOSOK FILL-IN_ORT2 

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

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1.

DEFINE VARIABLE FILL-IN_AOSOK AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 9.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_ARTAL AS INTEGER FORMAT "9999" INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "Ort/Benämning" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN_ORT2 AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_PLANNR AS CHARACTER FORMAT "X(6)" 
     LABEL "Plannr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Det finns inga aonr att visa!" 
     VIEW-AS FILL-IN 
     SIZE 30.5 BY 1
     FONT 17 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 44.25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 2.42
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      aonrtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR DIALOG-1 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      aonrtemp.OMRADE FORMAT "X(6)":U
      aonrtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      aonrtemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      aonrtemp.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      aonrtemp.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 57 BY 8.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_PLANNR AT ROW 1.54 COL 14.5 COLON-ALIGNED
     FILL-IN_ARTAL AT ROW 1.54 COL 30 COLON-ALIGNED
     FILL-IN_ORT AT ROW 3.04 COL 14.5 COLON-ALIGNED
     FILL-IN_AONR AT ROW 4.54 COL 14.5 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 6 COL 14.5 COLON-ALIGNED
     RAD_FAST AT ROW 7.42 COL 1.5 NO-LABEL
     BRW_AONR AT ROW 8.83 COL 1.5
     FILL-IN_TEXT AT ROW 11.75 COL 22 COLON-ALIGNED NO-LABEL
     FILL-IN_AOSOK AT ROW 17.88 COL 25.63 COLON-ALIGNED
     FILL-IN_ORT2 AT ROW 18.92 COL 25.63 COLON-ALIGNED
     BTN_REG AT ROW 20.63 COL 29.5
     BTN_AVS AT ROW 20.63 COL 44.5
     "Sök på:" VIEW-AS TEXT
          SIZE 8.88 BY .63 AT ROW 17.88 COL 2.63
     RECT-22 AT ROW 17.67 COL 1.5
     SPACE(1.24) SKIP(1.57)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Koppla aonr till plan":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: aonrtemp T "?" NO-UNDO temp-db aonrtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AONR RAD_FAST DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000
       BRW_AONR:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ARTAL IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_ARTAL:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ORT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_PLANNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_PLANNR:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_TEXT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_TEXT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-1
   SHARED                                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.aonrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.aonrtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.aonrtemp.AONR
"aonrtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.aonrtemp.DELNR
"aonrtemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.aonrtemp.ORT
"aonrtemp.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
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
ON END-ERROR OF FRAME DIALOG-1 /* Koppla aonr till plan */
DO:
   {BORTBRWPROC.I}
   RETURN.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Koppla aonr till plan */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR DIALOG-1
ON VALUE-CHANGED OF BRW_AONR IN FRAME DIALOG-1
DO:
   status-ok = BRW_AONR:SELECT-FOCUSED-ROW().
   ASSIGN 
   FILL-IN_AONR = aonrtemp.AONR
   FILL-IN_DELNR = aonrtemp.DELNR.
   DISPLAY FILL-IN_AONR WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i}  
   FILL-IN_AONR = INPUT FILL-IN_AONR.  
   FILL-IN_DELNR = INPUT FILL-IN_DELNR.
   IF FILL-IN_AONR NE "" THEN DO TRANSACTION:      
      FIND FIRST aonrtemp WHERE 
      aonrtemp.AONR = FILL-IN_AONR AND aonrtemp.DELNR = FILL-IN_DELNR AND 
      aonrtemp.PLANNR = ? NO-ERROR.
      IF NOT AVAILABLE aonrtemp THEN DO:
         MESSAGE 
         "Det finns inget " + LC(Guru.Konstanter:gaok) + " med nummer:" + FILL-IN_AONR +
         " och delnr:" + STRING(FILL-IN_DELNR,Guru.Konstanter:varforetypchar[1]) VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.     
      ELSE DO:
         ASSIGN
         aonrvar = aonrtemp.AONR
         delvar = aonrtemp.DELNR.
         FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND 
         upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
         musz = FALSE.
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN KOPAOAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
             (INPUT aonrvar,INPUT delvar,
             INPUT plannrvar,INPUT artalvar,
             OUTPUT TABLE felmeddtemp).
         END.
         ELSE DO:
            RUN KOPAOAPP.P 
             (INPUT aonrvar,INPUT delvar,
             INPUT plannrvar,INPUT artalvar,
             OUTPUT TABLE felmeddtemp).    
         END.
         FIND FIRST felmeddtemp NO-ERROR.
         IF AVAILABLE felmeddtemp THEN musz = TRUE.
         FOR EACH felmeddtemp:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.
         END.
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RETURN NO-APPLY.
         END.        
         FIND FIRST aonrtemp WHERE aonrtemp.AONR = aonrvar AND aonrtemp.DELNR = delvar NO-LOCK NO-ERROR.
         ASSIGN 
         aonrtemp.PLANNR = plannrvar 
         aonrtemp.ARTAL = artalvar. 
         FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND 
         upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
         ASSIGN 
         upplantemp.AONR = aonrvar
         upplantemp.DELNR = delvar
         upplantemp.KOPPAO = TRUE.           
      END.             
   END.   
   ELSE DO:
      MESSAGE "Felaktigt " + LC(Guru.Konstanter:gaok) + "!" VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.    
   END.   
   {musarrow.i} 
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {BORTBRWPROC.I}
   RETURN.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AOSOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AOSOK DIALOG-1
ON ANY-KEY OF FILL-IN_AOSOK IN FRAME DIALOG-1 /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_AOSOK IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AOSOK DIALOG-1
ON LEAVE OF FILL-IN_AOSOK IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AOSOK = INPUT FILL-IN_AOSOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AOSOK DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_AOSOK IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AOSOK = INPUT FILL-IN_AOSOK.
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "AONR", INPUT FILL-IN_AOSOK).
   APPLY "VALUE-CHANGED" TO BRW_AONR IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORT2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT2 DIALOG-1
ON ANY-KEY OF FILL-IN_ORT2 IN FRAME DIALOG-1 /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_ORT2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT2 DIALOG-1
ON LEAVE OF FILL-IN_ORT2 IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_ORT2 = INPUT FILL-IN_ORT2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT2 DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_ORT2 IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_ORT2 = INPUT FILL-IN_ORT2.
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "ORT", INPUT FILL-IN_ORT2).
   APPLY "VALUE-CHANGED" TO BRW_AONR IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST DIALOG-1
ON VALUE-CHANGED OF RAD_FAST IN FRAME DIALOG-1
DO:              
   RAD_FAST = INPUT RAD_FAST.   
   RUN fastaao_UI.   
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
   FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND
   upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
   
/*    FIND FIRST PLANNRTAB WHERE PLANNRTAB.PLANNR = plannrvar AND */
/*    PLANNRTAB.ARTAL = artalvar EXCLUSIVE-LOCK NO-ERROR.         */
/*    FIND PLANNRTAB WHERE RECID(PLANNRTAB) = aonrrec NO-LOCK NO-ERROR. */
   ASSIGN 
   omrvar = upplantemp.OMRADE
   RAD_FAST = FALSE
   FILL-IN_DELNR = 000.
   {TILLFAST.I}   
   FIND FIRST aonrtemp WHERE aonrtemp.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE aonrtemp THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   RUN grundtid2_UI.  
/*    &Scoped-define FORMATNAMN AONRTAB.AONR */
   &Scoped-define FORMATNAMN aonrtemp.AONR
   &Scoped-define BROWSE-NAME BRW_AONR
   {AOFORMAT1.I}
   &Scoped-define FORMATNAMN FILL-IN_AOSOK   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   ASSIGN FRAME {&FRAME-NAME}:TITLE = "Koppla " + LC(Guru.Konstanter:gaok) + " till plan".
   ASSIGN
   FILL-IN_ORT:LABEL = Guru.Konstanter:gaonamnk
   aonrtemp.AONR:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gaok 
   FILL-IN_AOSOK:LABEL = Guru.Konstanter:gaok 
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok
   FILL-IN_PLANNR:LABEL = Guru.Konstanter:gplk.      
   RUN enable_UI.       
   {FRMSIZED.I}
   DISPLAY FILL-IN_TEXT WITH FRAME {&FRAME-NAME}.
   ASSIGN
   FILL-IN_TEXT:HIDDEN = TRUE
   BRW_AONR:HIDDEN = FALSE.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN PLANAOHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 1,INPUT omrvar,OUTPUT TABLE aonrtemp).
   END.
   ELSE DO:
      RUN PLANAOHMT.P 
      (INPUT 1,INPUT omrvar,OUTPUT TABLE aonrtemp).                 
   END.
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   RUN fastaao_UI.   
   
   {musarrow.i}  
   my1hand = FILL-IN_AONR:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BRW_AONR:MOVE-AFTER-TAB-ITEM(my1hand).
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
   aonrtemp.AONR:READ-ONLY IN BROWSE BRW_AONR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).
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
  DISPLAY FILL-IN_PLANNR FILL-IN_ARTAL FILL-IN_ORT FILL-IN_AONR FILL-IN_DELNR 
          RAD_FAST FILL-IN_AOSOK FILL-IN_ORT2 
      WITH FRAME DIALOG-1.
  ENABLE RECT-22 FILL-IN_AONR FILL-IN_DELNR RAD_FAST FILL-IN_AOSOK FILL-IN_ORT2 
         BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fastaao_UI DIALOG-1 
PROCEDURE fastaao_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   ASSIGN datumvar = 01/01/1991.
   IF RAD_FAST = TRUE THEN DO:
      kommandovar = " WHERE aonrtemp.OMRADE = """ + STRING(omrvar) + """ 
      AND aonrtemp.PLANNR = ? AND aonrtemp.FASTAAONR = TRUE AND aonrtemp.AONRAVDATUM = " + STRING(datumvar) + " ".
   END.
   ELSE DO:
      kommandovar = " WHERE aonrtemp.OMRADE = """ + STRING(omrvar) + """ 
      AND aonrtemp.PLANNR = ? AND aonrtemp.FASTAAONR = FALSE AND aonrtemp.AONRAVDATUM = " + STRING(datumvar) + " ".
   END.
   RUN openbdyn_UI IN brwproc[1] (INPUT kommandovar).  
   FIND FIRST aonrtemp WHERE aonrtemp.OMRADE = omrvar AND aonrtemp.FASTAAONR = RAD_FAST AND
   aonrtemp.AONRAVDATUM = 01/01/1991 AND aonrtemp.PLANNR = ?
   USE-INDEX OMRADE NO-LOCK NO-ERROR.
   IF AVAILABLE aonrtemp THEN DO:
      ASSIGN
      FILL-IN_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_AONR:HIDDEN = FALSE.
      BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
      ENABLE FILL-IN_AOSOK FILL-IN_ORT2 WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_AONR:HIDDEN = TRUE.
      DISABLE FILL-IN_AOSOK FILL-IN_ORT2
      WITH FRAME {&FRAME-NAME}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid2_UI DIALOG-1 
PROCEDURE grundtid2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/
  ASSIGN
  FILL-IN_PLANNR = upplantemp.PLANNR
  FILL-IN_ARTAL = upplantemp.ARTAL  
  FILL-IN_ORT = upplantemp.ORT.          
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
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

