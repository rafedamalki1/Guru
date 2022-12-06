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

  Created: 09/23/97 - 10:48 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&Scoped-define NEW

&Scoped-define SHARED SHARED
{KONID.I}    
{LISTMTRL.I} 
{KONID2.I}    
{LEVTEMP.I}
DEFINE SHARED VARIABLE franrowid AS ROWID NO-UNDO.      
DEFINE SHARED VARIABLE tillrowid AS ROWID NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
   
DEFINE TEMP-TABLE lin_tab
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER LABEL "Benämning" 
   FIELD PRIS AS DECIMAL
   FIELD ENHET AS CHARACTER
   FIELD ENHET2 AS CHARACTER
   FIELD METER  AS INTEGER
   FIELD LEVKOD AS CHARACTER
   FIELD DIAMETER  AS INTEGER. 
   
DEFINE TEMP-TABLE lin_tab2 NO-UNDO LIKE lin_tab.
   
DEFINE BUFFER idbuff FOR kon_id.  
DEFINE BUFFER idbuff2 FOR kon_id2.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_KAB1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES lin_tab lin_tab2

/* Definitions for BROWSE BRW_KAB1                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KAB1 lin_tab.ENR lin_tab.BENAMNING ~
lin_tab.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KAB1 
&Scoped-define QUERY-STRING-BRW_KAB1 FOR EACH lin_tab NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KAB1 OPEN QUERY BRW_KAB1 FOR EACH lin_tab NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KAB1 lin_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KAB1 lin_tab


/* Definitions for BROWSE BRW_KAB2                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KAB2 lin_tab2.ENR lin_tab2.BENAMNING ~
lin_tab2.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KAB2 
&Scoped-define QUERY-STRING-BRW_KAB2 FOR EACH lin_tab2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KAB2 OPEN QUERY BRW_KAB2 FOR EACH lin_tab2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KAB2 lin_tab2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KAB2 lin_tab2


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_KAB1 BRW_KAB2 BTN_FRAN BTN_TILL BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_FRAN 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "Btn 1" 
     SIZE 5.13 BY 1.71.

DEFINE BUTTON BTN_TILL 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "Btn 1" 
     SIZE 5.13 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KAB1 FOR 
      lin_tab SCROLLING.

DEFINE QUERY BRW_KAB2 FOR 
      lin_tab2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KAB1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KAB1 DIALOG-1 _STRUCTURED
  QUERY BRW_KAB1 NO-LOCK DISPLAY
      lin_tab.ENR FORMAT "X(11)":U
      lin_tab.BENAMNING FORMAT "x(25)":U
      lin_tab.ENHET COLUMN-LABEL "Leverantör" FORMAT "x(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 54.75 BY 25
         TITLE "Från".

DEFINE BROWSE BRW_KAB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KAB2 DIALOG-1 _STRUCTURED
  QUERY BRW_KAB2 NO-LOCK DISPLAY
      lin_tab2.ENR FORMAT "X(11)":U
      lin_tab2.BENAMNING FORMAT "x(25)":U
      lin_tab2.ENHET COLUMN-LABEL "Leverantör" FORMAT "x(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 55 BY 25
         TITLE "Till".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_KAB1 AT ROW 1.5 COL 1.5
     BRW_KAB2 AT ROW 1.5 COL 63
     BTN_FRAN AT ROW 11.88 COL 57
     BTN_TILL AT ROW 15.21 COL 57
     BTN_AVB AT ROW 27.75 COL 104
     SPACE(1.12) SKIP(0.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Linor och kablar kopplade till resp. konstruktion".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db lin_tab
      TABLE: ? T "?" NO-UNDO temp-db lin_tab2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_KAB1 1 DIALOG-1 */
/* BROWSE-TAB BRW_KAB2 BRW_KAB1 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KAB1
/* Query rebuild information for BROWSE BRW_KAB1
     _TblList          = "Temp-Tables.lin_tab"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.lin_tab.ENR
     _FldNameList[2]   > Temp-Tables.lin_tab.BENAMNING
"lin_tab.BENAMNING" ? "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.lin_tab.ENHET
"lin_tab.ENHET" "Leverantör" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KAB1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KAB2
/* Query rebuild information for BROWSE BRW_KAB2
     _TblList          = "Temp-Tables.lin_tab2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.lin_tab2.ENR
     _FldNameList[2]   > Temp-Tables.lin_tab2.BENAMNING
"lin_tab2.BENAMNING" ? "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.lin_tab2.ENHET
"lin_tab2.ENHET" "Leverantör" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KAB2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Linor och kablar kopplade till resp. konstruktion */
DO:
  {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Linor och kablar kopplade till resp. konstruktion */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KAB1
&Scoped-define SELF-NAME BRW_KAB1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KAB1 DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_KAB1 IN FRAME DIALOG-1 /* Från */
DO:
    APPLY "CHOOSE" TO BTN_TILL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KAB1 DIALOG-1
ON VALUE-CHANGED OF BRW_KAB1 IN FRAME DIALOG-1 /* Från */
DO:
   status-ok = BRW_KAB1:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KAB2
&Scoped-define SELF-NAME BRW_KAB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KAB2 DIALOG-1
ON VALUE-CHANGED OF BRW_KAB2 IN FRAME DIALOG-1 /* Till */
DO:
   status-ok = BRW_KAB2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avsluta */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FRAN DIALOG-1
ON CHOOSE OF BTN_FRAN IN FRAME DIALOG-1 /* Btn 1 */
DO:   
   FIND FIRST kon_id2 WHERE ROWID(kon_id2) = tillrowid NO-LOCK NO-ERROR.
   FIND FIRST list_mtrl WHERE list_mtrl.NUM = kon_id2.NUM AND 
   list_mtrl.ENR = lin_tab.ENR AND list_mtrl.LEVKOD = lin_tab.LEVKOD
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE list_mtrl THEN DO:
      CREATE list_mtrl.   
      ASSIGN
      list_mtrl.NUM = kon_id2.NUM       
      list_mtrl.ENR = lin_tab.ENR 
      list_mtrl.BENAMNING = lin_tab.BENAMNING 
      list_mtrl.LEVKOD = lin_tab.LEVKOD 
      list_mtrl.PRIS = lin_tab.PRIS
      list_mtrl.ENHET = LC(lin_tab.ENHET2)
      list_mtrl.ANTAL = 0
      list_mtrl.LINKAB = TRUE
      list_mtrl.DIAMETER = lin_tab.DIAMETER.   
      CREATE lin_tab2.
      ASSIGN  
      lin_tab2.ENR = lin_tab.ENR 
      lin_tab2.BENAMNING = lin_tab.BENAMNING 
      lin_tab2.LEVKOD = lin_tab.LEVKOD 
      lin_tab2.PRIS = lin_tab.PRIS
      lin_tab2.ENHET2 = lin_tab.ENHET2
      lin_tab2.ENHET = lin_tab.ENHET
      lin_tab2.DIAMETER = lin_tab.DIAMETER.                 
   END.   
   ELSE DO:
      IF list_mtrl.LINKAB = FALSE THEN DO:
         ASSIGN
         list_mtrl.LINKAB = TRUE
         list_mtrl.DIAMETER = lin_tab.DIAMETER.
         CREATE lin_tab2.
         ASSIGN  
         lin_tab2.ENR = lin_tab.ENR 
         lin_tab2.BENAMNING = lin_tab.BENAMNING 
         lin_tab2.LEVKOD = lin_tab.LEVKOD 
         lin_tab2.PRIS = lin_tab.PRIS
         lin_tab2.ENHET2 = lin_tab.ENHET2
         lin_tab2.ENHET = lin_tab.ENHET
         lin_tab2.DIAMETER = lin_tab.DIAMETER.
      END.
   END.   
   RUN openbdynspec_UI IN brwproc[2].   
   /*OPEN QUERY BRW_KAB2 FOR EACH lin_tab2 NO-LOCK BY lin_tab2.ENR.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TILL DIALOG-1
ON CHOOSE OF BTN_TILL IN FRAME DIALOG-1 /* Btn 1 */
DO:
   FIND FIRST kon_id WHERE ROWID(kon_id) = franrowid NO-LOCK NO-ERROR.
   FIND FIRST list_mtrl WHERE list_mtrl.NUM = kon_id.NUM AND 
   list_mtrl.ENR = lin_tab2.ENR AND list_mtrl.LEVKOD = lin_tab2.LEVKOD
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE list_mtrl THEN DO:
      CREATE list_mtrl. 
      ASSIGN
      list_mtrl.NUM = kon_id.NUM       
      list_mtrl.ENR = lin_tab2.ENR 
      list_mtrl.BENAMNING = lin_tab2.BENAMNING 
      list_mtrl.LEVKOD = lin_tab2.LEVKOD 
      list_mtrl.PRIS = lin_tab2.PRIS
      list_mtrl.ENHET = LC(lin_tab2.ENHET2)
      list_mtrl.ANTAL = 0
      list_mtrl.LINKAB = TRUE
      list_mtrl.DIAMETER = lin_tab2.DIAMETER.  
      CREATE lin_tab.
      ASSIGN  
      lin_tab.ENR = lin_tab2.ENR 
      lin_tab.BENAMNING = lin_tab2.BENAMNING 
      lin_tab.LEVKOD = lin_tab2.LEVKOD 
      lin_tab.PRIS = lin_tab2.PRIS
      lin_tab.ENHET2 = lin_tab2.ENHET2
      lin_tab.ENHET = lin_tab2.ENHET
      lin_tab.DIAMETER = lin_tab2.DIAMETER.          
   END.   
   ELSE DO:
      IF list_mtrl.LINKAB = FALSE THEN DO:
         ASSIGN
         list_mtrl.LINKAB = TRUE
         list_mtrl.DIAMETER = lin_tab.DIAMETER.
         CREATE lin_tab.
         ASSIGN  
         lin_tab.ENR = lin_tab2.ENR 
         lin_tab.BENAMNING = lin_tab2.BENAMNING 
         lin_tab.LEVKOD = lin_tab2.LEVKOD 
         lin_tab.PRIS = lin_tab2.PRIS
         lin_tab.ENHET2 = lin_tab2.ENHET2
         lin_tab.ENHET = lin_tab2.ENHET
         lin_tab.DIAMETER = lin_tab2.DIAMETER.
      END.
   END.
   RUN openbdynspec_UI IN brwproc[1].
   /*OPEN QUERY BRW_KAB1 FOR EACH lin_tab NO-LOCK BY lin_tab.ENR.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KAB1
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
   RUN ladda_UI.
   RUN enable_UI.       
   {FRMSIZED.I} 
   RUN openbdynspec_UI IN brwproc[1].   
   RUN openbdynspec_UI IN brwproc[2].
   FIND FIRST kon_id2 WHERE ROWID(kon_id2) = tillrowid NO-LOCK NO-ERROR.
   FIND FIRST kon_id WHERE ROWID(kon_id) = franrowid NO-LOCK NO-ERROR.
   BRW_KAB1:TITLE = "Från: " + STRING(kon_id.FRI2).
   BRW_KAB2:TITLE = "Till: " + STRING(kon_id2.FRI2).
   /*OPEN QUERY BRW_KAB1 FOR EACH lin_tab NO-LOCK BY lin_tab.ENR.  
   OPEN QUERY BRW_KAB2 FOR EACH lin_tab2 NO-LOCK BY lin_tab2.ENR.*/
   {musarrow.i}
   lin_tab.ENR:LABEL IN BROWSE BRW_KAB1 = Guru.Konstanter:genk. 
   lin_tab2.ENR:LABEL IN BROWSE BRW_KAB2 = Guru.Konstanter:genk.
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
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_KAB1:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_KAB2:HANDLE IN FRAME {&FRAME-NAME}).            
   RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "ENR").   
   RUN setdefaultcolbyname_UI IN brwproc[2] (INPUT "ENR").   
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
  ENABLE BRW_KAB1 BRW_KAB2 BTN_FRAN BTN_TILL BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI DIALOG-1 
PROCEDURE ladda_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EMPTY TEMP-TABLE  lin_tab NO-ERROR.
   EMPTY TEMP-TABLE  lin_tab2 NO-ERROR.   
   FIND FIRST kon_id WHERE ROWID(kon_id) = franrowid NO-LOCK NO-ERROR.
   IF AVAILABLE kon_id THEN DO:  
      IF kon_id.XKORD = ? THEN DO:     
         FOR EACH list_mtrl WHERE list_mtrl.NUM = kon_id.NUM AND
         list_mtrl.LINKAB = TRUE NO-LOCK: 
            FIND FIRST lin_tab WHERE lin_tab.ENR = list_mtrl.ENR AND 
            lin_tab.LEVKOD = list_mtrl.LEVKOD NO-LOCK NO-ERROR.
            IF NOT AVAILABLE lin_tab THEN DO:
              RUN skapa_UI.
            END.   
         END.
      END.
      ELSE DO:
         FOR EACH idbuff WHERE idbuff.XKORD = kon_id.XKORD:
            FOR EACH list_mtrl WHERE list_mtrl.NUM = idbuff.NUM AND
            list_mtrl.LINKAB = TRUE NO-LOCK: 
               FIND FIRST lin_tab WHERE lin_tab.ENR = list_mtrl.ENR AND
               lin_tab.LEVKOD = list_mtrl.LEVKOD NO-LOCK NO-ERROR.
               IF NOT AVAILABLE lin_tab THEN DO:
                  RUN skapa_UI.
               END.   
            END.     
         END.
      END.     
   END.              
   FIND FIRST kon_id2 WHERE ROWID(kon_id2) = tillrowid NO-LOCK NO-ERROR.
   IF AVAILABLE kon_id2 THEN DO:   
      IF kon_id2.XKORD = ? THEN DO:     
         FOR EACH list_mtrl WHERE list_mtrl.NUM = kon_id2.NUM AND
         list_mtrl.LINKAB = TRUE NO-LOCK: 
            FIND FIRST lin_tab2 WHERE lin_tab2.ENR = list_mtrl.ENR AND 
            lin_tab2.LEVKOD = list_mtrl.LEVKOD NO-LOCK NO-ERROR.
            IF NOT AVAILABLE lin_tab2 THEN DO:
              RUN skapa2_UI.
            END.   
         END.
      END.
      ELSE DO:
         FOR EACH idbuff WHERE idbuff.XKORD = kon_id2.XKORD:
            FOR EACH list_mtrl WHERE list_mtrl.NUM = idbuff.NUM AND
            list_mtrl.LINKAB = TRUE NO-LOCK: 
               FIND FIRST lin_tab2 WHERE lin_tab2.ENR = list_mtrl.ENR AND
               lin_tab2.LEVKOD = list_mtrl.LEVKOD NO-LOCK NO-ERROR.
               IF NOT AVAILABLE lin_tab2 THEN DO:
                  RUN skapa2_UI.
               END.   
            END.     
         END.
      END.           
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapa2_UI DIALOG-1 
PROCEDURE skapa2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/          
    CREATE lin_tab2.
    ASSIGN
    lin_tab2.ENR = list_mtrl.ENR
    lin_tab2.BENAMNING = list_mtrl.BENAMNING
    lin_tab2.LEVKOD = list_mtrl.LEVKOD
    lin_tab2.PRIS = list_mtrl.PRIS
    lin_tab2.ENHET2 = list_mtrl.ENHET
    lin_tab2.DIAMETER = list_mtrl.DIAMETER. 
    FIND FIRST levtemp WHERE levtemp.LEVKOD = list_mtrl.LEVKOD NO-LOCK NO-ERROR.
    IF AVAILABLE levtemp THEN DO:        
       lin_tab2.ENHET = levtemp.LEVNAMN.
    END.
    ELSE DO:  
       lin_tab2.ENHET = "SAKNAS".
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapa_UI DIALOG-1 
PROCEDURE skapa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
    CREATE lin_tab.
    ASSIGN
    lin_tab.ENR = list_mtrl.ENR
    lin_tab.BENAMNING = list_mtrl.BENAMNING
    lin_tab.LEVKOD = list_mtrl.LEVKOD
    lin_tab.PRIS = list_mtrl.PRIS
    lin_tab.ENHET2 = list_mtrl.ENHET
    lin_tab.DIAMETER = list_mtrl.DIAMETER. 
    FIND FIRST levtemp WHERE levtemp.LEVKOD = list_mtrl.LEVKOD NO-LOCK NO-ERROR.
    IF AVAILABLE levtemp THEN DO:        
       lin_tab.ENHET = levtemp.LEVNAMN.
    END.
    ELSE DO:  
       lin_tab.ENHET = "SAKNAS".
    END.            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

