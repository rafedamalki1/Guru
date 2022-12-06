&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER tblName AS CHARACTER.

/* This com-handle is defined as shared in order to use it 
 * the generated code: temp.p 
 */   
DEFINE NEW SHARED VARIABLE chFrmListView AS COM-HANDLE.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CFListView AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFListView AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_Cancel AT ROW 15.05 COL 33
     SPACE(34.39) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE ""
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CFListView ASSIGN
       FRAME        = FRAME Dialog-Frame:HANDLE
       ROW          = 1.24
       COLUMN       = 2
       HEIGHT       = 13.57
       WIDTH        = 78
       HIDDEN       = no
       SENSITIVE    = yes.
      CFListView:NAME = "CFListView":U .
/* CFListView OCXINFO:CREATE-CONTROL from: {58DA8D8A-9D6A-101B-AFC0-4210102A8DA7} type: ListView */
      CFListView:MOVE-BEFORE(Btn_Cancel:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  ASSIGN 
    FRAME Dialog-Frame:TITLE = "Contents of " + tblName + " table.".

  RUN enable_UI.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "dTable.wrx":U ).

IF OCXFile <> ? THEN DO:
  ASSIGN
    chCFListView = CFListView:COM-HANDLE
    UIB_S = chCFListView:LoadControls( OCXFile, "CFListView":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "The file, dTable.wrx, could not be found." skip
             "The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  RUN control_load.
  ENABLE Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls Dialog-Frame 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE colClmnHeadrs AS COM-HANDLE.
DEFINE VARIABLE chLstItem AS COM-HANDLE.
DEFINE VARIABLE i AS INTEGER INIT 1.
DEFINE VARIABLE exCount AS INTEGER INIT 0.
DEFINE VARIABLE exTotal AS INTEGER INIT 0.

chFrmListView = CFListView:COM-HANDLE.
chFrmListView:ListView:View = 3.  /* Report */
chFrmListView:ListView:LabelEdit = 1. /* Manual: Only first Item in first Column 
                                         is editable, thus disable it */
colClmnHeadrs = chFrmListView:ListView:ColumnHeaders.

/* *** Generic way to loop through fields and records of a specific database table.
    This requires us to generate a .p (temp.p) that will fill the ListView 
    report for the current table.
*** */
  
IF tblName <> "Customer" THEN DO:
    FIND _file WHERE _file-name = tblName. 
    FOR EACH _field OF _file :
        IF _field._extent > 0 THEN DO:
            excount = _field._extent.
            DO WHILE exCount <> 0:
               colClmnHeadrs:Add(,,_field-name + "[" + String((_field._extent - (exCount - 1))) + "]",,).
               exCount = exCount - 1.
            END.
        END. 
        ELSE 
            colClmnHeadrs:Add(,,_field-name,,).
             
        IF i = 1 THEN DO:
            OUTPUT TO "temp.p".
            PUT UNFORMATTED "DEFINE SHARED VARIABLE chFrmListView AS COM-HANDLE." SKIP(1).
            PUT UNFORMATTED "DEFINE VARIABLE chListViewT AS COM-HANDLE." SKIP(1).
            PUT UNFORMATTED "FOR EACH " tblName ":" SKIP(1).
            IF _field._extent > 0 THEN DO:
                PUT UNFORMATTED "chListViewT=chFrmListView:ListView:ListItems:Add(,," tblName "." _field-name "[1],,)." SKIP(1).
                exCount = 1.
                DO WHILE exCount <> _field._extent:
                    PUT UNFORMATTED "chListViewT:SubItems(" exCount ") = " tblName "." _field-name "[" + String(exCount + 1) + "]." SKIP(1). 
                    exCount = exCount + 1.
                END.  
            END.
            ELSE 
                PUT UNFORMATTED "chListViewT=chFrmListView:ListView:ListItems:Add(,," tblName "." _field-name ",,)." SKIP(1).
            OUTPUT CLOSE.
        END.
               
        IF i <> 1 THEN DO:
            OUTPUT TO "temp.p" APPEND.
            IF exCount > 1 THEN DO:
                PUT UNFORMATTED "chListViewT:SubItems(" exCount ") = " tblName "." _field-name "." SKIP(1). 
                exCount = exCount + 1.
            END.
            ELSE
                PUT UNFORMATTED "chListViewT:SubItems(" (i - 1) ") = " tblName "." _field-name " NO-ERROR." SKIP(1).  
                OUTPUT CLOSE.
            END.
            i = i + 1.
        END.
        
    OUTPUT TO "temp.p" APPEND. 
    PUT UNFORMATTED "END." SKIP(1).
    PUT UNFORMATTED "RELEASE OBJECT chListViewT." SKIP(1).
    OUTPUT CLOSE.
    RUN temp.p.
END.

/* ----- This is specific for the Customer table, just for illustration ----- */
ELSE DO:
    colClmnHeadrs:Add(,,"#..",15,).
    colClmnHeadrs:Add(,,"Name",,). 
    colClmnHeadrs:Add(,,"Address",,).
    colClmnHeadrs:Add(,,"Address2",,).
    colClmnHeadrs:Add(,,"City",,).
    colClmnHeadrs:Add(,,"State",,).
    colClmnHeadrs:Add(,,"Country",,).
    colClmnHeadrs:Add(,,"Postal-Code",,).
    colClmnHeadrs:Add(,,"Phone",,).
    colClmnHeadrs:Add(,,"Contact",,).
    colClmnHeadrs:Add(,,"Credit-Limit",,).
    colClmnHeadrs:Add(,,"Discount",,).
    colClmnHeadrs:Add(,,"Sales-Rep",,).
    colClmnHeadrs:Add(,,"Terms",,).
    colClmnHeadrs:Add(,,"Comments",,).

    FOR EACH Customer:
        chLstItem = chFrmListView:ListView:ListItems:Add(,,Customer.Cust-Num,,).
        chLstItem:SubItems(1) = Customer.Name.
        chLstItem:SubItems(2) = Customer.Address.
        chLstItem:SubItems(3) = Customer.Address2.
        chLstItem:SubItems(4) = Customer.City.
        chLstItem:SubItems(5) = Customer.State.
        chLstItem:SubItems(6) = Customer.Country.
        chLstItem:SubItems(7) = Customer.Postal-Code.
        chLstItem:SubItems(8) = Customer.Phone.
        chLstItem:SubItems(9) = Customer.Contact.
        chLstItem:SubItems(10) = Customer.Credit-Limit.
        chLstItem:SubItems(11) = Customer.Discount.
        chLstItem:SubItems(12) = Customer.Sales-Rep.
        chLstItem:SubItems(13) = Customer.Terms.
        chLstItem:SubItems(14) = Customer.Comments.
        RELEASE OBJECT chLstItem.
    END.
END.

RELEASE OBJECT colClmnHeadrs.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


