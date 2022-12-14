&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : _bldfld.w
    Purpose     : This procedure builds a set of _tt-fld temp-table records
                  with the names of the fields that can be selected for a given
                  temp-table. The name of the actual (the real db table) is in
                  p_table and the _tt-tbl record that is the parent of the
                  _tt-fld records created in this procedure has a RECID of
                  p_tt-rec.

    Syntax      : RUN adecomm/_bldfld.w (INPUT p_tt-rec, INPUT p_table).

    Description :

    Author(s)   : D. Ross Hunter
    Created     : 11/05/96
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER p_tt-rec AS RECID                            NO-UNDO.
DEFINE INPUT PARAMETER p_table  AS CHARACTER                        NO-UNDO.

{adecomm/tt-brws.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND DICTDB._File "_Field":U NO-LOCK.
IF NOT CAN-DO(DICTDB._File._Can-Read, USERID("DICTDB":U)) THEN DO:
  MESSAGE "You do not have permission to see any field information.":t
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

FIND DICTDB._File WHERE DICTDB._FILE._FILE-NAME = p_table NO-LOCK.
FOR EACH _FIELD OF DICTDB._FILE NO-LOCK:
  DO TRANSACTION:
    CREATE _tt-fld.
    ASSIGN _tt-fld.tt-fld   = _FIELD._FIELD-NAME
           _tt-fld.tt-recid = p_tt-rec.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


