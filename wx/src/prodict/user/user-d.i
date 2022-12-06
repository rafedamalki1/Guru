/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Progress Lex Converter 7.1A->7.1B Version 1.11 */

/*
-------------------------------------------------------------------------------
DESCRIPTION:
------------
  Table Name: ______________________________         Hidden: ___
  Table Type: ______________________________         Frozen: ___
 Table Owner: ______________________________   Table Number: _____
   Dump Name: ________ (name for dump data)     Record Size: _____
       Label: ______________________________

 Name in DataServer Database:
              _______________________________________________________________
 Description: _______________________________________________________________
              _______________________________________________________________
              _______________________________________________________________
              _______________________________________________________________

          [^Validation] [^Triggers] [String Attributes] [^DataServer]
-------------------------------------------------------------------------------
*/

FORM
  wfil._File-name FORMAT "x(32)"    LABEL "  Table Name"
   /*
    VALIDATE(
      wfil._File-name <> ? AND wfil._File-name <> ""
        AND KEYWORD(wfil._File-name) = ?,
      IF wfil._File-name = ? OR wfil._File-name = ""
        THEN "The table name must not be blank or unknown."
        ELSE "This name conflicts with a PROGRESS reserved keyword.")
   */
  wfil._Hidden       FORMAT "yes/no" LABEL "        Hidden"  SKIP

  wfil._For-Type     FORMAT "x(32)"  LABEL "  Table Type"
  wfil._Frozen       FORMAT "yes/no" LABEL "        Frozen"  SKIP

  wfil._For-Owner    FORMAT "x(32)"  LABEL " Table Owner"
  wfil._File-number  FORMAT "->>>9"  LABEL "  Table Number"  SKIP

  wfil._File-label   FORMAT "x(32)"  LABEL "       Label"      
  wfil._Dump-name    FORMAT "x(8)"   LABEL "     Dump Name" SKIP

  wfil._Fil-misc2[6] FORMAT "X(32)"  LABEL " Replication"
  wfil._For-Size     FORMAT ">>>>9"  LABEL "   Record Size" SKIP
  
  "Name in DataServer  Database:"    VIEW-AS TEXT AT 2   
  wfil._For-Name     FORMAT "x(45)"  NO-LABEL 
    HELP "The name of the table in the DataServer's schema"  SKIP
 
  "Link to Distributed DataBase:"    VIEW-AS TEXT AT 2                                
  wfil._Fil-misc2[8] FORMAT "x(45)"  NO-LABEL                SKIP 
 
  wfil._Desc      VIEW-AS EDITOR
                               INNER-CHARS 63 INNER-LINES 3
                               BUFFER-LINES 4     LABEL " Description"    SKIP(1)

  button-v AT 12 SPACE(2) button-f SPACE(2) 
        button-s SPACE(2) button-d                           SKIP(1)
  btn_OK   AT 21 SPACE(2) btn_Cancel SPACE(5) btn_flds
  WITH FRAME frame-d
    /* need scrollable to compile cleanly on gui - though it's not run there */
    NO-BOX ATTR-SPACE SIDE-LABELS OVERLAY ROW 2 COLUMN 2 SCROLLABLE.


/*----- GO or OK -----*/
ON GO OF FRAME frame-d DO:
  RUN check_file_name (OUTPUT isbad).
  IF isbad THEN DO:
    ASSIGN
      go_field = no
      get-hit = no.
    RETURN NO-APPLY.
  END.

  RUN check_dump_name (OUTPUT isbad).
  IF isbad THEN DO:
    ASSIGN
      get-hit = no
      go_field = no.
    RETURN NO-APPLY.
  END.
END.


/*----- CHOOSE OF FIELD EDITOR BUTTON-----*/
ON CHOOSE OF btn_flds IN FRAME frame-d 
   go_field = yes.
   /* GO trigger will fire after this */


/*----- CHOOSE of SUB-DIALOG BUTTONS -----*/
ON CHOOSE OF button-v IN FRAME frame-d DO:
  RUN Tbl_Validation.
  MESSAGE COLOR NORMAL stdmsg.
END.

ON CHOOSE OF button-f IN FRAME frame-d DO:
  RUN Tbl_Triggers.
  MESSAGE COLOR NORMAL stdmsg.
END.

ON CHOOSE OF button-s IN FRAME frame-d DO:
  /* used for GUI too! */
  RUN Tbl_String_Attrs.
  MESSAGE COLOR NORMAL stdmsg.
END.

ON CHOOSE OF button-d IN FRAME frame-d DO:
  RUN prodict/gate/_gat_row.p 
    ( INPUT user_dbtype,
      INPUT dbkey
    ).
  MESSAGE COLOR NORMAL stdmsg.
END.


/*---- DEFAULT DUMP NAME -----*/
ON ENTRY OF wfil._Dump-name IN FRAME frame-d DO:
  IF SELF:SCREEN-VALUE = "?" OR SELF:SCREEN-VALUE = "" THEN
    DISPLAY LC(INPUT FRAME frame-d wfil._File-name)
      @ wfil._Dump-name WITH FRAME frame-d.
END.

/*----- HANDLE GET TO SWITCH TABLES -----*/
ON   GET OF wfil._File-name  IN FRAME frame-d
  OR GET OF wfil._For-Type   IN FRAME frame-d
  OR GET OF wfil._Hidden     IN FRAME frame-d
  OR GET OF wfil._Dump-name  IN FRAME frame-d
  OR GET OF wfil._File-label IN FRAME frame-d
  OR GET OF wfil._For-Size   IN FRAME frame-d
  OR GET OF wfil._Desc       IN FRAME frame-d 
  OR GET OF button-v         IN FRAME frame-d
  OR GET OF button-f         IN FRAME frame-d 
  OR GET OF button-s         IN FRAME frame-d 
  OR GET OF btn_OK           IN FRAME frame-d 
  OR GET OF btn_Cancel       IN FRAME frame-d 
  OR GET OF btn_flds         IN FRAME frame-d 
DO:
  IF NOT adding THEN DO:
    get-hit = TRUE.
    APPLY "GO" TO FRAME frame-d.
  END.
  RETURN NO-APPLY.
END.


/*------------------------Internal Procedures--------------------*/
PROCEDURE check_dump_name:
  DEFINE OUTPUT PARAMETER isbad AS LOGICAL INITIAL FALSE NO-UNDO.

  IF INPUT FRAME frame-d wfil._Dump-name = ?
    OR INPUT FRAME frame-d wfil._Dump-name = "" THEN
    DISPLAY LC(INPUT FRAME frame-d wfil._File-name)
      @ wfil._Dump-name WITH FRAME frame-d.
  FIND FIRST _File
    WHERE _File._Dump-name = INPUT FRAME frame-d wfil._Dump-name
      AND (adding OR RECID(_File) <> dbkey) NO-ERROR.
  IF AVAILABLE _File THEN DO:
    MESSAGE "That dump name is not unique within this database."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    isbad = TRUE.
    APPLY "ENTRY" TO wfil._Dump-name IN FRAME frame-d.
  END.
END PROCEDURE.


PROCEDURE check_file_name:
  DEFINE OUTPUT PARAMETER isbad AS LOGICAL INITIAL FALSE NO-UNDO.

  IF newnam THEN DO: 
    RUN "adecomm/_valname.p"
      (INPUT (INPUT FRAME frame-d wfil._File-name),INPUT no,OUTPUT isbad).
    isbad = NOT isbad. /* _valname.p returns opposite of what we want here*/
    IF isbad THEN DO:
       APPLY "ENTRY" TO wfil._File-name IN FRAME frame-d.
       RETURN.
    END.
  END.

  FIND FIRST _File OF _Db
    WHERE _File._File-name = INPUT FRAME frame-d wfil._File-name
      AND (adding OR RECID(_File) <> dbkey) NO-ERROR.
  isbad = (AVAILABLE _File).
  IF isbad THEN DO:
    MESSAGE "This table name is already used within this database."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY "ENTRY" TO wfil._File-name IN FRAME frame-d.
  END.
END PROCEDURE.

