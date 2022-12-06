/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/
/*----------------------------------------------------------------------------

File: _chkfmt.i

Description:
This function validates the format of a given field.

Input Parameters:
	fd-tp  - The type of the field 
	          1 - char
		  2 - date
		  3 - logical
		  4 - other (integer or decimal)
	fd-nm  - The field Name
	lbl    - The field label
   	fmt    - The field format to check

Output Parameters:
	fld-wdth - The width of the field
	lError   - TRUE if there is an error.

Author:   R. Hunter, G. O'Connor

Date Created: June 24, 1993

----------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER fd-tp     AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER fd-nm     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER lbl       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER fmt       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER fld-wdth  AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER lError    AS LOGICAL   NO-UNDO.
							    
DEFINE VARIABLE sError AS CHARACTER NO-UNDO.

/* The field width is the maximum of label-width and format width */
/* ksu 02/24/94 LENGTH use raw mode */
ASSIGN 
  fld-wdth = MAXIMUM(LENGTH(lbl, "RAW":U), 
               IF      fd-tp = 1 THEN LENGTH(STRING("A",fmt), "RAW":U)
               ELSE IF fd-tp = 2 THEN LENGTH(STRING(TODAY,fmt), "RAW":U)
               ELSE IF fd-tp = 3 THEN MAXIMUM(LENGTH(STRING(YES,fmt), "RAW":U),
                                              LENGTH(STRING(NO,fmt), "RAW":U))
               ELSE LENGTH(STRING(0,fmt), "RAW":U))
  lError   = FALSE
  NO-ERROR.

IF (ERROR-STATUS:ERROR) THEN DO:
  ASSIGN
    sError = REPLACE(ERROR-STATUS:GET-MESSAGE(1),"** ":u,"")
    sError = SUBSTRING(sError,1,INDEX(sError,".":u),"CHARACTER":u)
    lError = TRUE
    .

  IF (fd-nm > "") THEN
    MESSAGE sError SKIP
       """" + fmt + """ is not a valid format for" fd-nm + "."
       VIEW-AS ALERT-BOX ERROR BUTTON OK.
  ELSE
    MESSAGE sError SKIP
      """" + fmt + """ is not a valid format."
      VIEW-AS ALERT-BOX ERROR BUTTON OK.
  ASSIGN
    fld-wdth = LENGTH(lbl,"RAW":U).
END.

RETURN.

/* _chkfmt.p - end of file */

