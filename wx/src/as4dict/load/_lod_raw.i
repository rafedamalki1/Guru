/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _lod_raw.i - load the value of an _Db field whose data type is RAW.

   Modified 2/23/95 to use as4dict instead of prodict DLM.

   The first element of ilin should be a comment.  After that, the
   first num bytes (from the version) of the raw value are in the ilin 
   array - one array element per byte.  Each byte is in it's 3-digit 
   ascii representation.  Handle this line and all subsequent lines for 
   this field attribute.  The value will be stored in the wdbs buffer.

   {1} is the field to load.
   {2} field length (# of bytes to load)
   {3} recid of the wdbs buffer to load into.

   Input Parameters:
      p_Version - The version # of the collate/translate table format
*/

{ as4dict/load/loaddefs.i }

DEFINE INPUT  PARAMETER p_Version AS CHAR    NO-UNDO.
DEFINE OUTPUT PARAMETER p_Changed AS LOGICAL NO-UNDO.

DEFINE VAR ix  	    AS INT NO-UNDO init 0.
DEFINE VAR jx       AS INT NO-UNDO.
DEFINE VAR byte     AS INT NO-UNDO.
DEFINE VAR num      AS INT NO-UNDO.  /* # of bytes per line */
DEFINE VAR raw_val  AS RAW NO-UNDO.
DEFINE VAR orig_val AS RAW NO-UNDO.

/* Do some rudimentary error checking on the format of the data */
IF NOT (ilin[1] BEGINS "/*") THEN DO:
   ierror = 24.
   return.
END.
jx = INTEGER(ilin[2]) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
   ierror = 24.
   return.
END.

/* The version # format is n.n-m where m is the # of bytes on one
   line of the .df file.
*/
num = INTEGER(SUBSTR(p_Version, INDEX(p_Version, "-") + 1)).

FIND wdbs WHERE RECID(wdbs) = {3}.
orig_val = wdbs.{1}.

IF ilin[1] = ? THEN 
   wdbs.{1} = ?.
ELSE DO:
   IF wdbs.{1} = ? THEN
      /* This is the only way to replace the unknown value.
      	 putbyte won't do it!.
      */
      wdbs.{1} = raw_val.  

   DO WHILE ix < {2}:
      DO jx = 1 to num:
	 byte = INTEGER(ilin[jx + 1]).
	 PUTBYTE(wdbs.{1}, ix + jx) = byte.
      END.
      ix = ix + num.
      IF ix < {2} THEN DO:
	 IMPORT ilin.
      	 ipos = ipos + 1.
      END.
   END.
END.

p_Changed = (IF orig_val <> wdbs.{1} THEN yes ELSE no).

