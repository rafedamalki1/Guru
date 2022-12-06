/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _dmpseqs.p - dump _Sequence file into _Seqvals.d */
/*
in:  user_env[2] = Name of file to dump to.
     user_env[5] = "<internal defaults apply>" or "<target-code-page>"

History:
    hutegger    94/02/24    code-page - support and trailer-info added
        
*/
/*h-*/

{ as4dict/dictvar.i shared}
{ as4dict/dump/dumpvar.i shared}

DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
DEFINE VARIABLE tmpfile AS CHARACTER NO-UNDO.

/*  The following variable is simply a placeholder to replace the */
/*  seq-num value which is displayed in a PROGRESS format dump.   */
/*  This dump format will be the same as a PROGRESS format dump, so  */
/*  a user may use the output of this dump in any LOAD SEQUENCE   */
/*  CURRENT VALUES selection.  Note, however, that the information */
/*  is dumped from the P__SEQ file, not the schema, as in all the  */
/*  PROGRESS/400 dumps. Seqnumber is simply incremented each time  */
/*  and displyed in the dump, taking the place of _seq-num.        */
DEFINE VARIABLE seqnumber AS INTEGER NO-UNDO.

FIND FIRST as4dict.P__Db /* WHERE RECID(_Db) = drec_db */ NO-LOCK.

IF NOT CAN-FIND(FIRST as4dict.p__Seq) THEN DO:
   MESSAGE "There are no sequences to dump." SKIP
      	   "The output file has not been modified."
      	    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   RETURN.
END.

run adecomm/_setcurs.p ("WAIT").

RUN "adecomm/_tmpfile.p" (INPUT "", INPUT ".adm", OUTPUT tmpfile).
OUTPUT TO VALUE(tmpfile) NO-MAP NO-ECHO.

seqnumber = 0.
FOR EACH as4dict.p__Seq:
  PUT UNFORMATTED
/*  'EXPORT '
    as4dict.p__Seq._Seq-Num 
    ' "'                */
    'EXPORT ' 
    seqnumber
    ' "'
    as4dict.p__Seq._Seq-Name 
    '" CURRENT-VALUE(' as4dict.p__Seq._Seq-Name ',' LDBNAME(user_dbname) ').' SKIP. 
    seqnumber = seqnumber + 1.
END.
OUTPUT CLOSE.

IF  user_env[5] = " "
 OR user_env[5] = ?  THEN assign user_env[5] = "<internal defaults apply>".
 
IF user_env[5] = "<internal defaults apply>"
 then OUTPUT TO VALUE(user_env[2]) NO-ECHO NO-MAP NO-CONVERT.
 else OUTPUT TO VALUE(user_env[2]) NO-ECHO NO-MAP
             CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].

RUN VALUE(tmpfile).

  {prodict/dump/dmptrail.i
    &entries      = " "
    &seek-stream  = "OUTPUT"
    &stream       = " "
    }  /* adds trailer with code-page-entrie to end of file */
    
OUTPUT CLOSE.

OS-DELETE VALUE(tmpfile). 
run adecomm/_setcurs.p ("").
MESSAGE "Dump of sequence values completed." 
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
RETURN.
