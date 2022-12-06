/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ as4dict/dictvar.i shared }
{ as4dict/load/loaddefs.i }

DEFINE VARIABLE scrap AS CHARACTER NO-UNDO.

FIND FIRST wseq.
IF imod <> "a" THEN /* already proven to exist */
  FIND as4dict.p__Seq
    WHERE  as4dict.p__Seq._Seq-name = wseq._Seq-name.

IF imod = "a" THEN DO: /*---------------------------------------------------*/
  IF CAN-FIND(as4dict.p__Seq WHERE as4dict.p__Seq._Seq-name = wseq._Seq-name) THEN
    ierror = 7. /* "&2 already exists with name &3" */
/* sequences ought to be unique for physical PROGRESS-DB (hutegger) 94/08
 *  IF CAN-FIND(as4dict.p__Seq WHERE  as4dict.p__Seq._Seq-name = wseq._Seq-name) THEN
 *    ierror = 7. /* "&2 already exists with name &3" */
 */
     
  IF ierror > 0 THEN RETURN.
  CREATE as4dict.p__Seq.
  ASSIGN
    as4dict.p__Seq._Db-recid = 1
    as4dict.p__Seq._Seq-Name = CAPS(wseq._Seq-Name)
    as4dict.p__Seq._Seq-Init = wseq._Seq-Init
    as4dict.p__Seq._Seq-Incr = wseq._Seq-Incr
    as4dict.p__Seq._Seq-Min  = wseq._Seq-Min
    as4dict.p__Seq._Seq-Max  = (if wseq._Seq-Max = 0 then ? else wseq._Seq-Max)
    as4dict.p__Seq._Cycle-Ok = wseq._Cycle-Ok
    as4dict.p__Seq._Seq-Misc[1] = wseq._Seq-Misc[1]
    as4dict.p__Seq._Seq-Misc[2] = wseq._Seq-Misc[2]
    /* Set CURRENT-VALUE (stored in seq-num) to Seq-Min, otherwise it will
       default to 0 */
    as4dict.p__Seq._Seq-Num = wseq._Seq-Min.       
    
END. /*---------------------------------------------------------------------*/
ELSE
IF imod = "m" THEN DO: /*---------------------------------------------------*/
  IF as4dict.p__Seq._Seq-Name <> wseq._Seq-Name THEN
    as4dict.p__Seq._Seq-Name = wseq._Seq-Name.
  IF as4dict.p__Seq._Seq-Init <> wseq._Seq-Init THEN
    as4dict.p__Seq._Seq-Init = wseq._Seq-Init.
  IF as4dict.p__Seq._Seq-Min  <> wseq._Seq-Min THEN
    as4dict.p__Seq._Seq-Min  = wseq._Seq-Min.
  IF as4dict.p__Seq._Seq-Max  <> wseq._Seq-Max THEN
    as4dict.p__Seq._Seq-Max  = wseq._Seq-Max.
  IF as4dict.p__Seq._Seq-Incr <> wseq._Seq-Incr THEN
    as4dict.p__Seq._Seq-Incr = wseq._Seq-Incr.
  IF as4dict.p__Seq._Cycle-Ok <> wseq._Cycle-Ok THEN
    as4dict.p__Seq._Cycle-Ok = wseq._Cycle-Ok.
END. /*---------------------------------------------------------------------*/
ELSE
IF imod = "r" THEN DO: /*---------------------------------------------------*/
  IF CAN-FIND(FIRST as4dict.p__Seq WHERE as4dict.p__Seq._Seq-name = irename) THEN
    ierror = 7. /* "&2 already exists with name &3" */
  IF ierror > 0 THEN RETURN.
  as4dict.p__Seq._Seq-name = irename.
END. /*---------------------------------------------------------------------*/
ELSE
IF imod = "d" THEN DO: /*---------------------------------------------------*/
  DELETE as4dict.p__Seq.
END. /*---------------------------------------------------------------------*/

RETURN.
