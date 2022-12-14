/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* dictsrch.i - BINARY SEARCH THROUGH A SORTED ARRAY. */
/*
Necessary definitions:
DEFINE VARIABLE hb AS INTEGER NO-UNDO.
DEFINE VARIABLE lb AS INTEGER NO-UNDO.

&vector = array name
&extent = array limit
&tofind = value to find
&result = holds element number in array or -1 for not found
*/

ASSIGN
  hb = {&extent}
  lb = 1
  {&result} = 1.

DO WHILE {&result} <> -1:
  {&result} = TRUNCATE((hb + lb) / 2,0).
  IF hb < 1 OR lb > {&extent} OR hb < lb THEN {&result} = -1.
  ELSE IF {&vector}[{&result}] BEGINS {&tofind} THEN LEAVE.
  ELSE IF {&tofind} > {&vector}[{&result}] THEN lb = {&result} + 1.
  ELSE IF {&tofind} < {&vector}[{&result}] THEN hb = {&result} - 1.
  /*ELSE IF {&tofind} = {&vector}[{&result}] THEN LEAVE.*/
END.
DO WHILE {&result} > 1:
  IF {&vector}[{&result} - 1] BEGINS {&tofind} THEN
    {&result} = {&result} - 1.
  ELSE
    LEAVE.
END.
