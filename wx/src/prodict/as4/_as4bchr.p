/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*  _as4bchr.p   Replace invalid characters in a string with a space */

/* Created 12/15/93 jmorin 
  Modified        
*/ 

DEFINE INPUT-OUTPUT PARAMETER io-str   AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER bad-chr  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE i   AS INTEGER NO-UNDO INITIAL 1.
DEFINE VARIABLE j   AS INTEGER NO-UNDO INITIAL 999.

IF io-str = "" OR io-str = ? THEN RETURN.

DO WHILE j NE 0:
  ASSIGN j = INDEX(io-str,bad-chr,i).
  IF j NE 0 THEN 
    ASSIGN SUBSTRING (io-str,j,1) = " ".
           i = j + 1.
END. /* do while */

