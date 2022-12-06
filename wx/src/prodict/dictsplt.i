/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* dictsplt.i - this routine will split a string into several array elements */

/*
  {&src} = variable to split into array elements
  {&dst} = array variable to hold pieces
  {&num} = number of pieces
  {&len} = maximum length of each piece
  {&chr} = char to split at (e.g. " " or ",")
*/

{&dst}[1] = {&src}.

DO j = 1 TO {&num} - 1:
  IF LENGTH({&dst}[j],"character") > {&len} THEN
    ASSIGN
      i             = R-INDEX({&dst}[j],"{&chr}",{&len})
      i             = (IF i = 0 THEN {&len} ELSE i)
      {&dst}[j + 1] = TRIM(SUBSTRING({&dst}[j],i + 1,-1,"character"))
      {&dst}[j    ] = SUBSTRING({&dst}[j],1,i,"character").
  ELSE
    {&dst}[j + 1] = "".
END.
