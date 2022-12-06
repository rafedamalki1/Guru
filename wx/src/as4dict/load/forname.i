/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* as4dict/load/forname.i
   Donna L. McMann
   September 30, 1997

   Include to verify that an underscore is not present in the _For-Format   
   when RPG/400 length names are to be generated.
   
*/

lngth = LENGTH(nam).

do i = 1 to lngth:
    if asc(substring(nam,i,1)) = 95  THEN
         assign nam = substring(nam, 1, i - 1) + substring(nam,i + 1).   
end.     

/* Make sure we don't have a duplicate out there.  */

IF CAN-FIND(FIRST as4dict.p__File WHERE as4dict.p__File._For-Format = nam) THEN
  ASSIGN pass = 1
         nam  = SUBSTRING(nam,1,8 - LENGTH(STRING(pass))).
 

DO pass = 2 TO 9999 WHILE 
  CAN-FIND(FIRST as4dict.p__File WHERE as4dict.p__File._For-Format = nam):
     ASSIGN pass = 1
            nam  = SUBSTRING(nam,1,8 - LENGTH(STRING(pass))).
END.




