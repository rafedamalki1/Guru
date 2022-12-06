/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: as4dict/load/as4name.i

Description:
    checkes and eventually transformes a name into a valid unique 
    as4-name
    
Text-Parameters:
    none

Objects changed:
    nam         INPUT:  as4-name estimation
                OUTPUT: valid, unique as4-name
                    
Included in:
    as4dict/load/_lod_fil.p
    

Author: Donna McMann

    mcmann      95/01/31    Modified dumpname.i for DB2/400 V7 Utilities
    mcmann      97/09/30    Modified for RPG/400 length names
--------------------------------------------------------------------*/

/*--------------------------------------------------------------------
needs the following variables defined:

DEFINE VARIABLE nam  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pass AS INTEGER   NO-UNDO.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE lngth AS INTEGER.
--------------------------------------------------------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/

/* Make sure the name is a valid for  AS400 */   

assign lngth = LENGTH(nam).  
IF lngth > 10 THEN lngth = 10.
  
do i = 1 to lngth:
  if i = 1 then do:
    if (asc(substring(nam,i,1)) >= 64 AND  asc(substring(nam,i,1)) <= 90)  OR
         (asc(substring(nam,i,1)) >= 97 AND asc(substring(nam,i,1)) <= 122)  OR
         (asc(substring(nam,i,1)) >= 35 AND asc(substring(nam,i,1)) <= 36)  THEN.
    else
           assign nam = "A" + substring(nam,2).
  end.
  else do:
     if (asc(substring(nam,i,1)) >= 64 AND asc(substring(nam,i,1)) <= 90)  OR
         (asc(substring(nam,i,1)) >= 97 AND asc(substring(nam,i,1)) <= 122)  OR
         (asc(substring(nam,i,1)) >= 35 AND asc(substring(nam,i,1)) <= 36)  OR   
         (asc(substring(nam,i,1)) >= 48 AND asc(substring(nam,i,1)) <= 57)  OR
         (asc(substring(nam,i,1)) = 44) OR
         (asc(substring(nam,i,1)) = 46) THEN.
     else 
         assign nam = substring(nam, 1, i - 1) + "_" + substring(nam,i + 1).     
  end.    
end. 

IF user_env[29] = "yes" THEN DO:
  do i = 1 to lngth:
      if asc(substring(nam,i,1)) = 95  THEN
           assign nam = substring(nam, 1, i - 1) + substring(nam,i + 1).   
  end.  
  IF LENGTH(nam) > 8 THEN 
    ASSIGN nam = SUBSTRING(nam,1,8)
           lngth = 8.
  ELSE         
    ASSIGN lngth = LENGTH(nam).
END.  

/* Make sure we don't have a duplicate out there.  */

IF (CAN-FIND(as4dict.p__File WHERE as4dict.p__File._AS4-File = nam) OR 
    CAN-FIND(as4dict.p__Index WHERE as4dict.p__Index._As4-File = nam)) THEN DO:
   IF user_env[29] = "yes" THEN
       ASSIGN pass = 1
              nam  = SUBSTRING(nam,1,lngth - LENGTH(STRING(pass)))
                        + STRING(pass)
              lngth = lngth - 1.
   ELSE             
       ASSIGN pass = 1
              nam  = SUBSTRING(nam + "_______",1,10 - LENGTH(STRING(pass)))
                        + STRING(pass).
END.

DO pass = 2 TO 9999 WHILE 
            CAN-FIND(FIRST as4dict.p__File WHERE as4dict.p__File._AS4-File = nam) OR
            CAN-FIND(FIRST as4dict.p__Index WHERE as4dict.p__Index._As4-File = nam):
   IF user_env[29] = "yes" THEN
       ASSIGN nam  = SUBSTRING(nam,1,lngth - LENGTH(STRING(pass)))
                         + STRING(pass).
   ELSE        
       ASSIGN nam = SUBSTRING(nam + "_______",1,10 - LENGTH(STRING(pass)))
                      + STRING(pass).
END.




