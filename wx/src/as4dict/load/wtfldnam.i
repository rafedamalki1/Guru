/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: wtFldnam.i

Description:  Validate AS/400 Field Name in the Load process.  This is 
      the same validation as the the Progress/400 Dictionary, so changes
      should be made in both places.

Modified 10/21/97 Changed to handle RPG Length Names DLM
         10/08/98 Changed user_dbname to user_env[34] 98-10-18-044 DLM
------------------------------------------------------------------------------*/      


/* Make sure the name is a valid for  AS400 */   
assign nlngth = LENGTH(A4FldNam).        
IF nlngth > 10 THEN 
    ASSIGN nlngth = 10
           A4FldNam = SUBSTRING(A4FldNam,1,10).

do j = 1 to nlngth:
  if j = 1 then do:
    if (asc(substring(A4FldNam,j,1)) >= 64 AND  asc(substring(A4FldNam,j,1)) <= 90)  OR
         (asc(substring(A4FldNam,j,1)) >= 97 AND asc(substring(A4FldNam,j,1)) <= 122)  OR
         (asc(substring(A4FldNam,j,1)) >= 35 AND asc(substring(A4FldNam,j,1)) <= 36)  THEN.
    else
         assign A4FldNam = "A" + substring(A4FldNam,2).
  end.
  else do:
    if (asc(substring(A4FldNam,j,1)) >= 64 AND asc(substring(A4FldNam,j,1)) <= 90)  OR
         (asc(substring(A4FldNam,j,1)) >= 97 AND asc(substring(A4FldNam,j,1)) <= 122)  OR
         (asc(substring(A4FldNam,j,1)) >= 35 AND asc(substring(A4FldNam,j,1)) <= 36)  OR   
         (asc(substring(A4FldNam,j,1)) >= 48 AND asc(substring(A4FldNam,j,1)) <= 57)  OR
         (asc(substring(A4FldNam,j,1)) = 44) OR
         (asc(substring(A4FldNam,j,1)) = 46) THEN.
     else 
         assign A4FldNam = substring(A4FldNam, 1, j - 1) + "_" + substring(A4FldNam,j + 1).
  end.    
end.     

IF user_env[29] = "yes" THEN DO:
  do j = 1 to nlngth:
    if asc(substring(A4FldNam,j,1)) = 95  THEN
         assign A4FldNam = substring(A4FldNam, 1, j - 1) + substring(A4FldNam,j + 1).   
  end. 
  IF LENGTH(A4FldNam) > 6 THEN
    ASSIGN nlngth = 6
           A4FldNam = SUBSTRING(A4FldNam,1,6).
  ELSE 
    ASSIGN nlngth = LENGTH(A4FldNam).
END.            
   

 /* Check for duplicate Field Name  */
if imod = "a" then do:

   if can-find(first wtp__field where wtp__field._As4-file = As4filename
              and wtp__field._As4-library = CAPS(user_env[34])
              and wtp__field._For-Name = A4Fldnam) then do:

/* If we generated the field name, just generate a different one.  If the
   user supplied it, notified them that the name is different.  */
      if generated_name then do:
         IF user_env[29] = "no" THEN DO:
            ASSIGN pass = 1
                 A4Fldnam = SUBSTRING(A4Fldnam + "_______",1,10 - LENGTH(STRING(pass)))
                            + STRING(pass).
 
            DO pass = 2 TO 9999 
              WHILE can-find(first wtp__field where wtp__field._As4-file = wtp__file._As4-file 
               and wtp__field._As4-library = CAPS(user_env[34])                  
               and wtp__field._For-Name = A4Fldnam):
                    
               ASSIGN A4Fldnam = SUBSTRING(A4Fldnam + "_______",1,10 - LENGTH(STRING(pass)))
                                 + STRING(pass).
            end.
         END.
         ELSE DO:
            ASSIGN pass = 1
                   A4Fldnam = SUBSTRING(A4Fldnam,1,nlngth - LENGTH(STRING(pass)))
                            + STRING(pass).         
 
            DO pass = 2 TO 9999 
               WHILE can-find(first wtp__field where wtp__field._As4-file = wtp__file._As4-file 
               and wtp__field._As4-library = CAPS(user_env[34])                  
               and wtp__field._For-Name = A4Fldnam):
                    
              ASSIGN A4Fldnam = SUBSTRING(A4Fldnam,1,nlngth - LENGTH(STRING(pass)))
                                + STRING(pass).
            end.       
         end.
      END.   
      else
        ierror = 43. /* "AS400 Field Name already exists in File" */         
   end.  /* if can-find */
end.  /* if imod = "a"  */




