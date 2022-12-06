DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE lngth AS INTEGER.

UPDATE aname.
assign lngth = LENGTH(aname).
do i = 1 to lngth:
  if i = 1 then do:
    if (asc(substring(aname,i,1)) >= 64 AND  asc(substring(aname,i,1)) <= 90)  OR
         (asc(substring(aname,i,1)) >= 97 AND asc(substring(aname,i,1)) <= 122)  OR
         (asc(substring(aname,i,1)) >= 35 AND asc(substring(aname,i,1)) <= 36)  THEN.
         else
           assign aname = "A" + substring(aname,2).
   end.
   else do:
     if (asc(substring(aname,i,1)) >= 64 AND asc(substring(aname,i,1)) <= 90)  OR
         (asc(substring(aname,i,1)) >= 97 AND asc(substring(aname,i,1)) <= 122)  OR
         (asc(substring(aname,i,1)) >= 35 AND asc(substring(aname,i,1)) <= 36)  OR
         (asc(substring(aname,i,1)) = 44) OR
         (asc(substring(aname,i,1)) = 46) THEN.
         else
         assign aname = substring(aname, 1, i - 1) + "_" + substring(aname,i + 1).
     end.    
 end.     
 display aname.