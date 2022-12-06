/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: repname.i

Description:
   The name of a schema object has been changed.  Reflect this change in
   the appropriate selection list.
 
Arguments:
   &OldName - The old object name
   &NewName - The new object name.
   &List    - The list to change the name in.
   &Fill    - The fill-in associated with the list in the browse window.
   &Curr    - The current value variable for this object type.

Author: Laura Stern

Date Created: 04/24/92

----------------------------------------------------------------------------*/

Define var ret as integer.

/* 09-08-92 mikep fix for bug 92-09-01-118

   KLUDGE ALERT!!!  The REPLACE function on a selection list does not
   allow case insensitive search and replace, which is exactly what
   we need in this circumstance.  If it is possible to get REPLACE to
   be case-insensitive, that would solve the kludge.  In the absense
   of this, I've checked the error returned from REPLACE and if negative,
   attempt to do the case insensitive search myself, and then try to
   REPLACE again with the correct case of the string.  If my second
   attempt fails, the list is not updated.  This avoids a runtime error
   of setting VALUE on a item that does not exist in the list, but of
   course leaves the display incorrect.  If should really just rebuild
   the list as a last resort.  Laura?
*/

s_Res = {&List}:replace({&NewName}, {&OldName}) in frame browse.
if s_Res = no then 
do:
   ret = lookup({&OldName}, {&List}:list-items in frame browse).
   if ret > 0 then 
      s_Res = {&List}:replace({&NewName},  
                              entry(ret, {&List}:list-items in frame browse))
              in frame browse.
end.

if s_Res = yes then 
do:
    {&List}:screen-value in frame browse = {&NewName}.
    {&Curr} = {&NewName}.

    /* Note - we can't use display here.  We want to change the value of the
       widget whether it's visible right now or not.  Display will make it
       appear and we may not want it to. When it becomes visible we'll 
       see the change.
    */
    {&Fill}:screen-value in frame browse = {&NewName}.
end.

