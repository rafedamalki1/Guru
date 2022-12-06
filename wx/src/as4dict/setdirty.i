/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: setdirty.i

Description:
   Set s_DictDirty to the given value.  If dict is dirty, enable the undo
   and commit menu options.  Otherwise, gray them.
 
Arguments
   &Dirty - true/false.

Author: Laura Stern

Date Created: 03/27/92

----------------------------------------------------------------------------*/

do:
   s_DictDirty = {&Dirty}.

   MENU-ITEM mi_Undo:sensitive in MENU s_mnu_Edit = {&Dirty}.
   MENU-ITEM mi_Commit:sensitive in MENU s_mnu_Edit = {&Dirty}.
end.

