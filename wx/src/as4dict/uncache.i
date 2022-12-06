/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: uncache.i

Description:
   Clear out one of the browse window selection lists, reset the cached
   flag and set the s_Currxxx variable for a particular object type.

Arguments:
   &List    - The list to clear.
   &Cached  - The cached flag to reset
   &Curr    - The s_Currxxx variable to reset.

Author: Laura Stern

Date Created: 04/26/92 

----------------------------------------------------------------------------*/

/* Clear out the selection list */
{&List}:LIST-ITEMS in frame browse = "".

{&Cached} = false.
{&Curr} = "".
