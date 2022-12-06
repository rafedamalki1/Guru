/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: dbvar.i

Description:   
   Include file which defines the user interface components for database
   properties.
 
Arguments:
   {1} - this is either "new shared" or "shared".

Author: Laura Stern

Date Created: 03/03/92 
    Modified: 09/29/97 Added logical DB name for as4dict
----------------------------------------------------------------------------*/

Define {1} frame dbprops.  /* database properties */

Define {1} var s_Db_Pname  as char NO-UNDO.
Define {1} var s_Db_Holder as char NO-UNDO.
Define {1} var s_Db_Type   as char NO-UNDO.
Define {1} var s_Db_LName as character no-undo.


/* This is the form for the database properties window. */
{as4dict/DB/dbprop.f} 






