/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------------
  dfile.i
  File Commands-Related Defines for Editor 
--------------------------------------------------------------------------*/

DEFINE VARIABLE  File_Name AS CHARACTER LABEL "Filename" FORMAT "x(50)" NO-UNDO.
  /*  OS file name currently being edited.  */

DEFINE VARIABLE Search_File AS CHAR NO-UNDO.
  /* Stores PROPATH search pathname for a file to be opened. */
