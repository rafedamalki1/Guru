/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Progress Lex Converter 7.1A->7.1B Version 1.11 */

/* usersho.i - variables for _usrshow.p */

/* The file to display - if this is ? then use sho_pages. */
DEFINE {1} SHARED VARIABLE sho_file  AS CHARACTER INITIAL ?   NO-UNDO.

/* Data to display - 1 line per array element */
DEFINE {1} SHARED VARIABLE sho_pages AS CHARACTER EXTENT 1024 NO-UNDO. 

/* The number of lines to display (only used with sho_pages) */
DEFINE {1} SHARED VARIABLE sho_limit AS INTEGER               NO-UNDO.

/* The title for the frame */
DEFINE {1} SHARED VARIABLE sho_title AS CHARACTER             NO-UNDO.
