/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Progress Lex Converter 7.1A->7.1B Version 1.11 */

/* userpaus.i - pause that is not confused by end-error */
/*              this also does not do an input-clear like PAUSE would. */

IF TERMINAL <> "" THEN DO ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
  READKEY PAUSE 0.
  IF LASTKEY = -1 THEN PAUSE.
END.
