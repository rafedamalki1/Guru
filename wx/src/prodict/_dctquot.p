/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

DEFINE INPUT  PARAMETER inline  AS CHARACTER            NO-UNDO.
DEFINE INPUT  PARAMETER quotype AS CHARACTER            NO-UNDO.
DEFINE OUTPUT PARAMETER outline AS CHARACTER INITIAL "" NO-UNDO.
DEFINE        VARIABLE  i       AS INTEGER              NO-UNDO.

IF INDEX(inline,quotype) > 0 THEN
  DO i = 1 TO LENGTH(inline):
    outline = outline + (IF SUBSTRING(inline,i,1) = quotype
              THEN quotype + quotype ELSE SUBSTRING(inline,i,1)).
  END.
ELSE
  outline = inline.

outline = (IF outline = ? THEN "?" ELSE quotype + outline + quotype).
RETURN.
