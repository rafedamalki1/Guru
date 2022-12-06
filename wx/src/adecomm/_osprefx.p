/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/
/**************************************************************************
    Procedure:  _osprefx.p
    
    Purpose:    Returns a file spec's path prefix and basename.

    Syntax :
    Parameters:
    Description:
    
    Notes  :
    Authors: Warren Bare
    Date   : 
    Updated: 
**************************************************************************/

/* ksu 94/02/24 SUBSTRING use default mode */
DEFINE INPUT  PARAMETER filename AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER fiprefix AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER basename AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

ASSIGN basename = TRIM(filename).

IF CAN-DO("OS2,WIN32,MSDOS,UNIX,VMS":u,OPSYS) THEN DO:
  i = R-INDEX(basename,":":u).
  IF i > 0 THEN
    basename = SUBSTRING(basename,i + 1,-1,"CHARACTER":u).
END.

IF CAN-DO("OS2,WIN32,MSDOS,UNIX":u,OPSYS) THEN DO:
  i = MAXIMUM(R-INDEX(basename,"~\":u),R-INDEX(basename,"/":u)).
  IF i > 0 THEN
  DO:
    /* WIN95-UNC - Check for UNC \\SERVER\SHARE and treat it like a
       drive specification. In which case, there is no basename and
       the prefix is the UNC or drive spec.  - jep 12/14/95 */
    IF basename BEGINS "~\~\":u AND NUM-ENTRIES(basename, "~\") <= 4 THEN
      basename = "".
    ELSE
      basename = SUBSTRING(basename,i + 1,-1,"CHARACTER":u).
  END.
END.

IF OPSYS = "BTOS":u AND R-INDEX(basename,"}":u) > 0 THEN
  basename = SUBSTRING(basename,R-INDEX(basename,"}":u) + 1,-1,"CHARACTER":u).

IF CAN-DO("VMS,BTOS":u,OPSYS) AND R-INDEX(basename,"]":u) > 0 THEN
  basename = SUBSTRING(basename,R-INDEX(basename,"]":u) + 1,-1,"CHARACTER":u).

IF CAN-DO("VMS,BTOS",OPSYS) AND R-INDEX(basename,">":u) > 0 THEN
  basename = SUBSTRING(basename,R-INDEX(basename,">":u) + 1,-1,"CHARACTER":u).

fiprefix = SUBSTRING(filename,1,LENGTH(filename,"CHARACTER":u) 
                     - LENGTH(basename,"CHARACTER":u),"CHARACTER":u).
RETURN.
