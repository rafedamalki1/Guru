/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: as4dict/load/dumpname.i

Description:
    checkes and eventually transformes a name into a valid unique 
    dumpname
    
Text-Parameters:
    none

Objects changed:
    nam         INPUT:  dump-name estimation
                OUTPUT: valid, unique dump-name
                    
Included in:
    as4dict/load/_lodname.p
    as4dict/load/_lod_fil.p
    

Author: Tom Hutegger

History:
    hutegger    94/05/23    creation
    mcmann      95/01/31    Modified for DB2/400 V7 Utilities
    
--------------------------------------------------------------------*/
/*h-*/
/*--------------------------------------------------------------------
needs the following variables defined:

DEFINE VARIABLE nam  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pass AS INTEGER   NO-UNDO.
--------------------------------------------------------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/

  nam = LC(SUBSTRING(nam,1,8)).
  IF CAN-FIND(as4dict.p__File WHERE as4dict.p__File._Dump-name = nam) THEN
    ASSIGN
      pass = 1 /*ABSOLUTE(_File-num)*/
      nam  = SUBSTRING(nam + "-------",1,8 - LENGTH(STRING(pass)))
           + STRING(pass).

  DO pass = 1 TO 9999 WHILE CAN-FIND(as4dict.p__File 
                 WHERE as4dict.p__File._Dump-name = nam):
    ASSIGN nam = SUBSTRING(nam + "-------",1,8 - LENGTH(STRING(pass)))
               + STRING(pass).
  END.

/*------------------------------------------------------------------*/
