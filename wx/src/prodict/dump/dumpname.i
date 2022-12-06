/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/dump/dumpname.i

Description:
    checkes and eventually transformes a name into a valid unique 
    dumpname
    
Text-Parameters:
    none

Objects changed:
    nam         INPUT:  dump-name estimation
                OUTPUT: valid, unique dump-name
                    
Included in:
    prodict/dump/_lodname.p
    prodict/dump/_lod_fil.p
    

Author: Tom Hutegger

History:
    hutegger    94/05/23    creation
    
--------------------------------------------------------------------*/
/*h-*/
/*--------------------------------------------------------------------
needs the following variables defined:

DEFINE VARIABLE nam  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pass AS INTEGER   NO-UNDO.
--------------------------------------------------------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/

  nam = LC(SUBSTRING(nam,1,8,"character")).
  IF CAN-FIND(_File WHERE _Dump-name = nam) THEN
    ASSIGN
      pass = 1 /*ABSOLUTE(_File-num)*/
      nam  = SUBSTRING(nam + "-------"
                      ,1
                      ,8 - LENGTH(STRING(pass),"character")
                      ,"character"
                      )
           + STRING(pass).

  DO pass = 1 TO 9999 WHILE CAN-FIND(_File WHERE _Dump-name = nam):
    ASSIGN nam = SUBSTRING(nam + "-------"
                          ,1
                          ,8 - LENGTH(STRING(pass),"character")
                          ,"character"
                          )
               + STRING(pass).
  END.

/*------------------------------------------------------------------*/
