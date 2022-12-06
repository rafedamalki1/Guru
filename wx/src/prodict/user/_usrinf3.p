/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/
/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/user/_usrinf3.p

Description:
    gets collation- and codepage-name of current DB
    
Input-Parameters:
    p_currdb        name of current DB
        
Output-Parameters:
    p_collname      collation-name of current db
    p_codepage      codepage-name of current db
    
    
History:
    hutegger    94/06/13    creation
    
--------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER p_currdbn  AS character.
DEFINE INPUT  PARAMETER p_currdbt  AS character.
DEFINE OUTPUT PARAMETER p_codepage AS character.
DEFINE OUTPUT PARAMETER p_collname AS character.

/*------------------------------------------------------------------*/

if p_currdbt = "PROGRESS"
  then find first DICTDB._db where DICTDB._db._db-name = ?         no-error.         
  else find first DICTDB._db where DICTDB._db._db-name = p_currdbn no-error.         
if available DICTDB._Db
  then assign 
    p_codepage = DICTDB._Db._Db-xl-name
    p_collname = DICTDB._Db._Db-coll-name.               
  else assign 
    p_codepage = ""
    p_collname = "".               


/*------------------------------------------------------------------*/
