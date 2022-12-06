/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/gate/_gat_atg.p

Description:
    
    To solve the fldpos-problem we delete all triggers and recreate
    them new. Thios gets done by prodict/ism/_ism_trg.p.
    There are some places in the GUI-Dict, where we need to do this
    for all tables of an _Db. That's what this routine does.
    
Input-Parameters:
    p_db-pname      physical db-name
    
Author: Tom Hutegger

History:
    hutegger    94/08/18    creation
    
--------------------------------------------------------------------*/        
/*h-*/

define INPUT parameter p_db-name   as   CHARACTER.

find first _Db where _Db._Db-name = p_db-name no-error.

if available _Db
 then for each _File of _Db:

  RUN prodict/gate/_gat_trg.p (RECID(_File)).
  
  end.
  
/*------------------------------------------------------------------*/        
