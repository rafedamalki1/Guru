 /*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* -------------------------------------------------------------------   
  
   Procedure _as4sydd.p 

  Load is finished now see what user wants to do.  This procedures is used in
  usermenu.i as an entry point to the PROGRESS/400 dictionary directory.
  
  Created 05/05/95 D. McMann.
----------------------------------------------------------------------- */   

{ prodict/user/uservar.i }

 FIND _Db WHERE _Db._Db-name = ldbname("DICTDBG").

CREATE ALIAS as4dict FOR DATABASE VALUE(_Db._Db-name).      
RUN as4dict/as4_sydd.p.

{ as4dict/delalias.i }   /* delete alias */
