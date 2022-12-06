/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: gateproc.i

Description:
   Configure the name of a gateway routine to call.

Arguments:
   &Suffix - the routine name suffix (following _ora for example).
   &Name   - the variable to set.

Author: Laura Stern

Date Created: 08/12/93 

----------------------------------------------------------------------------*/

&IF DEFINED(GATEPROC_VAR) = 0 &THEN /* in case someone includes this twice */
   Define var gdb_type  as char NO-UNDO.
   Define var gdb_otype as char NO-UNDO.
   &global-define GATEPROC_VAR ""
&ENDIF

assign
  gdb_type = s_DbCache_Type[s_DbCache_ix]
  gdb_otype = { adecomm/ds_type.i
                  &direction = "ODBC"
                  &from-type = "gdb_type"
                  }
  {&Name} = "prodict/" + 
   (if      gdb_type = "CISAM" OR
            gdb_type = "NETISAM"       then "ism/_ism{&Suffix}.p"
    else if gdb_type = "RDB"           then "rdb/_rdb{&Suffix}.p"
    else if gdb_type = "ORACLE"        then "ora/_ora{&Suffix}.p"
    else if gdb_type = "SYBASE"        then "syb/_syb{&Suffix}.p"
    else if gdb_type = "AS400"         then "as4/_as4{&Suffix}.p"
    else if gdb_type = "CTOSISAM"      then "bti/_bti{&Suffix}.p"
    else if gdb_type = "RMS"           then "rms/_rms{&Suffix}.p"
    else if CAN-DO(gdb_otype,gdb_type) then "odb/_odb{&Suffix}.p"
    else "").

