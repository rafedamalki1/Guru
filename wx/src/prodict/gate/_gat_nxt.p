/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _gat_nxt.p - iterator for define/update cycle */

{ prodict/user/uservar.i }

/* CTOSISAM */
IF user_env[7] = "b+" THEN /* add */
  user_path = "_usrtchg,19=alpha,_usrfchg,7=b,_gat_nxt".
ELSE
IF user_env[7] = "b-" THEN /* modify */
  user_path = "_usrtget,_usrtchg,19=alpha,_usrfchg,7=b,_gat_nxt".

/* C-ISAM */
ELSE
IF user_env[7] = "c" THEN
  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
  user_path = "2=add,_ism_def,_usrtchg,7=c,_gat_nxt".
  &ELSE
  user_path = "2=add,_ism_def,7=c,_gat_nxt".
  &ENDIF

/* NetISAM */
ELSE
IF user_env[7] = "n" THEN
  user_path = "2=add,_ism_def,_usrtchg,7=n,_gat_nxt".

/* RMS */
ELSE
IF user_env[7] = "r" THEN
  user_path = "2=add,_rms_def,_usrtchg,7=r,_gat_nxt".

RETURN.
