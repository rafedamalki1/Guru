/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Progress Lex Converter 7.1A->7.1B Version 1.11 */

/* userfchg.i - performs routine for the different db mgrs */

{4} = FALSE.

RUN VALUE("prodict/" +  
  IF   _File._Db-lang = 1            THEN "pro/_pro_sql.p"
  ELSE IF user_dbtype = "CISAM"
       OR user_dbtype = "NETISAM"    THEN "ism/_ism_fld.p"
  ELSE IF user_dbtype = "RDB"        THEN "rdb/_rdb_fld.p"
  ELSE IF user_dbtype = "RMS"        THEN "rms/_rms_fld.p"
  ELSE IF user_dbtype = "ORACLE"     THEN "ora/_ora_fld.p"
  ELSE IF user_dbtype = "SYBASE"     THEN "syb/_syb_fld.p"
  ELSE IF CAN-DO(odbtyp,user_dbtype) THEN "odb/_odb_fld.p"
  ELSE IF user_dbtype = "AS400"      THEN "as4/_as4_fld.p"
  ELSE IF user_dbtype = "CTOSISAM"   THEN "bti/_bti_fld.p"
  ELSE                                    "pro/_pro_fld.p")
  ("{2}",{3},OUTPUT {4}).
