/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* dictgate.i - (another) performs routine for the different db mgrs */

{&output} = "{&action}".
RUN VALUE("prodict/" +
  ENTRY(LOOKUP({&dbtype},
               "AS400,CISAM,CTOSISAM,GENERIC,NETISAM," 
             + "OBJECTA,ORACLE,RDB,RMS,SYBASE,ODBC," 
             + "SYB10,ALLBASE,DB2,DB2-DRDA,MSSQLSRV") + 1,
        "pro/_pro,as4/_as4,ism/_ism,bti/_bti,gen/_gen,ism/_ism,"
      + "oag/_oag,ora/_ora,rdb/_rdb,rms/_rms,syb/_syb,odb/_odb,"
      + "odb/_odb,odb/_odb,odb/_odb,odb/_odb,odb/_odb")
  + "_sys.p")
  ({&dbrec},INPUT-OUTPUT {&output}).
