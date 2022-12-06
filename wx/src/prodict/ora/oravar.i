/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/
/* Modified 11/17/97 DLM Added logicals for dump/load, validate and remove
                         obsolete object.
            01/13/98 DLM Added ora_version to know if 2000 or 4000 is now a long.             
*/                         

DEFINE {1} SHARED VARIABLE pro_dbname   AS CHARACTER.
DEFINE {1} SHARED VARIABLE pro_conparms AS CHARACTER.
DEFINE {1} SHARED VARIABLE ora_dbname   AS CHARACTER.
DEFINE {1} SHARED VARIABLE ora_version  AS INTEGER.
DEFINE {1} SHARED VARIABLE osh_dbname   AS CHARACTER.
DEFINE {1} SHARED VARIABLE ora_username AS CHARACTER.
DEFINE {1} SHARED VARIABLE ora_password AS CHARACTER.
DEFINE {1} SHARED VARIABLE ora_codepage AS CHARACTER.
DEFINE {1} SHARED VARIABLE ora_conparms AS CHARACTER.
DEFINE {1} SHARED VARIABLE ora_sid      AS CHARACTER.
DEFINE {1} SHARED VARIABLE compatible   AS LOGICAL.
DEFINE {1} SHARED VARIABLE movedata     AS LOGICAL.
DEFINE {1} SHARED VARIABLE loadsql      AS LOGICAL.
DEFINE {1} SHARED VARIABLE rmvobj       AS LOGICAL.

DEFINE {1} SHARED STREAM dbg_stream.
