/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

DEFINE {1} SHARED VARIABLE pro_dbname   AS CHARACTER.
DEFINE {1} SHARED VARIABLE pro_conparms AS CHARACTER.
DEFINE {1} SHARED VARIABLE osh_dbname   AS CHARACTER.
DEFINE {1} SHARED VARIABLE odb_dbname   AS CHARACTER.
DEFINE {1} SHARED VARIABLE odb_pdbname  AS CHARACTER.
DEFINE {1} SHARED VARIABLE odb_username AS CHARACTER.
DEFINE {1} SHARED VARIABLE odb_password AS CHARACTER.
DEFINE {1} SHARED VARIABLE odb_codepage AS CHARACTER.
DEFINE {1} SHARED VARIABLE odb_conparms AS CHARACTER.
DEFINE {1} SHARED VARIABLE movedata	AS LOGICAL.

DEFINE {1} SHARED STREAM dbg_stream.

DEFINE {1} SHARED VARIABLE stages 		AS LOGICAL EXTENT 7 NO-UNDO.
DEFINE {1} SHARED VARIABLE stages_complete 	AS LOGICAL EXTENT 7 NO-UNDO.

/*
 * Constants describing stage we are at.
 */ 
define {1} shared variable odb_create_sql	as integer   initial 1.
define {1} shared variable odb_dump_data   	as integer   initial 2.
define {1} shared variable odb_create_sh 	as integer   initial 3. 
define {1} shared variable odb_create_objects	as integer   initial 4.
define {1} shared variable odb_build_schema	as integer   initial 5.
define {1} shared variable odb_fixup_schema	as integer   initial 6.
define {1} shared variable odb_load_data	as integer   initial 7. 
define {1} shared variable s_file-sel           as character initial "*". 
