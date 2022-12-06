/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* dictvar.i - dictionary shared variable definitions */
/*  02/19/97 DLM Changed cache_file from extent 1024 to 2048 (97-02-14-001) */

DEFINE {1} SHARED VARIABLE dict_rog    AS LOGICAL               NO-UNDO.
DEFINE {1} SHARED VARIABLE dict_trans  AS LOGICAL               NO-UNDO.
DEFINE {1} SHARED VARIABLE dict_dirty  AS LOGICAL               NO-UNDO.

DEFINE {1} SHARED VARIABLE cache_dirty AS LOGICAL  INITIAL TRUE NO-UNDO.

DEFINE {1} SHARED VARIABLE cache_db#   AS INTEGER   INITIAL 0   NO-UNDO.
DEFINE {1} SHARED VARIABLE cache_db_s  AS CHARACTER EXTENT 64   NO-UNDO.
DEFINE {1} SHARED VARIABLE cache_db_l  AS CHARACTER EXTENT 64   NO-UNDO.
DEFINE {1} SHARED VARIABLE cache_db_p  AS CHARACTER EXTENT 64   NO-UNDO.
DEFINE {1} SHARED VARIABLE cache_db_t  AS CHARACTER EXTENT 64   NO-UNDO.
DEFINE {1} SHARED VARIABLE cache_db_e  AS CHARACTER EXTENT 64   NO-UNDO.
/* s=schema_db, l=logical_db, p=physical_db, t=db_type(int), e=db_type(ext)*/

DEFINE {1} SHARED VARIABLE cache_file# AS INTEGER  INITIAL 0    NO-UNDO.
DEFINE {1} SHARED VARIABLE cache_file  AS CHARACTER EXTENT 2048 NO-UNDO.

DEFINE {1} SHARED VARIABLE drec_db     AS RECID    INITIAL ?    NO-UNDO.
DEFINE {1} SHARED VARIABLE drec_file   AS RECID    INITIAL ?    NO-UNDO.

/* ism_typ.p needs the RECID of the current _Db
 * TYY uses drec_db; GUI uses s_DbRecId
 * so we define s_DbRecId here too, it has no real value for TTY-side
 * ism_udt (called from ism_typ.p) uses s_DbRecId only if _Db(drec_db) 
 * can't be found
 */
DEFINE {1} SHARED VARIABLE s_DbRecId   AS RECID    INITIAL ?    NO-UNDO.

&IF "{&DATASERVER}" = "YES" OR "{&ORACLE-DATASERVER}" = "YES"
 &THEN
  { prodict/ora/ora_lkdf.i 
      &new = " {1}"
      } /* Defines temp-table s_ttb_link for DATASERVER*/
  {prodict/gate/gatework.i 
    &new        = " {1}"
    &selvartype = "{1} shared variable s"
    &options    = "initial ""*"" "
    } /* Defines: temp-table gate-work */
 &ENDIF
 

