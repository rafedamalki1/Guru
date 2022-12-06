/*--------------------------------------------------------------------

File: adecomm/_getfdbs.p

Description:
    
    creates s_ttb_db-records for all foreign schemas in this physical
    PROGRESS-Db
            
Input-Parameters:
    p_sdbnm     Name of SchemaHolder
    p_sdbnm     Name of SchemaHolder
    p_vrsn      Version of SchemaHolder

Output-Parameters:
    none
    
Used/Modified Shared Objects:
    none
    
Author: Tom Hutegger

History:
    hutegger    94/06   creation
    hutegger    95/08   added dbrcd, empty and dspnm fields

                            
--------------------------------------------------------------------*/
/*h-*/

/*----------------------------  DEFINES  ---------------------------*/

define input parameter p_dbnr       as integer.
define input parameter p_sdbnm      as character.
define input parameter p_vrsn       as character.

{ adecomm/getdbs.i }

/*------------------------  INT.-PROCEDURES  -----------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/

/*------------------------------------------------------------------*/

for each DICTDB._Db:

  if  DICTDB._Db._Db-local = TRUE          /* not a foreign schema  */
   or DICTDB._Db._Db-name  = "z_ora_links" /* for internal use only */
   then next.

  find first DICTDB._File of DICTDB._Db
    /* where DICTDB._File._File-num > 0 *//* foregin schemas don't  */
    no-lock no-error.                     /* have negativ file-num  */
   
  create s_ttb_db.
  assign
    s_ttb_db.cnnctd = CONNECTED(DICTDB._Db._Db-name)
    s_ttb_db.dbnr   = p_dbnr
    s_ttb_db.dbrcd  = RECID(DICTDB._Db)
    s_ttb_db.dbtyp  = { adecomm/ds_type.i
                        &direction = "itoe"
                        &from-type = "DICTDB._Db._Db-type"
                      }
    s_ttb_db.dspnm  = DICTDB._Db._Db-name + "(" + p_sdbnm + ")"
    s_ttb_db.empty  = NOT available DICTDB._File
    s_ttb_db.ldbnm  = DICTDB._Db._Db-name
    s_ttb_db.local  = FALSE
    s_ttb_db.pdbnm  = ( if CONNECTED(DICTDB._Db._Db-name)
                          then PDBNAME(DICTDB._Db._Db-name)
                          else DICTDB._Db._Db-addr
                      )
    s_ttb_db.sdbnm  = p_sdbnm
    s_ttb_db.vrsn   = p_vrsn.

  end.

/*------------------------------------------------------------------*/
