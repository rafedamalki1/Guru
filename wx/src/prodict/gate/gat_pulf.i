/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/gat/gat_pulf.i

Description:
    creates the _field-definitions out of the oraC-definitions
    
Text-Parameters:
    &length         ds-field containing the length
    &mand           condition for mandatory-field
    &msc23          "{&msc23}"
    &name           ds-field containing the name
    &order-offset   {0|5} to be added to the l_fld-pos
    &precision      ds-field containing the Precision
    &radix          ds-field containing the Radix
    &scale          ds-field containing the Scale
    &extent         in the range of 0 to n
    
Included in:            
    prodict/gate/gat_pul.i
    
History:
    hutegger    95/03   abstracted from prodict/ora/ora67mak.i

--------------------------------------------------------------------*/

/* this code gets executed only for the first element of array-field   */
/* so extent-code of field is always ##1 (even with real extent >= 10) */
assign
  pnam = TRIM(DICTDBG.{&name})
  pnam = ( if {&extent} > 0 AND LENGTH (pnam, "character") > 3 /* Drop the "##1" */
             then SUBSTRING (pnam, 1, LENGTH (pnam, "character") - 3, "character")
             else pnam )
  fnam = pnam
  pnam = ( if lookup(s_ttb_tbl.ds_type,"PROCEDURE,FUNCTION,PACKAGE") <> 0
            and lookup(SUBSTRING(pnam,1,1,"character"),"@,&,#") <> 0
            then substring(pnam,2,-1,"character")
            else pnam
         ).

RUN prodict/gate/_gat_fnm.p 
    ( INPUT        "FIELD",
      INPUT        RECID(s_ttb_tbl),
      INPUT-OUTPUT pnam
    ).

if NOT SESSION:BATCH-MODE
 then DISPLAY
   TRIM(DICTDBG.{&name}) @ msg[3]
   pnam                  @ msg[4]
   WITH FRAME ds_make.

assign
  dtyp    = LOOKUP(l_dt,user_env[12])
  l_init  = ?
  ntyp    = ( if dtyp = 0
                then "undefined"
                else ENTRY(dtyp,user_env[15])
            )
  l_dcml  = 0.

CREATE s_ttb_fld.

assign
  s_ttb_fld.pro_Desc    = l_fld-descr
  s_ttb_fld.pro_Extnt   = {&extent}
  s_ttb_fld.pro_name    = pnam
  s_ttb_fld.ttb_tbl     = RECID(s_ttb_tbl)
  s_ttb_fld.ds_prec     = {&Precision}
  s_ttb_fld.ds_scale    = {&Scale}
  s_ttb_fld.ds_lngth    = {&Length}
  s_ttb_fld.ds_radix    = {&Radix}
  s_ttb_fld.ds_msc23    = {&msc23}
  s_ttb_fld.ds_msc24    = l_fld-msc24
  s_ttb_fld.ds_stoff    = l_fld-pos
  s_ttb_fld.ds_name     = fnam
  s_ttb_fld.ds_type     = l_dt
  s_ttb_fld.pro_order   = l_fld-pos * 10 + 1000 
                              + {&order-offset}
  s_ttb_fld.pro_mand    = {&mand}
  l_init                = {&init}.
/* ODBC:                                                      */
/*   If the field is not updatable (l_fld-msc24 contains "N") */ 
/*   then the field cannot be mandatory.                      */
/* ORA, SYB:                                                  */
/*   l_fld-msc24 = ?                                          */

{prodict/gate/gat_pul2.i
  &undo = "next"
  }


/*------------------------------------------------------------------*/
