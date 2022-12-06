/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/ora/ora_typ.i

Description:
    assignes the correct data-type names according to the type-number
    
Text-Parameters:
    &data-type      Foreign data-type in PROGRESS-Notation
                    usually ds_columns.type#, except
                    when it's "TIME" to support the date/time structure
    &extent         in the range of 0 to n
    &order-offset   gets added to the _field._order
    
Included in:            
    prodict/ora/_ora_pul.p
    
History:
    hutegger    95/03   abstracted from prodict/ora/ora_mak.i

--------------------------------------------------------------------*/
/*h-*/
    
assign 
  l_dt = (IF    ds_columns.type# =  1
             OR ds_columns.type# = 96  THEN "CHAR"
        ELSE IF ds_columns.type# =  2  THEN "NUMBER"
        ELSE IF ds_columns.type# =  9  THEN "VARCHAR"
        ELSE IF ds_columns.type# = 11
             OR ds_columns.type# = 69  THEN "ROWID"
        ELSE IF ds_columns.type# = 12  THEN "DATE"
        ELSE IF ds_columns.type# =  8  THEN "LONG"
        ELSE IF ds_columns.type# = 23 
             OR ds_columns.type# = 108 THEN "RAW"
        ELSE IF ds_columns.type# = 24  THEN "LONGRAW"
        ELSE IF ds_columns.type# = 252 THEN "LOGICAL"
        ELSE IF ds_columns.type# = 102 THEN "CURSOR"
        ELSE                                "UNDEFINED").

&IF "{&procedure}" <> "YES"
 &THEN   /* this part is only for real columns, not for arguments */
  if      ds_columns.type#      =  2
    and   ds_columns.scale      =  ?
    and   ds_columns.precision_ <> ?  then assign l_dt = "FLOAT".
  else if ds_columns.type#      =  2
    and   ds_columns.precision_ <  0 
    and   ds_columns.scale
        - ds_columns.precision_ > 10  then assign l_dt = "FLOAT".
  &ENDIF
  

/*------------------------------------------------------------------*/
