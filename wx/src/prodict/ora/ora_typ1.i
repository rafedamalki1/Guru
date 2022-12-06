/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------
    THIS IS THE OLD VERSION OF ora_typ.i! IT'S THERE ONLY TO MAKE THE
    OLD PULL-ROUTINES STILL WORK!

File: prodict/ora/ora_typ.i

Description:
    assignes the correct data-type names according to the type-number
    
Text-Parameters:
    &data-type      Foreign data-type in PROGRESS-Notation
                    usually {1}, except
                    when it's "TIME" to support the date/time structure
    &extent         in the range of 0 to n
    &order-offset   gets added to the _field._order
    
Included in:            
    prodict/ora/_ora_pul.p
    
History:
    hutegger    95/03   abstracted from prodict/ora/ora67mak.i

--------------------------------------------------------------------*/
/*h-*/
    
         (IF    {1} =  1
             OR {1} = 96  THEN "CHAR"
        ELSE IF {1} =  2  THEN "NUMBER"
        ELSE IF {1} =  9  THEN "VARCHAR"
        ELSE IF {1} = 11
             OR {1} = 69  THEN "ROWID"
        ELSE IF {1} = 12  THEN "DATE"
        ELSE IF {1} =  8  THEN "LONG"
        ELSE IF {1} = 23 
             OR {1} = 108 THEN "RAW"
        ELSE IF {1} = 24  THEN "LONGRAW"
        ELSE IF {1} = 252 THEN "LOGICAL"
        ELSE                                "UNDEFINED").

/*------------------------------------------------------------------*/
