/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

OUTPUT TO demodmp.out NO-MAP.
FOR EACH agedar:     DISPLAY agedar     WITH 2 COLUMNS. END.
FOR EACH customer:   DISPLAY customer   WITH 2 COLUMNS. END.
FOR EACH item:       DISPLAY item       WITH 2 COLUMNS. END.
FOR EACH monthly:    DISPLAY monthly    WITH 2 COLUMNS. END.
FOR EACH order:      DISPLAY order      WITH 2 COLUMNS. END.
FOR EACH order-line: DISPLAY order-line WITH 2 COLUMNS. END.
FOR EACH salesrep:   DISPLAY salesrep   WITH 2 COLUMNS. END.
FOR EACH shipping:   DISPLAY shipping   WITH 2 COLUMNS. END.
FOR EACH state:      DISPLAY state      WITH 2 COLUMNS. END.
/*FOR EACH syscontrol: DISPLAY syscontrol WITH 2 COLUMNS. END.*/
OUTPUT CLOSE.
RETURN.
