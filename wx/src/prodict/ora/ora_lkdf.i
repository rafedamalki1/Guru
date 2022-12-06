/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/ora/ora_lkdf.i

Description:
    defines the link - temp-table 
    
Text-Parameters:  
    &new    "new" or "" for the shared temp-table

    
History:
    hutegger    94/09/11    creation
    
--------------------------------------------------------------------*/
/*h-*/

define {&new} shared temp-table s_ttb_link
            field name       as   character format "x(32)"
            field level      as   integer   format "z9"
            field slctd      as   logical   format "*/ "
            field srchd      as   logical   format "done/open"
            field master     as   character format "x(42)"
            field presel-n   as   character format "x(30)" initial "*"
            field presel-o   as   character format "x(30)" initial "*"
            field presel-q   as   character format "x(30)" initial "*"
            field presel-t   as   character format "x(30)" initial "*"
            field presel-v   as   logical              /*  initial no */
            index us                           srchd level master
            index uit        is unique         master name slctd
            index upi        is unique primary level master name.

define {&new} shared variable s_ldbconct    as character.
define {&new} shared variable s_level       as integer.
define {&new} shared variable s_ldbname     as character.
define {&new} shared variable s_lnkname     as character.
define {&new} shared variable s_master      as character.
define {&new} shared variable s_name-hlp    as character.
define {&new} shared variable s_owner-hlp   as character.
define {&new} shared variable s_qual-hlp    as character.
define {&new} shared variable s_type-hlp    as character.
define {&new} shared variable s_vrfy-hlp    as logical.
define {&new} shared variable s_trig-reass  as logical.

/*------------------------------------------------------------------*/

