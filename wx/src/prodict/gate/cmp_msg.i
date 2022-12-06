/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/gate/cmp_msg.i

Description:
    
    compares an attribute, and ev. creates and add an adequate message

        
Text-Parameters:
    &object    {Sequence|Table|Field|Index|Index-Field}  
    &o-name    foreign name of the object
    &attrbt    Label of attribute
    &msgidx    index of message
    &msgvar    message-variable
    &sh        value in the PROGRESS-schemaholder
    &ns        value in the nativ foreign schema

Output-Parameters:
    none
    
Included in:
    gate/_gat_cmp.p

History:
    hutegger    95/03   creation
    
--------------------------------------------------------------------*/        
/*h-*/

if {&sh} <> {&ns}
 then assign 
    l_{&msgvar}-msg = l_{&msgvar}-msg + "    " + "{&object} "
                    + {&o-name} + ": " + "{&attrbt}"
                    + ( if length(l_msg[{&msgidx}],"character") > 10
                            then chr(10) + chr(9)
                            else ""
                      )
                    + l_msg[{&msgidx}] + chr(10)
                    + chr(9) + "SH: " 
                    + ( if {&sh} = ?
                            then "?" 
                            else string({&sh})
                      )
                    + ( if length(string({&sh}) + string({&ns})
                                 ,"character"                  ) > 30
                            then chr(10)
                            else ""
                      )
                    + chr(9) + "NS: "
                    + ( if {&ns} = ?
                            then "?" 
                            else string({&ns})
                      )
                    + chr(10).

/*------------------------------------------------------------------*/
