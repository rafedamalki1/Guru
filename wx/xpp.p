Problem Description:
How to open several .p file's in new buffer from double click in Windows?
--------------------------------------------------------------------------------
26/08/04 18:22 nsahlqvi ACTION LOG ENTRY BY: Nicolas Sahlqvist
Hi Anders,

I have been looking into your issue, but I have not find any options or
clues into the behavior, can you please check in your Explorer Browser,
under "Tools" you will find "Folder Options" and therein you will find
"File Types". The file types is in a browser window with a scrollbar,
please find the "Progress .P File" in that list, push Advanced, thereafter
select the default choice and push Edit. Please send me all the info noted
in the fields (Action, Application used to preform action and all the DDE
fields if
enabled).

The following param.p will show the -param syntax, please call it param.p
and place it
in a directory of your choice and include ithe path to it in the above
"Application used
to preform action" field to include -p <path to param.p> before the -param
string. It will
create a param.log in the current directory with the file names as
parameters. Please restart the machine and begin by clicking on a .p file
and when it is started click on another one. A dialogue box should pop up
each time displaying the path, also appended to the param.log in the
current work directory (i.e. where you double click on the files).

/* param.p begin */

OUTPUT TO param.log APPEND.
MESSAGE "From command line: " SESSION:PARAMETER.
OUTPUT CLOSE.
MESSAGE SESSION:PARAMETER
    VIEW-AS ALERT-BOX TITLE "From command line".

/* param.p end */

Please send me all the param.log files and additional information I
requested.


Regards,
Nicolas Sahlqvist
Progress Technical Support EMEA

================================================================================
                          Progress Software Corporation
================================================================================
Report Title : Print Work Ticket                                   Page:    1
Requested By : Nicolas Sahlqvist (nsahlqvi)          Printed: 27/08/04 17:37
Work Request : W408260019    W/R Summary     Last Action Log Entry
--------------------------------------------------------------------------------

W408260019  Customer: Elpool i Umeå AB                Req.Date: 26/08/04 12:18CE
            Caller: Anders Olsson                     Ph: [46] 90-184544
            Fax: 90   184549
            EMail: anders@elpool.se
            Product: 0000091772           9.1D        Problem: DEPLOY
            Skill Group: PC                EMEA       Queue: nsahlqvi
            Pri: MED   Status: FOLLOWUP               Esc.Date:


END OF CALL DETAILS
--
----------------------------------------------------------------------------
                         Technical Support URL

Website    : http://www.progress.com/tech_support/index.ssp
Knowledge  : http://www.progress.com/tech_support/knowledge_center/index.ssp
Center    

----------------------------------------------------------------------------
