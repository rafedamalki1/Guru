/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Progress Lex Converter 7.1A->7.1B Version 1.11 */

/*----------------------------------------------------------------
   Shared variables used for parameter file editor.

   Author: Tony Lavinio, Laura Stern
     Data: 02/03/93

     Tex updated 1/4/95 with 7.3b I18N parameters (cp**)
-----------------------------------------------------------------*/

/* arg_nam   - Starts as array of all the parm data: name description and type.
               Translated into array of arg name/description pairs 
               (e.g., -B   Buffers)
   arg_lst   - All parm names (-B) strung out in a comma delimited list
   arg_typ   - Array of argument data types (l, c, n)
   arg_val   - Array of values
   args#     - Number of arguments 
   arg_comm  - Array of comment lines 
   arg_comm# - # of non-blank comment lines
   pf_file   - The parameter file to edit.
*/

DEFINE {1} SHARED VARIABLE arg_lst   AS CHARACTER CASE-SENSITIVE NO-UNDO.
DEFINE {1} SHARED VARIABLE arg_typ   AS CHARACTER EXTENT 200     NO-UNDO.
DEFINE {1} SHARED VARIABLE arg_val   AS CHARACTER EXTENT 200     NO-UNDO.
DEFINE {1} SHARED VARIABLE args#     AS INTEGER   INITIAL 0      NO-UNDO.
DEFINE {1} SHARED VARIABLE arg_comm  AS CHARACTER EXTENT 10      NO-UNDO.
DEFINE {1} SHARED VARIABLE arg_comm# AS INTEGER   INITIAL 0      NO-UNDO.
DEFINE {1} SHARED VARIABLE pf_file   AS CHARACTER                NO-UNDO.

/* PKEY  is the actual parameter: "-B"
   PDESC is the description: "Buffers"
   PNAME is the key plus the description: "-B  Buffers" 
   COMM# is the number of comment lines supported
*/
&GLOBAL-DEFINE PKEY_LEN    11
&GLOBAL-DEFINE PDESC_LEN   25
&GLOBAL-DEFINE PNAME_LEN   37
&GLOBAL-DEFINE VAL_START   39
&GLOBAL-DEFINE VAL_CHARS   39   /*How many chars to allow input*/
&GLOBAL-DEFINE COMM_LEN           73
&IF "{&WINDOW-SYSTEM}" begins "MS-Win" &THEN
&GLOBAL-DEFINE COMM#       8
&ELSE
/* Change comm# to 6 if we use _guipfed.p under TTY */
&GLOBAL-DEFINE COMM#       10
&ENDIF

/*---------------------------------------------------------------------
   These labels match the VMS arguments wherever possible. 
   They should not be translated to non-English languages. 

   The following parameters are not valid in .pf files:
     -lm  (leave memory)
     -ovl (overlay buffer size)
     -rft (return fault table)
   
   The following parameters were deliberately left out to avoid problems:
     -K c   (keycapture mode)
     -noshm (no shared memory on server)
   
   There are a bunch of other parameters that are used only in utilities
   (like proshut).
   
----------------------------------------------------------------------*/

DEFINE {1} SHARED VARIABLE arg_nam AS CHARACTER EXTENT 137 NO-UNDO INITIAL [
  "1,SINGLE-USER,l",
  "25,RESTORE-25-ROW-MODE,l",
  "a,AFTER-IMAGE,c",
  "aibufs,AFTER-IMAGE-BUFFERS,n",
  "aistall,AFTER-IMAGE-STALL,l",
  "autoexit,AUTO-EXIT,l",
  "b,BATCH,l",
  "B,BUFFERS,n",
  "bi,BI-CLUSTER-SIZE,n",
  "bibufs,BI-BUFFERS,n",
  "brl,BLEEDING-RECORD-LOCK,l",
  "Bt,TEMP-TABLE-BUFFERS,n",
  "charset,CHARACTER-SET,c",
  "checkdbe,CHECK-DBE,l",
  "convmap,CONVERSION-MAP,c",
  "cp,COMM-PARMFILE,c",
  "cpcase,CASE-CHARACTER-SET,c",
  "cpcoll,COLLATION-CHARACTER-SET,c",
  "cpdb,V6-DB-CHARACTER-SET,c",
  "cpinternal,INTERNAL-CHARACTER-SET,c",
  "cpprint,PRINTER-CHARACTER-SET,c",
  "cprcodein,READ-RCODE-CHARACTER-SET,c",
  "cprcodeout,WRITE-RCODE-CHARACTER-SET,c",
  "cpstream,STREAM-CHARACTER-SET,c",
  "cpterm,TERMINAL-CHARACTER-SET,c",
  "crd,COORDINATOR,l",
  "cs,INDEX-CURSOR-SIZE,n",
  "d,DATE-FORMAT,c",
  "D,COMPILED-FILE-SLOTS,n",
  "da,DIRECT-ACCESS,l",
  "db,DATABASE,c",
  "debug,DEBUG,l",
  "decdtm,DECDTM,l",
  "dictexps,DICTIONARY-EXPRESSIONS,l",
  "directio,DIRECT-I/O,l",
  "dosnames,DOSNAMES,l",
  "dt,DB-TYPE,c",
  "E,EUROPEAN-NUMBERS,l",
  "ems,USE-EXP-MEM-SYS,l",
  "esqllog,ESQL-LOG,l",
  "esqlnopad,ESQL-NO-PADDING,l",
  "g,BEFORE-IMAGE,c",
  "h,MAX-DATABASES,n",
  "H,HOST-NAME,c",
  "hash,HASH,n",
  "hs,UNIX-HEAPSIZE,n",
  "i,NO-INTEGRITY,l",
  "inp,MAX-STATEMENT-LEN,n",
  "is,IGNORE-TIME-STAMPS,l",
  "k,KEYWORD-FORGET,c",
  "l,LOCAL-BUFFER-SIZE,n",
  "L,NUMBER-LOCKS,n",
  "ld,LOGICAL-DBNAME,c",
  "lng,LANGUAGE-NAME,c",
  "m1,AUTO-SERVER,l",
  "m2,MANUAL-SERVER,l",
  "m3,LOGIN-BROKER,l",
  "Ma,MAXIMUM-CLIENTS,n",
  "mb,ISAM-BATCH,l",
  "Mf,TRANSACTION-DELAY,n",
  "Mi,MINIMUM-CLIENTS,n",
  "ml,ISAM-LOG-FILE-NAME,c",
  "Mm,MESSAGE-BUFFER-SIZE,n",
  "mmax,MAX-R-CODE-MEMORY,n",
  "Mn,MAXIMUM-SERVERS,n",
  "Mp,SERVERS/PROTOCOL,n",
  "Mr,RECORD-BUFFER-SIZE,n",
  "mt,ISAM-TRANS,l",
  "Mv,MAXIMUM-FILES,n",
  "Mxs,EXCESS-SHARED-MEM,n",
  "n,NUMBER-OF-USERS,n",
  "N,NETWORK-TYPE,c",
  "nb,NESTED-BLOCK-LIMIT,n",
  "Nb,NETWORK-BUFFERS,n",
  "Nd,UNIX-NETWORK-DEVICE,c",
  "NL,NO-LOCK-DEFAULT,l",
  "Nn,NET-NUM,n",
  "noshm,NO-SHARED-MEMORY,l",
  "Nv,NETWORK-VERSION,n",
  "o,PRINTER,c",
  "ovl,OVERLAY-SIZE,n",
  "p,STARTUP-PROCEDURE,c",
  "P,PASSWORD,c",
  "param,PARAMETER,c",
  "pf,PARAMETER-FILE,c",
  "plm,PROLIB-MEMORY,l",
  "pls,PROLIB-SWAP,l",
  "pp,PROPATH-DIRECTORY,c",
  "pwqdelay,APW-QUEUE-DELAY,n",
  "pwqmin,APW-MIN-QUEUE-LEN,n",
  "pwscan,APW-SCAN-BUFFERS,n",
  "pwsdelay,APW-SCAN-DELAY,n",
  "pwwmax,APW-WRITES-PER-SCAN,n",
  "q,QUICK-REQUEST,l",
  "Q,STRICT-ANSI-SQL,l",
  "Q2,ANSI-SQL-CLIENT,l",
  "r,BI-BUFFERED-I/O,l",
  "R,BI-UNBUFFERED-I/O,l",
  "rand,RANDOM-NUM-MODE,n",
  "rft,RETURN-FAULT-TABLE,n",
  "rg,RUN-4GL-CLIENT,l",
  "RO,READ-ONLY-DATABASE,l",
  "rq,RUN-QUERY-CLIENT,l",
  "rr,RUN-RUNTIME-CLIENT,l",
  "rx,ENCRYPTED-COMPILER,l",
  "s,STACK-SIZE,n",
  "S,SERVER-NAME,c",
  "spin,SPIN-LOCK-TRIES,n",
  "stream,STREAM,c",
  "stsh,STASH-AREA-SIZE,n",
  "SV,ESQL-SERVER-CONNECTION,l",
  "SYBc,SYBASE-CONNECTIONS,c",
  "SYBt,SYBASE-TIMEOUT,n",
  "t,SAVE-TEMP-FILES,l",
  "T,TEMP-FILE-DIRECTORY,c",
  "TB,SORT-BLOCK-SIZE,n",
  "TM,MERGE-BUFFER-COUNT,n",
  "tok,MAX-TOKENS,n",
  "trig,TRIGGER-LOCATION,c",
  "tstamp,TIME-STAMP,l",
  "U,USERID,c",
  "v,VIDEO,c",
  "v6q,V6-QUERY,l",
  "VO,ORACLE-VERSION,n",
  "Wa,X-WINDOW-ATTRIBUTES,c",
  "ws,WINDOW-SYSTEM,l",
  "wx,WINDOWS-EXIT,c",
  "xc,COLLATION-RULES,c",
  "y,STATISTICS,l",
  "yc,STATISTICS-CTRLC,l",
  "yd,SEGMENT-STATISTICS,l",
  "yield,YIELD-CPU,l",
  "yx,STATISTICS-XREF,l",
  "yy,YEAR-OFFSET,n",

  ""
].


