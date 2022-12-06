/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

file: prodict/misc/_resxlat.p

Description:
    This routine checks wether an object-name is a reserved word by the
    respective foreign DB. If so, it appends an "_". In addition all 
    characters that are not supported by the foreign DB are replaced with
    "_".

Input:
    p_param     <object-name>[,<db-type>]

Output:
    p_param    <corrected object-name>

Changed/Used Shared Objects:
    none

Used Aliases:
    none

History:
    hutegger    01/96   rewritten based on the old version
                        prepared for separate lists per dataserver
                        (i.e: generic list for all DS, generic list
                        for ODBC-based DataServer, special lists
                        for all SQL-based DataServers)
                        I also prepared for separate substitution-chars
                        and/or different unallowed characters.
    jlewis   01/10/96   Added mssqlsrv V6.0 keywords
    hutegger    01/96   Added "&" as unallowed character
    jlewis   02/01/96   Added new Sybase 11 keywords
    jlewis   06/28/96   Added mssqlsrv V6.5 keywords

--------------------------------------------------------------------*/
/*-h*/
define input-output parameter p_param  as character no-undo.

define variable l_ch                as character no-undo.
define variable l_db-type           as character no-undo.
define variable l_db2-keywords      as character no-undo.
define variable l_gen-char          as character no-undo.
define variable l_ext-indicat       as character no-undo.
define variable l_gen-keywords      as character no-undo.
define variable l_i                 as integer   no-undo.
define variable l_mss-keywords      as character no-undo.
define variable l_odb-keywords      as character no-undo.
define variable l_ora-keywords      as character no-undo.
define variable l_pos               as integer   no-undo.
define variable l_s10-keywords      as character no-undo.
define variable l_subst-ch          as character no-undo.
define variable l_syb-keywords      as character no-undo.
define variable l_word              as character no-undo.
define variable l_maxlen            as integer   no-undo.

/*----------------------- INTERNAL PROCEDURES ----------------------*/
/*---------------------------- TRIGGERS ----------------------------*/
/*------------------------- INITIALIZATIONS ------------------------*/

assign

  /* all words, that are keywords in ALL foreign DBs */
  /* actually this is an Oracle reserved word list, but I cant assume
     that it is only Oracle without knowing ALL foreign DBs reserved
     words--sorry, in the future, we will add new Oracle reserved
     words to the Oracle list. (2/2/96 DaveM.) */
  l_gen-keywords = "ACCESS,ADD,ALL,ALTER,AND,ANY,ARITH_OVERFLOW,AS,ASC,"
                 + "ASSERT,ASSIGN,AT,AUDIT,AUTHORIZATION,AVG,BEGIN,"
                 + "BETWEEN,BREAK,BROWSE,BULK,BY,CASCADE,CHAR_CONVERT,"
                 + "CHAR,CHECK,CHECKPOINT,CLOSE,CLUSTER,CLUSTERED,"
                 + "COLUMN,COMMENT,COMMIT,COMPRESS,COMPUTE,CONFIRM,"
                 + "CONNECT,CONSTRAINT,CONTAIN,CONTAINS,CONTINUE,"
                 + "CONTROLROW,CONVERT,COUNT,CRASH,CREATE,CURRENT,CURSOR,"
                 + "DATA_PGS,DATABASE,DATABASES,DATAPAGES,DATE,DBA,DBCC,"
                 + "DEALLOCATE,DECIMAL,DECLARE,DEFAULT,DEFINITION,"
                 + "DELETE,DESC,DISK,DISTINCT,DOES,DOUBLE,DROP,DUMMY,"
                 + "DUMP,EACH,ELSE,END,ENDIF,ENDTRAN,ERRLVL,ERROREXIT,"
                 + "ESCAPE,EVALUATE,EXCEPT,EXCLUSIVE,EXEC,EXECUTE,EXISTS,"
                 + "EXIT,FETCH,FILE,FILLFACTOR,FLOAT,FOR,FOREIGN,"
                 + "FROM,GOTO,GRANT,GRAPHIC,GROUP,HAVING,HOLDLOCK,"
                 + "IDENTIFIED,IDENTITY,IDENTITY_INSERT,IF,IFDEF,"
                 + "IMAGE,IMMEDIATE,IN,INCREMENT,INDEX,INDEXED,"
                 + "INDEXPAGES,INITIAL,INSERT,INTEGER,INTERSEC,"
                 + "INTERSECT,INTO,IS,ISOLATION,KEY,KILL,LEVEL,LIKE,"
                 + "LINENO,LIST,LOAD,LOCK,LONG,MAX,MAXEXTENTS,MIN,"
                 + "MINUS,MIRROR,MIRROREXIT,MODE,MODIFY,MOVE"
                 + "NATIONAL,NEW,NOAUDIT,NOCOMPRESS,NOHOLDLOCK,"
                 + "NONCLUSTERED,NOSYSSORT,NOT,NOWAIT,NULL,NUMBER,"
                 + "NUMERIC_TRANSACTION,NUMERIC_TRUNCATION,OF,OFF,"
                 + "OFFLINE,OFFSETS,OLD,ON,ONCE,ONLINE,ONLY,OPEN,"
                 + "OPTIMIZE,OPTION,OR,ORDER,OVER,PARTITION,PCTFREE,PERM,"
                 + "PERMANENT,PLAN,PRECISION,PREPARE,PRIMARY,PRINT,PRIOR,"
                 + "PRIVILEGES,PROC,PROCEDURE,PROCESSEXIT,PUBLIC,"
                 + "RAISERROR,RAW,READ,READTEXT,RECONFIGURE,REFERENCES,"
                 + "RENAME,REPLACE,RESERVE_PGS,RESOURCE,RETURN,REVOKE,"
                 + "ROLE,ROLLBACK,ROW,ROWCNT,ROWCOUNT,ROWID,ROWLABEL,"
                 + "ROWNUM,ROWS,RULE,RUN,SAVE,SCHEMA,SELECT,SESSION,SET,"
                 + "SETUSER,SHARE,SHARED,SHUTDOWN,SIZE,SMALLINT,SOME,"
                 + "SPACE,START,STATEMENT,STATISTICS,STRIPE,SUCCESSFUL,"
                 + "SUM,SYNONYM,SYSDATE,SYSSORT,"
                 + "TABLE,TAPE,TEMP,TEMPORARY,TEXTSIZE,THEN,TO,TRAN,"
                 + "TRANSACTION,TRIGGER,TRUNCATE,TSEQUAL,UID,UNION,"
                 + "UNIQUE,UPDATE,USE,USED_PGS,USER,USER_OPTION,USING,"
                 + "VALIDATE,VALUES,VARCHAR,VARCHAR2,VARGRAPHIC,VARYING,"
                 + "VIEW,WAITFOR,WHENEVER,WHERE,WHILE,"
                 + "WITH,WORK,WRITETEXT"

  /* generic keywords plus all words, that are keywords in ORACLE only */
  l_ora-keywords = l_gen-keywords
                 + "DUAL,ORACLE"

  /* generic keywords plus all words, that are keywords in SYBASE-4 only */
  l_syb-keywords = l_gen-keywords
                 + "SYB_IDENTITY,SYB_RESTREE,SYB_TERMINATE"

  /* generic keywords plus all words, that are keywords in all odbc-based DBs */
  l_odb-keywords = l_gen-keywords
                 + ""

  /* generic keywords plus all words, that are keywords in DB2 only */
  l_db2-keywords = l_odb-keywords
                 + ""

  /* generic keywords plus all words, that are keywords in MS SQL-Server only */
  /* 01/10/95 jlewis - Below are a list of MS SQL Server V6.0 Reserved Words
                       These are POSSIBLE FUTURE KEYWORDS that may be added later 
                       by Microsoft.
                   "absolute,action,allocate,are,assertion,both,cascaded,cast,"
                 + "char_length,character,character_length,collate,collation,connection,"
                 + "constraints,corresponding,day,deferrable,deferred,describe,"
                 + "descriptor,diagnostics,disconnect,domain,end_exec,exception,"
                 + "expiredate,external,extract,false,first,get,global,hour,"
                 + "initially, inner,input,interval,last,leading,local,"
                 + "match,minute,month,names,natural,nchar,next,no,octet_length,"
                 + "output,overlaps,pad,partial,position,preserve,relative,"
                 + "retaindays,rows,second,sqlstate,time,timestamp,timezone_hour,"
                 + "timezone_minute,trailing,translate,translation,true,unknown,usage,"
                 + "using,value,volume,write,year,zone"                           */

  l_mss-keywords = l_odb-keywords
                 + "case,coalesce,committed,cross,current_date,current_time,current_timestamp,"
                 + "current_user,distributed,floppy,full,identitycol,insensitive,inner,join,"
                 + "left,nocheck,nullif,outer,pipe,repeatable,replication,restrict,right,"
                 + "scroll,serializable,session_user,system_user,uncommitted,updatetext,when"

  /* 02/01/96 jlewis - Added Sybase 11-specific keywords
                   "max_rows_per_page,age,unpartition" */
  /* generic keywords plus all words, that are keywords in SYBASE-10 only */
  l_s10-keywords = l_odb-keywords
                 + "SYB_IDENTITY,SYB_RESTREE,SYB_TERMINATE,"
                 + "max_rows_per_page,unpartition"
  .

assign

  /* characters that are not allowed */
  l_gen-char     = "%,-,&"

  /* substitution-character for characters that are not allowed */
  l_subst-ch     = "_"

  /* characters that are not allowed */
  l_ext-indicat  = "##"

  /* split up input-parameter's value */
  l_db-type      = ( if num-entries(p_param) > 1
                        then entry(2,p_param)
                        else ""
                   )
  l_word         = lc(entry(1,p_param))
  l_maxlen       = ( if num-entries(p_param) > 2
                        then integer ( entry(3,p_param))
                        else 0
                   )
  p_param        = l_word.

/*--------------------------- MAIN-BLOCK ---------------------------*/


/* replace unallowed characters */
repeat l_i = 1 to num-entries(l_gen-char):
  assign
    l_ch  = entry(l_i,l_gen-char)
    l_pos = index(l_word,l_ch)
    .
  repeat while l_pos <> 0:
    assign
      overlay(l_word,l_pos,1) = l_subst-ch
      l_pos                   = index(l_word,l_ch)
      .
    end.  /* repeat while l_pos <> 0 */
  end.  /* repeat l_i = 1 to num-entries(l_gen-char) */

/* trim leading "_"s */
repeat while substring(l_word,1,1,"character") = "_":
  assign l_word = substring(l_word,2,-1,"character").
  end.

/* check for empty value in l_word */
if l_word = ""
 then assign l_word = "z" + fill(l_subst-ch
                                ,length(p_param,"character") - 1
                                ).
/* This is old code for compatibility with V6 and is now causing problems
/* change foo#[0-9]* to foo##[0-9]* */
assign l_pos = r-index(l_word,l_ext-indicat).
if l_pos > 1
 and l_pos < length(l_word,"character")
 and substring(l_word, l_pos - 1, 1,"character") <> l_ext-indicat
 and index("0123456789",substring(l_word, l_pos + 1, 1,"character")) <> 0
 then assign substring(l_word,l_pos,1,"character")
                 = l_ext-indicat + substring(l_word,l_pos,1,"character").
*/

/* Need to verify that if Progress name has ## in it that the ## is separated so that it is
   not confused as an extent field. 
*/   
assign l_pos = r-index(l_word,l_ext-indicat).
if l_pos > 1 and l_pos < length(l_word,"character") THEN 
  ASSIGN l_word = substring(l_word, 1, l_pos) + "_" + substring(l_word, l_pos + 1).
   
/* check for reserved words */
/* note: if no db-type got passed, we check every db-type's keywords
 *       if db-type is not specially handled we check odbc's keywords
 */
if  (    lookup(l_db-type,"oracle,")   <> 0
     and lookup(l_word,l_ora-keywords) <> 0 )
 or (    lookup(l_db-type,"odbc,")     <> 0
     and lookup(l_word,l_odb-keywords) <> 0 )
 or (    lookup(l_db-type,"syb10,")    <> 0
     and lookup(l_word,l_s10-keywords) <> 0 )
 or (    lookup(l_db-type,"mssqlsrv,") <> 0
     and lookup(l_word,l_mss-keywords) <> 0 )
 or (    lookup(l_db-type,"db2,")      <> 0
     and lookup(l_word,l_db2-keywords) <> 0 )
 or (    lookup(l_db-type,"sybase,")   <> 0
     and lookup(l_word,l_syb-keywords) <> 0 )
 or (    lookup(l_db-type,"sybase,db2,mssqlsrv,syb10,odbc,oracle,") = 0
     and lookup(l_word,l_odb-keywords) <> 0 )
 then assign l_word = l_word + l_subst-ch.

/* Check for max length     */
/* This isn't perfect;  it will cut off a few characters;  it's possible what
   we end up with won't be unique 
*/

if l_maxlen <> 0 then do:
  if length (l_word) > l_maxlen then do:
    l_word = substring(l_word, 1, l_maxlen).
  end.
end.

assign p_param = l_word.


/*------------------------------------------------------------------*/









