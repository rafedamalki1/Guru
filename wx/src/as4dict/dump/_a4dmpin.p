/*
   Procedure _a4dmpin.p Entry point for the incremental dump in the PROGRESS/400
                        Data Dictionary.  Called from menutrig.i when the user
                        selects Create Incrementad .DF file in the Admin menu.
                        
   Created:  D. McMann  06/05/97
   
*/                        
   

{ as4dict/dictvar.i SHARED }
{ as4dict/dump/dumpvar.i "NEW SHARED" }
{ as4dict/brwvar.i SHARED }
{ as4dict/dump/userpik.i NEW }

DEFINE NEW SHARED FRAME working.
{ as4dict/dump/as4dmpdf.f &FRAME = "working" }

DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE j AS INTEGER.

ASSIGN cache_db# = 0
       j = 1.

/* Setup shared variables for proceeding procedures */
DO i = 1 TO NUM-DBS:
  IF DBTYPE(i) = "AS400" THEN  
     ASSIGN cache_db# = j
            cache_db_t[j] = dbtype(i)    
            cache_db_l[j] = pdbname(i)
            j = j + 1.    
END.  

ASSIGN user_dbtype = "AS400".
PAUSE 0 BEFORE-HIDE.
     
RUN as4dict/dump/_usrincr.p.

RETURN.
