/* Compile a series of source files passed 
   in a comma separated list.		     */
   
DEFINE INPUT PARAMETER sources AS CHARACTER.

DEFINE VARIABLE entry-num AS INTEGER.

/* If the output file already exists, delete it.
   (If this results in an error, ignore the error.) */
OS-DELETE "compile.msgs".

DO entry-num = 1 TO NUM-ENTRIES(sources):
   COMPILE VALUE(ENTRY(entry-num, sources)) SAVE.
   IF COMPILER:ERROR
   THEN DO:
	OUTPUT TO "compile.msgs" APPEND.
	MESSAGE "Compilation error in" COMPILER:FILENAME "at line"
		 COMPILER:ERROR-ROW "column" COMPILER:ERROR-COL.
	OUTPUT CLOSE.
   END.
END.
