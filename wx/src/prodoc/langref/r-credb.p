DEFINE VARIABLE dbpath	AS CHARACTER LABEL "Database" FORMAT "x(65)".

/* Prompt the user for the name of a demo database to connect. */
SET dbpath HELP "Enter the path of your database."
    WITH FRAME dbname-frame SIDE-LABELS.

/* If the entered name does not have the .db suffix, add it.
   This is necessary for the search function to work correctly. */
IF LENGTH(dbpath) < 3
THEN dbpath = dbpath + ".db".
ELSE IF SUBSTR(dbpath, LENGTH(dbpath) - 2) = ".db"
     THEN dbpath = dbpath + ".db".

/* If the database does not exist, create it from SPORTS. */
IF SEARCH(dbpath) = ?
THEN DO:
	MESSAGE "Database does not exist. Do you want to create it?"
	   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Connect Database"
	   	UPDATE create-it AS LOGICAL.
	IF create-it
	THEN DO:	
   		CREATE DATABASE dbpath FROM "SPORTS".
   		MESSAGE "New database created:" dbpath.
   	END.
   	ELSE UNDO, RETRY.
END.

/* Connect the database. */
CONNECT VALUE(dbpath) -1.
