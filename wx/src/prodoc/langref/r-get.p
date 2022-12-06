DEF VARIABLE code-page-list AS CHARACTER.
DEF VARIABLE collation-list AS CHARACTER.
DEF VARIABLE i AS INTEGER.
DEF VARIABLE J AS INTEGER.

code-page-list = GET-CODEPAGES.

REPEAT i = 1 TO NUM-ENTRIES(code-page-list):
  DISPLAY ENTRY(i,code-page-list) FORMAT "x(19)" COLUMN-LABEL "Code Page" 
      WITH DOWN FRAME a.
  collation-list = GET-COLLATIONS(ENTRY(i,code-page-list)).
  REPEAT j = 1 TO NUM-ENTRIES(collation-list):
    DISPLAY ENTRY(j,collation-list) FORMAT "x(19)" COLUMN-LABEL "Collation" 
        WITH DOWN FRAME a.
    DOWN WITH FRAME a.
  END.
END.
