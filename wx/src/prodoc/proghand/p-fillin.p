/* p-fillin.p */

DEFINE VARIABLE filename AS CHARACTER FORMAT "X(80)"
                            VIEW-AS FILL-IN SIZE 42 BY 1.
DEFINE VARIABLE no-scroll AS CHARACTER.

DEFINE FRAME f
       filename no-scroll.

UPDATE filename no-scroll WITH FRAME f.
