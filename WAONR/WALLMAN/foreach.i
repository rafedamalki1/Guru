/*foreach.i*/
            DEFINE VARIABLE {2}{5}           AS {1} NO-UNDO . 
            DEFINE VARIABLE {2}Enumerator{5} AS System.Collections.IEnumerator NO-UNDO . 
            ASSIGN {2}Enumerator{5} = {4}:GetEnumerator() .
            {2}Enumerator{5}:Reset() .
            DO WHILE {2}Enumerator{5}:MoveNext() ON ERROR UNDO, THROW:
               ASSIGN {2}{5} = CAST({2}Enumerator{5}:Current, {1}) .  
