/* p-codpag.p */

DEFINE VARIABLE xname LIKE customer.name INITIAL ?.

DO WHILE xname <> "":
    ASSIGN xname = "".
    DISPLAY xname LABEL "Starting Name to German List" 
        WITH FRAME A SIDE-LABELS.
    SET xname WITH FRAME A.
    IF xname <> "" THEN DO:
        OUTPUT TO german.txt
            CONVERT SOURCE "iso8859-1" TARGET "german-7-bit".

        FOR EACH customer WHERE customer.name >= xname BY name:
            DISPLAY customer WITH STREAM-IO.
        END.
        OUTPUT CLOSE.

        OUTPUT TO swedish.txt
            CONVERT SOURCE "iso8859-1" TARGET "swedish-7-bit".

        FOR EACH customer WHERE customer.name < xname BY name:
            DISPLAY customer WITH STREAM-IO.
        END.
        OUTPUT CLOSE.
    END.
END.
