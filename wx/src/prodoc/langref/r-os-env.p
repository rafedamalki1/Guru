
DEFINE VARIABLE pathname    AS CHARACTER FORMAT "x(32)"
                            LABEL "The report will be stored in ".
DEFINE VARIABLE report_name AS CHARACTER FORMAT "x(32)"
                            LABEL "Please enter report name." .
UPDATE report_name.
pathname = OS-GETENV("DLC") + "/" + report_name.
DISPLAY pathname WITH FRAME b SIDE-LABELS.
