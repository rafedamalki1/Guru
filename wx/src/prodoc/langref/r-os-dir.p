
DEFINE VARIABLE stat AS INTEGER.
DEFINE VARIABLE dir_name AS CHARACTER FORMAT "x(64)"
   LABEL "Enter the name of the directory you want to create." .

UPDATE dir_name.
OS-CREATE-DIR VALUE(dir_name).
stat = OS-ERROR.
IF stat NE 0 THEN
   MESSAGE "Directory not created. System Error #" stat.
