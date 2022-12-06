This project demonstrates the use of the VideoSoft FlexArray (vsflex2.ocx) and the 
Pinnacle-BPS Graph (graph32.ocx) controls.

The sample initializes the VSFlex array to display customer records from the sports 
database.  Ten rows of the array are filled to start, and it is then filled on demand as 
the user scrolls.  It is sorted either by name or by city depending on the radio-box 
setting at the top of the window.  If shown by city, the City column for successive rows 
that share the same city is merged.  You can also edit any cell by clicking on it and 
typing a new value.  To store the new value in the database, click outside the cell and 
hit OK from the resulting alert-box.

When you select a row in the array, the total value of orders for that customer, by month,
will be graphed in the Graph control below.  If the customer records are sorted by city,
then the graph shows total orders for that city, by month.  The 2nd radio set at the top
allows you to change the graph style at any time.

These specified controls must be installed on your machine in order to run this sample.
You must also connect to the sports database before running this sample.


Files for this Sample:

flex.w
flex.wrx

