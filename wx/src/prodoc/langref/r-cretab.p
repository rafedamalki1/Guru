CREATE TABLE employee (emp_num INTEGER NOT NULL UNIQUE,
                       name CHARACTER(30) NOT NULL,
                       job CHARACTER(15),
                       dept CHARACTER(15),
                       startdate DATE NOT NULL,
                       salary DECIMAL(8,2),
                   UNIQUE(name, startdate)).
