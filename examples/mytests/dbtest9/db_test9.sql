CREATE SCHEMA schema_test9;
SET SCHEMA 'schema_test9';

CREATE TABLE Banks (
	BIC     Char (4)     NOT NULL,
	IBAN    varchar (4)  NOT NULL,
    Office  Integer,
    City    varchar (20)
);

COMMENT ON TABLE Banks IS '(Bank) Banks Data';
COMMENT ON COLUMN Banks.BIC    IS 'Bank Identification Code';
COMMENT ON COLUMN Banks.IBAN   IS 'International Bank Account Number';
COMMENT ON COLUMN Banks.Office IS 'Office Number';
COMMENT ON COLUMN Banks.City   IS 'Office City';

CREATE TABLE Cars (
	Maker   varchar (10)    NOT NULL,
	Model   varchar (10)    NOT NULL,
    CYear   integer         NOT NULL,
    Price   Numeric (24, 8) NOT NULL Default 0.0
);
COMMENT ON TABLE Cars IS '(Car) Cars Data';
COMMENT ON COLUMN Cars.Maker  IS 'Car Maker';
COMMENT ON COLUMN Cars.Model  IS 'Car Model';
COMMENT ON COLUMN Cars.CYear  IS 'Car Year';
COMMENT ON COLUMN Cars.Price  IS 'Car Price';

CREATE TABLE Persons (
	Name       char (6)        NOT NULL,
	Bank_BIC   char (4)        NOT NULL,
    Bank_IBAN  varchar (4)     NOT NULL,
    Car_Maker  varchar (10),
    Car_Model  varchar (10),
    Car_Year   integer,
    Amount     Numeric (24, 8) NOT NULL Default 0.0
);
COMMENT ON TABLE Persons IS '(Person) Persons Data';
COMMENT ON COLUMN Persons.Name       IS 'Name';
COMMENT ON COLUMN Persons.Bank_BIC   IS 'Bank Identification Code';
COMMENT ON COLUMN Persons.Bank_IBAN  IS 'International Bank Account Number';
COMMENT ON COLUMN Persons.Car_Maker  IS 'Car Maker';
COMMENT ON COLUMN Persons.Car_Model  IS 'Car Model';
COMMENT ON COLUMN Persons.Amount     IS 'Current Amount';

ALTER TABLE ONLY Banks   ADD CONSTRAINT PK_Bank   PRIMARY KEY (BIC, IBAN);
ALTER TABLE ONLY Cars    ADD CONSTRAINT PK_Car    PRIMARY KEY (Maker, Model, CYear);
ALTER TABLE ONLY Persons ADD CONSTRAINT PK_Person PRIMARY KEY (Name);

ALTER TABLE ONLY Persons
    ADD CONSTRAINT Customer_Of FOREIGN KEY (Bank_BIC, Bank_IBAN) 
                  REFERENCES Banks (BIC, IBAN) MATCH FULL;
ALTER TABLE ONLY Persons
    ADD CONSTRAINT Car_Of FOREIGN KEY (Car_Maker, Car_Model, Car_Year) 
                  REFERENCES Cars (Maker, Model, CYear) MATCH FULL;

insert into Banks values ('ES20', '2000', 800, 'Paris');
insert into Banks values ('ES20', '2001', 801, 'Berlin');
insert into Banks values ('ES22', '2201', 802, 'London');
insert into Banks values ('ES22', '2202', 803, 'Madrid');

insert into Cars values ('Ford', 'Mustang', 2015, 50000.42);
insert into Cars values ('Ford', 'Mustang', 2020, 50000.42);
insert into Cars values ('Ford', 'Bronco', 2018, 55000.54);
insert into Cars values ('Audi', 'A6', 2020, 80000.75);
insert into Cars values ('Audi', 'A7', 2021, 95000.54);
insert into Cars values ('BMW', 'S3', 2014, 30000.33);
insert into Cars values ('BMW', 'S5', 2022, 70000.50);
insert into Cars values ('BMW', 'S7', 2021, 94800.75);

insert into Persons values ('John  ', 'ES20','2000', 'Ford', 'Mustang', 2020, 33.5);
insert into Persons values ('Lolita', 'ES20','2000', 'Audi', 'A6',      2020, 43.5);
insert into Persons values ('Elon  ', 'ES22','2201', 'Ford', 'Mustang', 2020, 31.5);
insert into Persons values ('Donald', 'ES22','2201', 'Audi', 'A7',      2021, 83.5);
insert into Persons values ('Boris ', 'ES20','2000', 'Ford', 'Mustang', 2015, 33.5);
insert into Persons values ('Macron', 'ES20','2000', 'BMW',  'S7',      2021, 13.5);
insert into Persons values ('Putin ', 'ES22','2201', 'Ford', 'Mustang', 2020, 3.5);
