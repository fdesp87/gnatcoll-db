CREATE SCHEMA schema_test2;
SET SCHEMA 'schema_test2';

CREATE TABLE Ships (             
	IMO_Id          character varying (30)  NOT NULL, 
	Name            character varying (100) NOT NULL, 
	Tons            integer                 NOT NULL,
    Current_Flag    integer                 NOT NULL,
	Current_Captain character varying (30)  NOT NULL      
);
COMMENT ON TABLE Ships IS '(Ship) Ships Data';
COMMENT ON COLUMN Ships.IMO_Id IS 'Ship Identification';
COMMENT ON COLUMN Ships.Name IS 'Ship Name';
COMMENT ON COLUMN Ships.Tons IS 'Ship Tons';
COMMENT ON COLUMN Ships.Current_Flag IS '(Ships_With_Flag) Ship current registered country';
COMMENT ON COLUMN Ships.Current_Captain IS '(Ships_With_Captain) Ship current captain';


CREATE TABLE Flags (
	ISO_Number integer                NOT NULL,
	Country    character varying (15) NOT NULL,
    Currency   character (3)          NOT NULL
);
COMMENT ON TABLE Flags IS '(Flag) Flags or Countries Data';
COMMENT ON COLUMN Flags.ISO_Number IS 'ISO assigned country number';
COMMENT ON COLUMN Flags.Country IS 'Country name';
COMMENT ON COLUMN Flags.Currency IS 'country currency abbreviation';

CREATE TABLE Captains (
	Capt_Id    character varying (6)  NOT NULL,
	Capt_Name  character varying (30) NOT NULL,
    Capt_Phone character varying (20)  NOT NULL
);
COMMENT ON TABLE Captains IS '(Captain) Captains Data';
COMMENT ON COLUMN Captains.Capt_Id IS 'Captain identification';
COMMENT ON COLUMN Captains.Capt_Name IS 'Captain name';
COMMENT ON COLUMN Captains.Capt_Phone IS 'Captain phone number';

ALTER TABLE ONLY Ships    ADD CONSTRAINT Ships_PK    PRIMARY KEY (IMO_Id);
ALTER TABLE ONLY Flags    ADD CONSTRAINT Flags_PK    PRIMARY KEY (ISO_number);
ALTER TABLE ONLY Captains ADD CONSTRAINT Captains_PK PRIMARY KEY (Capt_Id);

ALTER TABLE ONLY Ships
    ADD CONSTRAINT Ships_With_Flag FOREIGN KEY (Current_Flag) 
    REFERENCES Flags (ISO_number);
ALTER TABLE ONLY Ships
    ADD CONSTRAINT Ships_With_Captain FOREIGN KEY (Current_Captain) 
    REFERENCES Captains (Capt_Id);

CREATE VIEW Full_Ships_View as
SELECT
	Ships.IMO_Id, 
	Ships.Name, 
	Ships.Tons,
    Flags.Country,
	Captains.Capt_Name
FROM (Ships JOIN Flags ON Ships.Current_Flag = Flags.ISO_Number)
      JOIN Captains ON Ships.Current_Captain = Captains.Capt_Id;  
COMMENT ON VIEW Full_Ships_View IS '(Full_Ship_View) Complete Ships View';

insert into Captains values ('US-901', 'Elon Musk',    '+1.777.777.777');
insert into Captains values ('EU-999', 'Rafael Nadal', '+2.888.888.888');
insert into Captains values ('US-DC', 'Dennis Conner', '+1.999.999.999');

insert into Flags values (840, 'United States', 'USD');
insert into Flags values (978, 'Europe', 'EUR');

insert into Ships values ('US1', 'America', 20, 840, 'US-901');
insert into Ships values ('US83', 'Starts and Stripes', 18, 840, 'US-DC');
insert into Ships values ('US85', 'Starts and Stripes', 18, 840, 'US-DC');
insert into Ships values ('US86', 'Starts and Stripes', 18, 840, 'US-DC');
insert into Ships values ('US87', 'Starts and Stripes', 18, 840, 'US-DC');
insert into Ships values ('EU1', 'Better', 25,  978, 'EU-999');

