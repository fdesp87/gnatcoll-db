CREATE SCHEMA schema_test1;
SET SCHEMA 'schema_test1';

CREATE TABLE Test_Types (
	F00_smallint smallint default -1 NOT NULL,                  -- Ada short_integer
	F01_integer  integer default -4 NOT NULL,
	F02_integer  integer NOT NULL,
	F03_bigint   bigint default -3 NOT NULL,                    -- Ada Long_Long_Integer

--	F10_Float_24 Float(24) default -3.5 NOT NULL,               -- unknown field type real
	F11_double_precision double precision default 1.0 NOT NULL, -- Ada Long_Float
	F12_Real Real default -3.5 NOT NULL,                        -- unknown field type real
	F13_Float Float default -3.5 NOT NULL,  -- translated by Postgresql to double precision

	F20_numeric_24_8 numeric(24,8) DEFAULT 1.0 NOT NULL,
	F21_numeric_8_4  numeric(8,4) DEFAULT -1.0 NOT NULL,
	F22_numeric      numeric DEFAULT -5.0 NOT NULL,   -- translated to Float
	F23_decimal      decimal DEFAULT -3.0 NOT NULL,   -- translated to Float
   	F24_money        money DEFAULT -3.0 NOT NULL,

	F30_char3    character(3) default 'abc' NOT NULL ,
	F31_varchar5 varchar (5)  NOT NULL,
	F32_varchar  varchar (5) default '89' NOT NULL,
	F33_text     text default 'abcdef' NOT NULL,

	F40_timestamp_notz timestamp without time zone NOT NULL, -- translated to timestamp with time zone
	F41_timestamp_tz   timestamp with time zone,
	F42_timestamp      timestamp,     -- error: SQL requires translated to timestamp without time zone
--	F43_timestamp6     timestamp (6), -- unknown field type timestamp(6) without time zone

	F50_date           date NOT NULL,
--	F51_time_notz      time without time zone, -- unknown field type time without time zone
-- 	F52_time_tz        time with time zone,    -- unknown field type time without time zone
--	F53_time           time,                   -- unknown field type time without time zone
	F54_interval       interval NOT NULL,

	F60_boolean boolean DEFAULT true NOT NULL,
	F61_integer integer
);
COMMENT ON TABLE Test_Types IS '(Test_Type) All Supported Types';
COMMENT ON COLUMN Test_Types.F00_smallint IS 'Translated to Ada Short_Integer';
COMMENT ON COLUMN Test_Types.F01_integer IS 'Translated to Ada Integer';
COMMENT ON COLUMN Test_Types.F02_integer IS 'Translated to Ada Integer';
COMMENT ON COLUMN Test_Types.F03_bigint IS 'Translated to Ada Long_Long_Integer';
COMMENT ON COLUMN Test_Types.F11_double_precision IS 'Translated to Ada Long_Float';
COMMENT ON COLUMN Test_Types.F12_Real IS 'Translated to Ada Float';
COMMENT ON COLUMN Test_Types.F13_Float IS 'Translated to Ada Long_Float';
COMMENT ON COLUMN Test_Types.F20_numeric_24_8 IS 'Translated to Ada Numeric_24_8';
COMMENT ON COLUMN Test_Types.F21_numeric_8_4 IS 'Translated to Ada Numeric_8_4';
COMMENT ON COLUMN Test_Types.F22_numeric IS 'Translated to Ada Float (error!!!)';
COMMENT ON COLUMN Test_Types.F23_decimal IS 'Translated to Ada Float (error!!!)';
COMMENT ON COLUMN Test_Types.F24_money IS 'Translated to Ada T_Monery';
COMMENT ON COLUMN Test_Types.F30_char3 IS 'Translated to Ada String';
COMMENT ON COLUMN Test_Types.F31_varchar5 IS 'Translated to Ada Unbounded_String';
COMMENT ON COLUMN Test_Types.F32_varchar IS 'Translated to Ada Unbounded_String';
COMMENT ON COLUMN Test_Types.F33_text IS 'Translated to Ada Unbounded_String';
COMMENT ON COLUMN Test_Types.F40_timestamp_notz IS 'Translated to Ada Calendar.Time';
COMMENT ON COLUMN Test_Types.F41_timestamp_tz IS 'Translated to Ada Calendar.Time';
COMMENT ON COLUMN Test_Types.F42_timestamp IS 'Translated to Ada Calendar.Time';
COMMENT ON COLUMN Test_Types.F50_date IS 'Translated to Ada Calendar.Time';
COMMENT ON COLUMN Test_Types.F54_interval IS 'Translated to Ada Duration';
COMMENT ON COLUMN Test_Types.F60_boolean IS 'Translated to Ada Triboolean';
COMMENT ON COLUMN Test_Types.F61_integer IS 'Translated to Ada Integer';

-- ALTER TABLE ONLY Test_Types ADD CONSTRAINT Test_Types_PK PRIMARY KEY (F01_integer);

CREATE INDEX Test_Types1_idx ON Test_Types USING btree (F30_char3, F31_varchar5);

insert into Test_Types values (
1,                         -- F00_smallint
-2,                        -- F01_integer
89,                        -- F02_integer
-4,                        -- F03_bigint

-- -1.234,                    -- F10_Float_24
1.134,                     -- F11_double_precision
10.435,                    -- F12_real
-10.435,                   -- F13_Float

23.12345678,               -- F20_numeric_24_8
23.1234,                   -- F21_numeric_8_4
10.432,                    -- F22_numeric
-10.432,                   -- F23_decimal
-123456789.56,                    -- F24_money

'def',                     -- F30_char3
'456',                     -- F31_varchar5
'123',                     -- F32_varchar
'1234567890',              -- F33_text

'2022-04-18 13:55:05.32',  -- F40_timestamp_notz
'2022-04-18 13:55:05.32',  -- F41_timestamp_tz
'2022-04-18 13:55:05.32',  -- F42_timestamp

'2022-04-18',              -- F50_date
                           -- F51_time_notz
                           -- F52_time_tz
                           -- F53_time
'120.34',                  -- F54_interval

True,                      -- F60_boolean
89                         -- F61_Integer
);

