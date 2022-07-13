------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2021, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.SQL;          use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Postgres;
with GNATCOLL.SQL.Inspect;  use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with Test1_Database;        use Test1_Database;
with Test1_ORM;             use Test1_ORM;
with Ada.Text_IO;           use Ada.Text_IO;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

with GNAT.Regexp;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;          use Ada.Calendar;
with Gnat.Calendar.Time_IO;

procedure test1 is
   package GCTIO renames GNAT.Calendar.Time_IO;

   Descr  : Database_Description :=
     GNATCOLL.SQL.Postgres.Setup (Database => "test1",
                                  Host     => "localhost",
                                  Port     => 5433,
                                  User     => "postgres",
                                  Password => "Nosoples1");
begin

   GNATCOLL.Traces.Parse_Config_File (".gnatdebug"); -- show traces
   GNATCOLL.SQL.Sessions.Setup (Descr => Descr, Max_Sessions => 2);

   declare
      Session : constant Session_Type := Get_New_Session;
      T1      : Test_Type_List;
      F00_Smallint         : Short_Integer;
      F01_Integer          : Integer;
      F02_Integer          : Integer;
      F03_Bigint           : Long_Long_Integer;

      F11_Double_Precision : Long_Float;
      F12_Real             : Float;
      F13_Float            : Long_Float;

      F20_Numeric_24_8     : T_Numeric_24_8;
      F21_Numeric_8_4      : T_Numeric_8_4;
      F22_Numeric          : Float;
      F23_Decimal          : Float;
      F24_Money            : T_Money;

      F30_Char3            : String (1 .. 3);
      F31_Varchar5         : Unbounded_String;
      F32_Varchar          : Unbounded_String;
      F33_Text             : Unbounded_String;

      F40_Timestamp_Notz   : Ada.Calendar.Time;
      F41_Timestamp_tz     : Ada.Calendar.Time;
      F42_Timestamp        : Ada.Calendar.Time;

      F50_Date             : Ada.Calendar.Time;
      F54_Interval         : Duration;

      F60_Boolean          : Boolean;
   begin
      Execute (Connection => DB (Session), Query => "set schema 'schema_test1';");

      T1 := All_Test_Types.Get (Session);
      Put_Line ("===================");
      while T1.Has_Row loop

         F00_Smallint := T1.Element.F00_Smallint;
         Put_Line ("F00_smallint: "        & T1.Element.F00_Smallint'Image);

         F01_Integer := T1.Element.F01_Integer;
         Put_Line ("F01_integer: "          & T1.Element.F01_integer'Image);

         F02_Integer := T1.Element.F02_Integer;
         Put_Line ("F02_integer: "          & F02_Integer'Image);

         F03_Bigint := T1.Element.F03_Bigint;
         Put_Line ("F03_bigint: "           & T1.Element.F03_bigint'Image);

         F11_Double_Precision := T1.Element.F11_Double_Precision;
         Put_Line ("F11_double_precision: " & T1.Element.F11_Double_Precision'Image);

         F12_Real := T1.Element.F12_Real;
         Put_Line ("F12_Real: "             & T1.Element.F12_real'Image);

         F13_Float := T1.Element.F13_Float; -- long float
         Put_Line ("F13_Float: "            & T1.Element.F13_Float'Image);

         F20_Numeric_24_8 := T1.Element.F20_Numeric_24_8;
         Put_Line ("F20_numeric_24_8: "     & T1.Element.F20_numeric_24_8'Image);

         F21_Numeric_8_4 := T1.Element.F21_Numeric_8_4;
         Put_Line ("F21_numeric_8_4: "      & T1.Element.F21_numeric_8_4'Image);

         F22_Numeric := T1.Element.F22_Numeric; -- float
         Put_Line ("F22_numeric: "          & T1.Element.F22_numeric'Image);

         F23_Decimal := T1.Element.F23_Decimal; -- float
         Put_Line ("F23_decimal: "          & T1.Element.F23_decimal'Image);

         F24_money:= T1.Element.F24_Money;
         Put_Line ("F24_money: "            & T1.Element.F24_money'Image);

         F30_Char3 := T1.Element.F30_Char3;
         Put_Line ("F30_char3: "            & T1.Element.F30_char3);

         F31_Varchar5 := To_Unbounded_String (T1.Element.F31_Varchar5);
         Put_Line ("F31_varchar5: "         & T1.Element.F31_varchar5);

         F32_Varchar := To_Unbounded_String (T1.Element.F32_Varchar);
         Put_Line ("F32_varchar: "          & T1.Element.F32_Varchar);

         F33_Text := To_Unbounded_String (T1.Element.F33_Text);
         Put_Line ("F33_text: "             & T1.Element.F33_text);

         F40_Timestamp_Notz := T1.Element.F40_Timestamp_Notz;
         Put_Line ("F40_timestamp_notz: "   &
           GCTIO.Image (T1.Element.F40_Timestamp_Notz, "%Y-%m-%d %H:%M:%S.%e"));

         F41_Timestamp_Tz := T1.Element.F41_Timestamp_Tz;
         Put_Line ("F41_timestamp_tz: "     &
           GCTIO.Image (T1.Element.F41_Timestamp_tz, "%Y-%m-%d %H:%M:%S.%e"));

         F42_Timestamp := T1.Element.F42_Timestamp;
         Put_Line ("F42_timestamp: "        &
           GCTIO.Image (T1.Element.F42_Timestamp, "%Y-%m-%d %H:%M:%S.%e"));

         F50_Date := T1.Element.F50_Date;
         Put_Line ("F50_date: "             &
           GCTIO.Image (T1.Element.F50_Date, "%Y-%m-%d"));

         F54_Interval := T1.Element.F54_Interval;
         Put_Line ("F54_Interval: "         & T1.Element.F54_Interval'Image);

         F60_Boolean := T1.Element.F60_Boolean;
         Put_Line ("F60_boolean: "          & T1.Element.F60_boolean'Image);

         Put_Line ("===================");
         T1.Next;
      end loop;

   end;
   ---------------------------------------------------
   --  Free memory
   ---------------------------------------------------

   GNATCOLL.Traces.Finalize;
end test1;
