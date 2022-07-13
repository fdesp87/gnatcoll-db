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
--with Test2_Database;        use Test2_Database;
with Test2_ORM_New;         use Test2_ORM_New;
with Ada.Text_IO;           use Ada.Text_IO;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

with GNAT.Regexp;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;          use Ada.Calendar;
with Gnat.Calendar.Time_IO;

procedure test2 is
   package GCTIO renames GNAT.Calendar.Time_IO;

   Session : Session_Type;
   Descr   : Database_Description :=
     GNATCOLL.SQL.Postgres.Setup (Database => "test2",
                                  Host     => "localhost",
                                  Port     => 5433,
                                  User     => "postgres",
                                  Password => "Nosoples1");
begin

   GNATCOLL.Traces.Parse_Config_File (".gnatdebug"); -- show traces
   GNATCOLL.SQL.Sessions.Setup (Descr        => Descr,
                                Max_Sessions => 2,
                                Flush_Before_Query => True);
   Session := Get_New_Session;
   Execute (Connection => DB (Session), Query => "set schema 'schema_test2';");

   declare
      S_List  : Ship_List;
      F_List  : Flag_List;
      C_List  : Captain_List;
      V_List  : Full_Ship_View_List;
      F  : Detached_Flag'Class := No_Detached_Flag;
      C  : Detached_Captain'Class := No_Detached_Captain;
      S  : Detached_Ship'Class := No_Detached_Ship;
      FS : Detached_Flag'Class := No_Detached_Flag;
      SF_List : Ship_List;
      V  : Detached_Full_Ship_View'Class := No_Detached_Full_Ship_View;
      VM : Full_Ships_View_Managers;
   begin

      Session.Begin_Transaction;

      F := New_Flag (Iso_Number => 800);
      F.Set_Country ("Russia Fed");
      F.Set_Currency ("RUB");
      Session.Persist (F);
      Session.Flush;
      Put_Line ("=== Inserted Flag 800 ===");

      C := New_Captain (Capt_Id => "666");
      C.Set_Capt_Name ("V. Putin");
      C.Set_Capt_Phone ("+8.MOSKA");
      Session.Persist (C);
      Session.Flush;
      Put_Line ("=== Inserted Capt 29716149B ===");

      S := New_Ship (IMO_Id => "RF-001");
      S.Set_Name ("SPUTNIK");
      S.Set_Tons (1);
      S.Set_Current_Flag (F);    -- also direct value is supported
      S.Set_Current_Captain (C); -- also direct value is supported
      Session.Persist (S);
      Session.Flush;
      Put_Line ("=== Inserted Ship RF-001 ===");

      Session.Commit;

      Put_Line ("=== Ships ===");
      S_List := All_Ships.Get (Session);
      while S_List.Has_Row loop
         Put (S_List.Element.IMO_Id'Image & ", ");
         Put (S_List.Element.Name'Image & ", ");
         Put (S_List.Element.Tons'Image & ", ");
         Put (Integer'(S_List.Element.Current_Flag)'Image & ", ");
         Put (String'(S_List.Element.Current_Captain)'Image);
         New_Line;
         S_List.Next;
      end loop;

      Put_Line ("=== Flags ===");
      F_List := All_Flags.Get (Session);
      while F_List.Has_Row loop
         Put (F_List.Element.Iso_Number'Image & ", ");
         Put (F_List.Element.Country'Image & ", ");
         Put (F_List.Element.Currency'Image);
         New_Line;
         F_List.Next;
      end loop;

      Put_Line ("== Captains ==");
      C_List := All_Captains.Get (Session);
      while C_List.Has_Row loop
         Put (C_List.Element.Capt_Id'Image & ", ");
         Put (C_List.Element.Capt_Name'Image & ", ");
         Put (C_List.Element.Capt_Phone'Image);
         New_Line;
         C_List.Next;
      end loop;

      Put_Line ("== Ships with Flag 840 ==");
      FS := Get_Flag (Session, Iso_Number => 840);
      SF_List := FS.Ships_With_Flag.Get (Session);
      while SF_List.Has_Row loop
         Put (SF_List.Element.IMO_Id'Image & ", ");
         Put (SF_List.Element.Name'Image & ", ");
         Put (SF_List.Element.Tons'Image & ", ");
         Put (Integer'(SF_List.Element.Current_Flag)'Image & ", ");
         Put (String'(SF_List.Element.Current_Captain)'Image);
         New_Line;
         SF_List.Next;
      end loop;

      Put_Line ("=== View Ship ===");
      V_List := All_Full_Ships_View.Get (Session);
      while V_List.Has_Row loop
         Put (V_List.Element.IMO_Id'Image & ", ");
         Put (V_List.Element.Name'Image & ", ");
         Put (V_List.Element.Tons'Image & ", ");
         Put (V_List.Element.Capt_Name'Image & ", ");
         New_Line;
         V_List.Next;
      end loop;

      Put_Line ("== Getting selected records of the ship view ==");
      V_List := All_Full_Ships_View.Filter (IMO_Id => "US1").Get (Session);
      while V_List.Has_Row loop
         Put (V_List.Element.IMO_Id'Image & ", ");
         Put (V_List.Element.Name'Image & ", ");
         Put (V_List.Element.Tons'Image & ", ");
         Put (V_List.Element.Capt_Name'Image & ", ");
         New_Line;
         V_List.Next;
      end loop;

      V_List := All_Full_Ships_View.Filter (IMO_Id => "US83").Get (Session);
      while V_List.Has_Row loop
         Put (V_List.Element.IMO_Id'Image & ", ");
         Put (V_List.Element.Name'Image & ", ");
         Put (V_List.Element.Tons'Image & ", ");
         Put (V_List.Element.Capt_Name'Image & ", ");
         New_Line;
         V_List.Next;
      end loop;

      Session.Begin_Transaction;
      S.Delete;
      Put_Line ("==== deleted Ship RF-001 ===");
      C.Delete;
      Put_Line ("==== deleted Captain 666 ===");
      F.Delete;
      Put_Line ("==== deleted Flag 800 ===");
      Session.Commit;
   end;
   ---------------------------------------------------
   --  Free memory
   ---------------------------------------------------

   GNATCOLL.Traces.Finalize;
end test2;
