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
--  with Test9_Database;        use Test9_Database;
with Test9_ORM_New;         use Test9_ORM_New;
with Ada.Text_IO;           use Ada.Text_IO;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

with GNAT.Regexp;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;          use Ada.Calendar;
with Gnat.Calendar.Time_IO;

procedure test9 is
   package GCTIO renames GNAT.Calendar.Time_IO;

   Descr  : Database_Description :=
     GNATCOLL.SQL.Postgres.Setup (Database => "test9",
                                  Host     => "localhost",
                                  Port     => 5433,
                                  User     => "postgres",
                                  Password => "Nosoples1");
begin

   GNATCOLL.Traces.Parse_Config_File (".gnatdebug"); -- show traces
   GNATCOLL.SQL.Sessions.Setup (Descr => Descr, Max_Sessions => 2);

   declare
      Session : constant Session_Type := Get_New_Session;
      BList   : Bank_List;
      PList   : Person_List;
      DPerson : Detached_Person;
      DBank   : Detached_Bank;
      P : Person;
      B : Bank;
   begin
      Execute (Connection => DB (Session), Query => "set schema 'schema_test9';");

      Put_Line ("===== Banks =====");
      BList := All_Banks.Get (Session);
      while BList.Has_Row loop
         Put_Line (BList.Element.BIC & " " &
                     BList.Element.IBAN & " " &
                     BList.Element.Office'Image & " " &
                     BList.Element.City);
         BList.Next;
      end loop;
      Put_Line ("===== Persons =====");
      PList := All_Persons.Get (Session);
      while PList.Has_Row loop
         P := PList.Element;
         B := Bank (Banks (P));
         Put_Line (PList.Element.Name & " " &
                     PList.Element.Bank_BIC & " " &
                     PList.Element.Bank_IBAN & " " &
                     PList.Element.Amount'Image & " " &
                     P.Car_Maker & " " &
                     P.Car_Model & " " &
                     B.Iban
                  );
         PList.Next;
      end loop;

      Put_Line ("===== Get Bank/City of a Person =====");
      DPerson := Detached_Person (Get_Person (Session, "Elon"));
      DBank   := Detached_Bank (Banks (DPerson));
      Put_line (DPerson.Name & " => " & DBank.BIC & " " & DBank.City);

      Put_Line ("===== Change Bank of a Person =====");
      DPerson := Detached_Person (Get_Person (Session, "Elon"));
      Session.Begin_Transaction;
      Set_Bank_Bic (DPerson, "ES20");
      Set_Bank_Iban (DPerson, "2001");
      Session.Persist (DPerson);
      Session.Flush;
      Session.Commit;

      Put_Line ("===== Get Again Bank/City of a Person =====");
      DPerson := Detached_Person (Get_Person (Session, "Elon"));
      DBank   := Detached_Bank (Banks (DPerson));
      Put_line (DPerson.Name & " => " & DBank.BIC & " " & DBank.City);

      Put_Line ("===== Restore Bank of a Person =====");
      DPerson := Detached_Person (Get_Person (Session, "Elon"));
      Session.Begin_Transaction;
      Set_Bank_Bic (DPerson, "ES22");
      Set_Bank_Iban (DPerson, "2201");
      Session.Persist (DPerson);
      Session.Flush;
      Session.Commit;

      Put_Line ("===== Get Again Bank/City of a Person =====");
      DPerson := Detached_Person (Get_Person (Session, "Elon"));
      DBank   := Detached_Bank (Banks (DPerson));
      Put_line (DPerson.Name & " => " & DBank.BIC & " " & DBank.City);

   end;
   ---------------------------------------------------
   --  Free memory
   ---------------------------------------------------

   GNATCOLL.Traces.Finalize;
end test9;
