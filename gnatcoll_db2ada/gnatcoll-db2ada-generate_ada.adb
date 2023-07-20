with Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with GNAT.Strings;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with GNATCOLL.Utils;         use GNATCOLL.Utils;

package body GNATCOLL.Db2Ada.Generate_Ada is
   Internal_Updatable_Views : Boolean := False;

   Max_Depth : constant := 3;
   --  Maximum valid value for Select_Related
   --  No need in putting this too high, since that will generate make
   --  queries much bigger, thus invalidating any saving we might have
   --  in doing only one of them
   type Counts_Array is array (0 .. Max_Depth) of Integer;

   package TIO renames Ada.Text_IO;
   package AC  renames Ada.Containers;

   ----------------------------------------------------------------------
   --  Mapping SQL_Types to Ada Types
   ----------------------------------------------------------------------
   package Mapping_SQL_Types_To_Ada_Types is
      procedure Register_Ada_Types;

      function Is_Integer (F : Field) return Boolean;
      function SQL_Field  (F : Field) return String;
      function Ada_Field  (F : Field) return String;
      function Ada_Return (F : Field) return String;
      function Ada_Param  (F : Field) return String;
      function Ada_Default_Record (F : Field) return String;
      function Value_From_DB (F : Field) return String;
      function Ada_Default_Param  (F : Field) return String;
   end Mapping_SQL_Types_To_Ada_Types;

   package body Mapping_SQL_Types_To_Ada_Types is
      type Ada_Type_Block is record
         Ada_Field      : GNAT.Strings.String_Access; -- Ada name
         Ada_Return     : GNAT.Strings.String_Access; -- Ada return
         Ada_Param      : GNAT.Strings.String_Access; -- Ada parameter
         Default_Record : GNAT.Strings.String_Access; -- Ada record field
         Default_Param  : GNAT.Strings.String_Access; -- Def. parameter value
         Value_From_DB  : GNAT.Strings.String_Access; -- value from database
      end record;
      package Ada_Types_Maps is new AC.Indefinite_Ordered_Maps
        (String,                      --  corresponding to SQL_Field_xxx
         Ada_Type_Block, "<", "=");
      Ada_Types : Ada_Types_Maps.Map;

      procedure Register_Ada_Types is
         function SA (Source : String) return GNAT.Strings.String_Access;
         function SA (Source : String) return GNAT.Strings.String_Access is
         begin
            return new String'(Source);
         end SA;
      begin
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Text",
            Ada_Type_Block'(Ada_Field      => SA ("Unbounded_String"),
                            Ada_Return     => SA ("String"),
                            Ada_Param      => SA ("String"),
                            Value_From_DB  => SA ("String_Value"),
                            Default_Record => SA ("Null_Unbounded_String"),
                            Default_Param  => SA ("No_Update")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Integer",
            Ada_Type_Block'(Ada_Field      => SA ("Integer"),
                            Ada_Return     => SA ("Integer"),
                            Ada_Param      => SA ("Integer"),
                            Value_From_DB  => SA ("Integer_Value"),
                            Default_Record => SA ("Integer'First"),
                            Default_Param  => SA ("Integer'First")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Interval",
            Ada_Type_Block'(Ada_Field      => SA ("Duration"),
                            Ada_Return     => SA ("Duration"),
                            Ada_Param      => SA ("Duration"),
                            Value_From_DB  => SA ("Interval_Value"),
                            Default_Record => SA ("Duration'First"),
                            Default_Param  => SA ("Duration'First")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Bigint",
            Ada_Type_Block'(Ada_Field      => SA ("Long_Long_Integer"),
                            Ada_Return     => SA ("Long_Long_Integer"),
                            Ada_Param      => SA ("Long_Long_Integer"),
                            Value_From_DB  => SA ("Bigint_Value"),
                            Default_Record => SA ("Long_Long_Integer'First"),
                            Default_Param  => SA ("Long_Long_Integer'First")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Smallint",
            Ada_Type_Block'(Ada_Field      => SA ("Short_Integer"),
                            Ada_Return     => SA ("Short_Integer"),
                            Ada_Param      => SA ("Short_Integer"),
                            Value_From_DB  => SA ("Smallint_Value"),
                            Default_Record => SA ("Short_Integer'First"),
                            Default_Param  => SA ("Short_Integer'First")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Double_Precision",
            Ada_Type_Block'(Ada_Field      => SA ("Long_Float"),
                            Ada_Return     => SA ("Long_Float"),
                            Ada_Param      => SA ("Long_Float"),
                            Value_From_DB  => SA ("Long_Float_Value"),
                            Default_Record => SA ("Long_Float'First"),
                            Default_Param  => SA ("Long_Float'First")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Long_Float",
            Ada_Type_Block'(Ada_Field      => SA ("Long_Float"),
                            Ada_Return     => SA ("Long_Float"),
                            Ada_Param      => SA ("Long_Float"),
                            Value_From_DB  => SA ("Long_Float_Value"),
                            Default_Record => SA ("Long_Float'First"),
                            Default_Param  => SA ("Long_Float'First")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Real",
            Ada_Type_Block'(Ada_Field      => SA ("Float"),
                            Ada_Return     => SA ("Float"),
                            Ada_Param      => SA ("Float"),
                            Value_From_DB  => SA ("Float_Value"),
                            Default_Record => SA ("Float'First"),
                            Default_Param  => SA ("Float'First")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Time",
            Ada_Type_Block'(Ada_Field      => SA ("Ada.Calendar.Time"),
                            Ada_Return     => SA ("Ada.Calendar.Time"),
                            Ada_Param      => SA ("Ada.Calendar.Time"),
                            Value_From_DB  => SA ("Time_Value"),
                            Default_Record => SA ("No_Time"),
                            Default_Param  => SA ("No_Time")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Date",
            Ada_Type_Block'(Ada_Field      => SA ("Ada.Calendar.Time"),
                            Ada_Return     => SA ("Ada.Calendar.Time"),
                            Ada_Param      => SA ("Ada.Calendar.Time"),
                            Value_From_DB  => SA ("Time_Value"),
                            Default_Record => SA ("No_Time"),
                            Default_Param  => SA ("No_Time")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Float",
            Ada_Type_Block'(Ada_Field      => SA ("Float"),
                            Ada_Return     => SA ("Float"),
                            Ada_Param      => SA ("Float"),
                            Value_From_DB  => SA ("Float_Value"),
                            Default_Record => SA ("Float'First"),
                            Default_Param  => SA ("Float'First")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Boolean",
            Ada_Type_Block'(Ada_Field      => SA ("Boolean"),
                            Ada_Return     => SA ("Boolean"),
                            Ada_Param      => SA ("Triboolean"),
                            Value_From_DB  => SA ("Boolean_Value"),
                            Default_Record => SA ("False"),
                            Default_Param  => SA ("Indeterminate")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Money",
            Ada_Type_Block'(Ada_Field      => SA ("GNATCOLL.SQL.T_Money"),
                            Ada_Return     => SA ("GNATCOLL.SQL.T_Money"),
                            Ada_Param      => SA ("GNATCOLL.SQL.T_Money"),
                            Value_From_DB  => SA ("Money_Value"),
                            Default_Record =>
                              SA ("GNATCOLL.SQL.T_Money'First"),
                            Default_Param  =>
                              SA ("GNATCOLL.SQL.T_Money'First")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Numeric_24_8",
            Ada_Type_Block'(Ada_Field      =>
                                SA ("GNATCOLL.SQL.T_Numeric_24_8"),
                            Ada_Return     =>
                              SA ("GNATCOLL.SQL.T_Numeric_24_8"),
                            Ada_Param      =>
                              SA ("GNATCOLL.SQL.T_Numeric_24_8"),
                            Value_From_DB  => SA ("Numeric_24_8_Value"),
                            Default_Record =>
                              SA ("GNATCOLL.SQL.T_Numeric_24_8'First"),
                            Default_Param  =>
                              SA ("GNATCOLL.SQL.T_Numeric_24_8'First")));
         Ada_Types_Maps.Insert
           (Ada_Types, "SQL_Field_Numeric_8_4",
            Ada_Type_Block'(Ada_Field      =>
                                SA ("GNATCOLL.SQL.T_Numeric_8_4"),
                            Ada_Return     =>
                              SA ("GNATCOLL.SQL.T_Numeric_8_4"),
                            Ada_Param      =>
                              SA ("GNATCOLL.SQL.T_Numeric_8_4"),
                            Value_From_DB  => SA ("Numeric_8_4_Value"),
                            Default_Record =>
                              SA ("GNATCOLL.SQL.T_Numeric_8_4'First"),
                            Default_Param  =>
                              SA ("GNATCOLL.SQL.T_Numeric_8_4'First")));
      end Register_Ada_Types;

      function Is_Integer (F : Field) return Boolean is
      begin
         return (Ada_Field (F) = "Short_Integer") or else
           (Ada_Field (F) = "Integer") or else
           (Ada_Field (F) = "Long_Long_Integer");
      end Is_Integer;

      function SQL_Field  (F : Field) return String is
         FMA : constant Field_Mapping_Access := F.Get_Type;
         Str : constant String := FMA.Type_To_SQL (null, False);
      begin
         return Str;
      end SQL_Field;

      function Ada_Field (F : Field) return String is
         FMA : constant Field_Mapping_Access := F.Get_Type;
         Str : constant String := FMA.Type_To_SQL (null, False);
      begin
         return Ada_Types_Maps.Element (Ada_Types, Str).Ada_Field.all;
      end Ada_Field;

      function Ada_Return (F : Field) return String is
         FMA : constant Field_Mapping_Access := F.Get_Type;
         Str : constant String := FMA.Type_To_SQL (null, False);
      begin
         return Ada_Types_Maps.Element (Ada_Types, Str).Ada_Return.all;
      end Ada_Return;

      function Ada_Param (F : Field) return String is
         FMA : constant Field_Mapping_Access := F.Get_Type;
         Str : constant String := FMA.Type_To_SQL (null, False);
      begin
         return Ada_Types_Maps.Element (Ada_Types, Str).Ada_Param.all;
      end Ada_Param;

      function Ada_Default_Record (F : Field) return String is
         FMA : constant Field_Mapping_Access := F.Get_Type;
         Str : constant String := FMA.Type_To_SQL (null, False);
      begin
         return Ada_Types_Maps.Element (Ada_Types, Str).Default_Record.all;
      end Ada_Default_Record;

      function Ada_Default_Param (F : Field) return String is
         FMA : constant Field_Mapping_Access := F.Get_Type;
         Str : constant String := FMA.Type_To_SQL (null, False);
      begin
         return Ada_Types_Maps.Element (Ada_Types, Str).Default_Param.all;
      end Ada_Default_Param;

      function Value_From_DB (F : Field) return String is
         FMA : constant Field_Mapping_Access := F.Get_Type;
         Str : constant String := FMA.Type_To_SQL (null, False);
      begin
         return Ada_Types_Maps.Element (Ada_Types, Str).Value_From_DB.all;
      end Value_From_DB;
   end Mapping_SQL_Types_To_Ada_Types;
   use Mapping_SQL_Types_To_Ada_Types;

   ----------------------------------------------------------------------
   --  Containers of tables and foreign keys
   ----------------------------------------------------------------------
   package Foreign_Keys is
      --  register the FKs
      procedure Register_FKs (Schema : DB_Schema);

      --      function Is_Pointing (T : Table_Description) return Boolean;
      function Is_Pointed (T : Table_Description) return Boolean;
      --  return true if this table T is pointed by some FK

      function Pointed_Table (FK : Field) return Table_Description;
      --  return the table pointed by FK.

      function Pointed_Field (FK : Field) return Field;
      --  return the field pointed by FK.

      function Pointer_Field (PK         : Field; -- in pointed table
                              Pter_Table : Table_Description) return Field;
      --  return the field in Pter_Table that points to PK in a pointed table

      function Pointer_Order (FK : Field) return Integer;
      --  return the order of this FK in the declaration of its table.
      --  Starts at 0. If FK is not foreign kay, returns -1.

      function Rev_Relation (FK : Field) return String;
      --  return the reverse relation of FK.
      --  If FK is not foreing key, return "". If FK has not declared reverse
      --  relation, a synthetic reverse relation is build by using
      --  <table_name>_<field_name>_Id.

      procedure For_Each_FK (T        : Table_Description;
                             Callback : access procedure (F : in out Field));
      --  call "Callback" for each FK declared in table T.
      --  the field in this callback is a pointer field in this table.

      procedure For_Each_RFK (T        : Table_Description;
                              Callback : access procedure (F : in out Field));
      --  call "Callback" for each FK belonging to other table and this FK
      --  points to some field in table T.
      --  the field in this callback is the pointer field of another table.
   end Foreign_Keys;

   package body Foreign_Keys is
      type FK_Info is record
         Pointing_Table : Table_Description; --  in which table
         Pointer_Field  : Field;   --  this is the field pointing
         Pointer_Order  : Integer; --  FK entry order
         Pointed_Table  : Table_Description; --  to which table
         Pointed_Field  : Field; --  this field is pointed by some other
         Rev_Relation   : GNAT.Strings.String_Access;
      end record;
      package FK_Lists is new AC.Vectors (Natural, FK_Info);

      type FK_Info_Block is record
         TB    : Table_Description; --  pointed table
         FK    : FK_Lists.Vector;   --  list of FK info of pointing table
      end record;
      package TablesFK_Maps is new AC.Indefinite_Ordered_Maps
        (String,                    --  pointing table.Name
         FK_Info_Block, "<", "=");

      --  The container
      TablesFK : TablesFK_Maps.Map;

      procedure Register_FKs (Schema : DB_Schema) is
         procedure Process_Field (F : in out Field);
         procedure Process_Field (F : in out Field) is
            CursT : TablesFK_Maps.Cursor;
            FKIB  : FK_Info_Block;
            FKI   : FK_Info;
            F0    : Field;
            T     : Table_Description;
         begin
            F0 := F.Is_FK;
            if F0 = No_Field then
               return;
            end if;
            T := Table_Description (F.Get_Table);
            CursT := TablesFK.Find (T.Name);
            if not TablesFK_Maps.Has_Element (CursT) then
               --  prepare FKI
               FKI.Pointer_Field  := F;
               FKI.Pointer_Order  := Integer (FKIB.FK.Length);
               FKI.Pointed_Field  := F0;
               FKI.Pointing_Table := T;
               FKI.Pointed_Table  := Table_Description (F0.Get_Table);
               FKI.Rev_Relation   := F.Reverse_Relation;
               --  prepare FKIB
               FKIB.TB := T;
               --  link FKI in FKIB
               FKIB.FK.Append (FKI);
               --  link FKIB
               TablesFK_Maps.Insert (TablesFK, T.Name, FKIB);
            else
               FKIB := TablesFK_Maps.Element (CursT);
               TablesFK_Maps.Delete (TablesFK, CursT);
               --  prepare FKI
               FKI.Pointer_Field  := F;
               FKI.Pointer_Order  := Integer (FKIB.FK.Length);
               FKI.Pointed_Field  := F0;
               FKI.Pointing_Table := T;
               FKI.Pointed_Table  := Table_Description (F0.Get_Table);
               FKI.Rev_Relation   := F.Reverse_Relation;
               --  link FKI in FKIB
               FKIB.FK.Append (FKI);
               --  link FKIB
               TablesFK_Maps.Insert (TablesFK, T.Name, FKIB);
            end if;
         end Process_Field;

         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
         begin
            For_Each_Field (T, Process_Field'Access, True);
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, False);
      end Register_FKs;

      --  function Is_Pointing (T : Table_Description) return Boolean is
      --     CursT : constant TablesFK_Maps.Cursor := TablesFK.Find (T.Name);
      --  begin
      --     return TablesFK_Maps.Has_Element (CursT);
      --  end Is_Pointing;

      function Pointer_Field (PK         : Field; -- in pointed table
                              Pter_Table : Table_Description) return Field is
         Found  : Boolean := False;
         The_FK : Field := No_Field;
         procedure Visit_FK (FK : in out Field);
         procedure Visit_FK (FK : in out Field) is
         begin
            if Found then
               return;
            end if;
            if Pointed_Field (FK)  = PK then
               The_FK := FK;
               Found := True;
            end if;
         end Visit_FK;
      begin
         For_Each_FK (Pter_Table, Visit_FK'Access);
         return The_FK;
      end Pointer_Field;

      function Is_Pointed (T : Table_Description) return Boolean is
         CursT : TablesFK_Maps.Cursor;
         CursF : FK_Lists.Cursor;
         FKIB  : FK_Info_Block;
         FKI   : FK_Info;
      begin
         CursT := TablesFK.First;
         while TablesFK_Maps.Has_Element (CursT) loop
            FKIB  := TablesFK_Maps.Element (CursT);
            CursF := FKIB.FK.First;
            while FK_Lists.Has_Element (CursF) loop
               FKI := FK_Lists.Element (CursF);
               if FKI.Pointed_Table = T then
                  return True;
               end if;
               FK_Lists.Next (CursF);
            end loop;
            TablesFK_Maps.Next (CursT);
         end loop;
         return False;
      end Is_Pointed;

      function Pointed_Table (FK : Field)
                              return Table_Description is
         T : constant Table_Description := Table_Description (FK.Get_Table);
         CursT : TablesFK_Maps.Cursor;
         CursF : FK_Lists.Cursor;
         FKIB  : FK_Info_Block;
         FKI   : FK_Info;
      begin
         if not Is_FK (FK) then
            return No_Table;
         end if;
         CursT := TablesFK.Find (T.Name);
         if not TablesFK_Maps.Has_Element (CursT) then
            return No_Table;
         end if;
         FKIB  := TablesFK_Maps.Element (CursT);
         CursF := FKIB.FK.First;
         while FK_Lists.Has_Element (CursF) loop
            FKI := FK_Lists.Element (CursF);
            if FKI.Pointer_Field = FK then
               return FKI.Pointed_Table;
            end if;
            FK_Lists.Next (CursF);
         end loop;
         return No_Table;
      end Pointed_Table;

      function Pointed_Field (FK : Field) return Field is
         CursT : TablesFK_Maps.Cursor;
         T : constant Table_Description := Table_Description (FK.Get_Table);
         CursF : FK_Lists.Cursor;
         FKIB  : FK_Info_Block;
         FKI   : FK_Info;
      begin
         if not Is_FK (FK) then
            return No_Field;
         end if;
         CursT := TablesFK.Find (T.Name);
         if not TablesFK_Maps.Has_Element (CursT) then
            return No_Field;
         end if;
         FKIB  := TablesFK_Maps.Element (CursT);
         CursF := FKIB.FK.First;
         while FK_Lists.Has_Element (CursF) loop
            FKI := FK_Lists.Element (CursF);
            if FKI.Pointer_Field = FK then
               return FKI.Pointed_Field;
            end if;
            FK_Lists.Next (CursF);
         end loop;
         return No_Field;
      end Pointed_Field;

      function Pointer_Order (FK : Field) return Integer is
         T : constant Table_Description := Table_Description (FK.Get_Table);
         CursT : TablesFK_Maps.Cursor;
         CursF : FK_Lists.Cursor;
         FKIB  : FK_Info_Block;
         FKI   : FK_Info;
      begin
         if not Is_FK (FK) then
            return -1;
         end if;
         CursT := TablesFK.Find (T.Name);
         if not TablesFK_Maps.Has_Element (CursT) then
            return -1;
         end if;
         FKIB  := TablesFK_Maps.Element (CursT);
         CursF := FKIB.FK.First;
         while FK_Lists.Has_Element (CursF) loop
            FKI := FK_Lists.Element (CursF);
            if FKI.Pointer_Field = FK then
               return FKI.Pointer_Order;
            end if;
            FK_Lists.Next (CursF);
         end loop;
         return -1;
      end Pointer_Order;

      function Rev_Relation (FK : Field) return String is
         CursT : TablesFK_Maps.Cursor;
         T : constant Table_Description := Table_Description (FK.Get_Table);
         CursF : FK_Lists.Cursor;
         FKIB  : FK_Info_Block;
         FKI   : FK_Info;
         use GNAT.Strings;
      begin
         if not Is_FK (FK) then
            return "";
         end if;
         CursT := TablesFK.Find (T.Name);
         if not TablesFK_Maps.Has_Element (CursT) then
            return "";
         end if;
         FKIB  := TablesFK_Maps.Element (CursT);
         CursF := FKIB.FK.First;
         while FK_Lists.Has_Element (CursF) loop
            FKI := FK_Lists.Element (CursF);
            if FKI.Pointer_Field = FK then
               if FKI.Rev_Relation = null or else
                 FKI.Rev_Relation.all = ""
               then
                  return Capitalize (FKI.Pointing_Table.Name) &
                    "_" & Capitalize (FK.Name) & "_Id";
               else
                  return FKI.Rev_Relation.all;
               end if;
            end if;
            FK_Lists.Next (CursF);
         end loop;
         return "";
      end Rev_Relation;

      function Get_First_FK (T : Table_Description) return Field;
      function Get_First_FK (T : Table_Description) return Field is
         CursT : TablesFK_Maps.Cursor;
         CursF : FK_Lists.Cursor;
         FKIB  : FK_Info_Block;
         FKI   : FK_Info;
      begin
         CursT := TablesFK.Find (T.Name);
         if not TablesFK_Maps.Has_Element (CursT) then
            return No_Field;
         end if;
         FKIB  := TablesFK_Maps.Element (CursT);
         CursF := FKIB.FK.First;
         if FK_Lists.Has_Element (CursF) then
            FKI := FK_Lists.Element (CursF);
            return FKI.Pointer_Field;
         else
            return No_Field;
         end if;
      end Get_First_FK;
      function Get_Next_FK (T : Table_Description; F : Field) return Field;
      function Get_Next_FK (T : Table_Description; F : Field) return Field is
         CursT : TablesFK_Maps.Cursor;
         CursF : FK_Lists.Cursor;
         FKIB  : FK_Info_Block;
         FKI   : FK_Info;
      begin
         CursT := TablesFK.Find (T.Name);
         if not TablesFK_Maps.Has_Element (CursT) then
            return No_Field;
         end if;
         FKIB  := TablesFK_Maps.Element (CursT);
         CursF := FKIB.FK.First;
         while FK_Lists.Has_Element (CursF) loop
            FKI := FK_Lists.Element (CursF);
            if FKI.Pointer_Field = F then
               FK_Lists.Next (CursF);
               if FK_Lists.Has_Element (CursF) then
                  FKI := FK_Lists.Element (CursF);
                  return FKI.Pointer_Field;
               else
                  return No_Field;
               end if;
            end if;
            FK_Lists.Next (CursF);
         end loop;
         return No_Field;
      end Get_Next_FK;

      procedure For_Each_FK (T        : Table_Description;
                             Callback : access procedure (F : in out Field))
      is
         F : Field;
      begin
         F := Get_First_FK (T);
         while F /= No_Field loop
            Callback (F);
            F := Get_Next_FK (T, F);
         end loop;
      end For_Each_FK;

      procedure For_Each_RFK (T        : Table_Description;
                              Callback : access procedure (F : in out Field))
      is
         CursT : TablesFK_Maps.Cursor;
         CursF : FK_Lists.Cursor;
         FKIB  : FK_Info_Block;
         FKI   : FK_Info;
      begin
         CursT := TablesFK.First;
         while TablesFK_Maps.Has_Element (CursT) loop
            FKIB  := TablesFK_Maps.Element (CursT);
            CursF := FKIB.FK.First;
            while FK_Lists.Has_Element (CursF) loop
               FKI := FK_Lists.Element (CursF);
               if FKI.Pointed_Table = T then
                  Callback (FKI.Pointer_Field);
               end if;
               FK_Lists.Next (CursF);
            end loop;
            TablesFK_Maps.Next (CursT);
         end loop;
      end For_Each_RFK;
   end Foreign_Keys;
   use Foreign_Keys;

   ----------------------------------------------------------------------
   --  Container of tables and all its fields (including inherited)
   --  alphabetically ordered
   --  All functions counts proper fields, PKs and FKs including the
   --  inherited onew
   ----------------------------------------------------------------------
   package Tables_Fields is
      procedure Register_Tables_And_Fields (Schema : DB_Schema);
      procedure For_Each_Table_Ordered_Field
        (T        : Table_Description;
         Callback : access procedure (F : in out Field));
      function Field_Order (T : Table_Description;
                            F : Field) return Integer;
      --  This is the entry order of the field, not the alphabetical order
      --  The order including supertable's fields after proper fields.
      --  Return -1 if field not in table
      function Num_Fields (T : Table_Description) return Integer;
      function Num_PKs (T : Table_Description) return Integer;
      function Has_PKs (T : Table_Description) return Boolean;
      function Num_FKs (T : Table_Description) return Integer;
      --  return the number of fields that are FK
      function Has_FKs (T : Table_Description) return Boolean;
      function Base_Key (T : Table_Description) return Long_Long_Integer;
      function Some_FK_Can_Be_Null (T : Table_Description) return Boolean;
      function Some_FK_Cannot_Be_Null (T : Table_Description) return Boolean;
      function Can_R_Fetch (T : Table_Description) return Boolean;
      function Last_FK_Can_Be_Null (T : Table_Description) return Integer;
      function Last_FK_Cannot_Be_Null (T : Table_Description) return Integer;
      function Has_Cache (T : Table_Description) return Boolean;
      function All_PKs_Autoincrement (T : Table_Description) return Boolean;
      --  This function also returns True if Num_PKs is 0
   end Tables_Fields;

   package body Tables_Fields is
      type Field_Block is record
         F  : Field;
         N  : Integer; -- entry order in the container, before sorting
      end record;
      package Ordered_Fields is new AC.Indefinite_Ordered_Maps
        (String,                 --  F.Name
         Field_Block, "<", "=");

      type Table_Block is record
         T         : Table_Description;
         FList     : Ordered_Fields.Map;
         NumF      : Integer := 0;
         NumPK     : Integer := 0;
         NumFK     : Integer := 0;
         AllPKsInt : Boolean := True;
         R_Fetch   : Boolean := True;
         Has_Cache : Boolean := True;
         Base_Key  : Long_Long_Integer := 0;
         SomeFK_Can_Be_Null    : Boolean := False;
         SomeFK_Cannot_Be_Null : Boolean := False;
         LastFK_Can_Be_Null    : Integer := 0;
         LastFK_Cannot_Be_Null : Integer := 0;
         AllPKs_Autoincrement  : Boolean := True;
      end record;
      package Table_Maps is new AC.Indefinite_Ordered_Maps
        (String,              -- T.Name
         Table_Block, "<", "=");

      TList : Table_Maps.Map;
      Current_Base_Key : Long_Long_Integer := 0;

      procedure Register_Tables_And_Fields (Schema : DB_Schema) is
         Num   : Integer;
         CursT : Table_Maps.Cursor;

         procedure Process_Field (F : in out Field);
         procedure Process_Field (F : in out Field) is
            IB    : Table_Block;
            FB    : Field_Block;
         begin
            IB := Table_Maps.Element (CursT);
            FB := Field_Block'(F, Num);
            IB.FList. Insert (F.Name, FB);
            Num := Num + 1;
            IB.NumF := IB.NumF + 1;
            if F.Is_PK then
               IB.NumPK := IB.NumPK + 1;
               if IB.AllPKsInt then
                  if not Is_Integer (F) then
                     IB.AllPKsInt := False;
                  end if;
               end if;
               if IB.NumPK > 1 then
                  IB.Has_Cache := False;
                  IB.R_Fetch   := False; -- Can't retrieve multi-key PK
               end if;
               if not Is_Integer (F) then
                  IB.Has_Cache := False;
                  IB.R_Fetch   := False; -- Can only retrieve integer PK
               end if;
               if not F.Is_Autoincrement then
                  IB.R_Fetch := False;   -- Can only retrieve auto/serial
               end if;
               IB.AllPKs_Autoincrement :=
                 IB.AllPKs_Autoincrement and F.Is_Autoincrement;
            end if;
            if F.Is_FK then
               IB.NumFK := IB.NumFK + 1;
               IB.SomeFK_Can_Be_Null :=
                 IB.SomeFK_Can_Be_Null or F.Can_Be_Null;
               IB.SomeFK_Cannot_Be_Null :=
                 IB.SomeFK_Cannot_Be_Null or (not F.Can_Be_Null);
               if F.Can_Be_Null then
                  IB.LastFK_Can_Be_Null := Integer'Max
                    (IB.LastFK_Can_Be_Null, FB.N);
               else
                  IB.LastFK_Cannot_Be_Null := Integer'Max
                    (IB.LastFK_Cannot_Be_Null, FB.N);
               end if;
            end if;
            TList.Replace_Element (CursT, IB);
         end Process_Field;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            IB    : Table_Block;
         begin
            CursT := TList.Find (T.Name);
            if not Table_Maps.Has_Element (CursT) then
               --  insert new table
               IB.Base_Key := Current_Base_Key;
               Current_Base_Key := Current_Base_Key + 1_000_000;
               IB.T := T;
               TList.Insert (T.Name, IB);
               --  now process all fields, including inherited
               CursT := TList.Find (T.Name);
               Num := 1;
               For_Each_Field (T, Process_Field'Access, True);
               --  last check for has_cache if NumPK still 0
               IB := Table_Maps.Element (CursT); -- as it was modified
               if IB.NumPK = 0 then
                  IB.Has_Cache := False;
                  TList.Replace_Element (CursT, IB);
               end if;
            else
               TIO.Put_Line ("Table " & T.Name & " already in List");
               raise Invalid_Table;
            end if;
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, False);
      end Register_Tables_And_Fields;

      procedure For_Each_Table_Ordered_Field
        (T        : Table_Description;
         Callback : access procedure (F : in out Field))
      is
         IB    : Table_Block;
         FB    : Field_Block;
         CursF : Ordered_Fields.Cursor;
         CursT : Table_Maps.Cursor;
      begin
         CursT := TList.Find (T.Name);
         if not Table_Maps.Has_Element (CursT) then
            return;
         end if;
         IB := Table_Maps.Element (TList, T.Name);
         CursF := IB.FList.First;
         while Ordered_Fields.Has_Element (CursF) loop
            FB := Ordered_Fields.Element (CursF);
            Callback (FB.F);
            CursF := Ordered_Fields.Next (CursF);
         end loop;
      end For_Each_Table_Ordered_Field;

      function Field_Order (T : Table_Description;
                            F : Field) return Integer is
         CursF : Ordered_Fields.Cursor;
         CursT : Table_Maps.Cursor;
         IB    : Table_Block;
      begin
         CursT := TList.Find (T.Name);
         if not Table_Maps.Has_Element (CursT) then
            return -1;
         end if;
         IB := Table_Maps.Element (TList, T.Name);
         CursF := IB.FList.Find (F.Name);
         if not Ordered_Fields.Has_Element (CursF) then
            return -1;
         else
            return Ordered_Fields.Element (CursF).N;
         end if;
      end Field_Order;

      function All_PKs_Autoincrement (T : Table_Description) return Boolean is
      begin
         return TList.Element (T.Name).AllPKs_Autoincrement;
      end All_PKs_Autoincrement;

      function Has_Cache (T : Table_Description) return Boolean is
      begin
         return TList.Element (T.Name).Has_Cache;
      end Has_Cache;

      function Base_Key (T : Table_Description) return Long_Long_Integer is
      begin
         return TList.Element (T.Name).Base_Key;
      end Base_Key;

      function Last_FK_Can_Be_Null (T : Table_Description) return Integer is
      begin
         return TList.Element (T.Name).LastFK_Can_Be_Null;
      end Last_FK_Can_Be_Null;

      function Last_FK_Cannot_Be_Null (T : Table_Description) return Integer is
      begin
         return TList.Element (T.Name).LastFK_Cannot_Be_Null;
      end Last_FK_Cannot_Be_Null;

      function Num_Fields (T : Table_Description) return Integer is
      begin
         return TList.Element (T.Name).NumF;
      end Num_Fields;

      function Num_PKs (T : Table_Description) return Integer is
      begin
         return TList.Element (T.Name).NumPK;
      end Num_PKs;

      function Has_PKs (T : Table_Description) return Boolean is
      begin
         return TList.Element (T.Name).NumPK > 0;
      end Has_PKs;

      function Num_FKs (T : Table_Description) return Integer is
      begin
         return TList.Element (T.Name).NumFK;
      end Num_FKs;

      function Has_FKs (T : Table_Description) return Boolean is
      begin
         return TList.Element (T.Name).NumFK > 0;
      end Has_FKs;

      function Some_FK_Can_Be_Null (T : Table_Description) return Boolean is
      begin
         return TList.Element (T.Name).SomeFK_Can_Be_Null;
      end Some_FK_Can_Be_Null;

      function Some_FK_Cannot_Be_Null (T : Table_Description)
                                       return Boolean is
      begin
         return TList.Element (T.Name).SomeFK_Cannot_Be_Null;
      end Some_FK_Cannot_Be_Null;

      function Can_R_Fetch (T : Table_Description) return Boolean is
      begin
         return TList.Element (T.Name).R_Fetch;
      end Can_R_Fetch;

   end Tables_Fields;
   use Tables_Fields;

   ----------------------------------------------------------------------
   --  Container of all fields alphabetically ordered
   ----------------------------------------------------------------------
   package All_Fields_Ordered is
      procedure Register_Fields (Schema : DB_Schema);
      procedure For_Each_Ordered_Field
        (Callback : access procedure (T : in out Table_Description;
                                      F : in out Field));
   end All_Fields_Ordered;

   package body All_Fields_Ordered is
      type Info_Block is record
         F : Field;
         T : Table_Description;
      end record;
      No_Block : constant Info_Block := Info_Block'(No_Field, No_Table);
      package Field_Map is new AC.Indefinite_Ordered_Maps
        (String,              -- F.Name @ T.Name
         Info_Block, "<", "=");
      Fields : Field_Map.Map;
      procedure Register_Fields (Schema : DB_Schema) is
         T0 : Table_Description;
         procedure Process_Field (F : in out Field);
         procedure Process_Field (F : in out Field) is
            CursFT : Field_Map.Cursor;
            Key    : constant String := F.Name & "@" & T0.Name;
         begin
            CursFT := Fields.Find (Key);
            if not Field_Map.Has_Element (CursFT) then
               Field_Map.Insert (Fields, Key, Info_Block'(F, T0));
            else
               raise Program_Error;
            end if;
         end Process_Field;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
         begin
            T0 := T;
            For_Each_Field (T, Process_Field'Access, True);
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, False);
      end Register_Fields;

      function Get_First_Block return Info_Block;
      function Get_First_Block return Info_Block is
         CursFT : Field_Map.Cursor;
      begin
         CursFT := Fields.First;
         if not Field_Map.Has_Element (CursFT) then
            return No_Block;
         else
            return Field_Map.Element (CursFT);
         end if;
      end Get_First_Block;
      function Get_Next_Block (B : Info_Block) return Info_Block;
      function Get_Next_Block (B : Info_Block) return Info_Block is
         Key    : constant String := B.F.Name & "@" & B.T.Name;
         CursFT : Field_Map.Cursor;
      begin
         CursFT := Fields.Find (Key);
         if not Field_Map.Has_Element (CursFT) then
            return No_Block;
         end if;
         Field_Map.Next (CursFT);
         if Field_Map.Has_Element (CursFT) then
            return Field_Map.Element (CursFT);
         else
            return No_Block;
         end if;
      end Get_Next_Block;

      procedure For_Each_Ordered_Field
        (Callback : access procedure (T : in out Table_Description;
                                      F : in out Field)) is
         B : Info_Block;
      begin
         B := Get_First_Block;
         while B /= No_Block loop
            Callback (B.T, B.F);
            B := Get_Next_Block (B);
         end loop;
      end For_Each_Ordered_Field;
   end All_Fields_Ordered;
   use All_Fields_Ordered;

   ----------------------------------------------------------------------
   --  Container of multiple FKDs.
   --  In some way duplicates private code in gnatcoll-sql-inpect
   ----------------------------------------------------------------------
   package All_MFKs is
      procedure Register_Tables_And_MFKs (Schema : DB_Schema);
      function Num_MFKs (T : Table_Description) return Integer;
      procedure For_Each_MFK
        (T        : Table_Description;
         Callback : access procedure
           (Pointed_Table : in out Table_Description;
            All_Not_Null  : in out Boolean));
   end All_MFKs;
   package body All_MFKs is
      use type Ada.Containers.Count_Type;

      type Field_Pair is record
         From : Field;
         To   : Field;
      end record;
      package Pair_Lists is new AC.Vectors (Natural, Field_Pair);

      type MFK_Block is record
         To_Table     : Table_Description;
         All_Not_Null : Boolean;
         Pairs        : Pair_Lists.Vector;
         Id           : Natural;
      end record;
      package MFKD_Lists is new AC.Vectors (Natural, MFK_Block);

      type Table_Info is record
         Table : Table_Description;
         FKDs  : MFKD_Lists.Vector;
      end record;
      package Table_Maps is new AC.Indefinite_Ordered_Maps
        (String,              -- T.Name
         Table_Info, "<", "=");
      TList : Table_Maps.Map;

      procedure Register_Tables_And_MFKs (Schema : DB_Schema) is
         --  procedure Dump (Msg : String);
         --  procedure Dump (Msg : String) is
         --     CursT : Table_Maps.Cursor;
         --     CursF : MFKD_Lists.Cursor;
         --     CursP : Pair_Lists.Cursor;
         --     TI    : Table_Info;
         --     MB    : MFK_Block;
         --     FP    : Field_Pair;
         --     use Table_Maps;
         --     use MFKD_Lists;
         --     use Pair_Lists;
         --  begin
         --     TIO.Put_Line ("=====> " & Msg);
         --     CursT := TList.First;
         --     while CursT /= Table_Maps.No_Element loop
         --        TI := Table_Maps.Element (CursT);
         --        TIO.Put_Line ("   Pointer Table " & TI.Table.Name);
         --        CursF := TI.FKDs.First;
         --        while CursF /= MFKD_Lists.No_Element loop
         --           MB := MFKD_Lists.Element (CursF);
         --           TIO.Put_Line ("      Id" & MB.Id'Image);
         --           TIO.Put_Line ("         Pointed Table " &
         --                                     MB.To_Table.Name);
         --           TIO.Put ("         All FKs Not Null ");
         --           if MB.All_Not_Null then
         --              TIO.Put_Line ("True");
         --           else
         --              TIO.Put_Line ("False");
         --           end if;
         --           TIO.Put_Line ("         Num. Pairs" &
         --                                     MB.Pairs.Length'Image);
         --           CursP := MB.Pairs.First;
         --           while CursP /= Pair_Lists.No_Element loop
         --              FP := Pair_Lists.Element (CursP);
         --              TIO.Put_Line ("            Pointer " &
         --                              TI.Table.Name & "." & FP.From.Name);
         --              TIO.Put_Line ("            Pointed " &
         --                              MB.To_Table.Name & "." & FP.To.Name);
         --              TIO.Put_Line ("            ------");
         --              Next (CursP);
         --           end loop;
         --           Next (CursF);
         --        end loop;
         --        Next (CursT);
         --     end loop;
         --  end Dump;

         Max_Id_Seen : Integer;

         procedure Process_FKD
           (From, To : Field; Id : Natural; Ambiguous : Boolean);
         procedure Process_FKD
           (From, To : Field; Id : Natural; Ambiguous : Boolean) is
            pragma Unreferenced (Ambiguous);

            Pointer_Table : constant Table_Description :=
              Table_Description (From.Get_Table);
            CursF : MFKD_Lists.Cursor;
            TI    : Table_Info;
            FP    : Field_Pair;
            MB    : MFK_Block;
            use MFKD_Lists;
         begin
            --  this is the field pair to be added
            FP := Field_Pair'(From, To);
            --  locate the table info
            TI := Table_Maps.Element (TList, Pointer_Table.Name);
            --  check Id
            if Id > Max_Id_Seen then --  this is a new FKD
               --  build the MB
               MB.To_Table     := Table_Description (To.Get_Table);
               MB.All_Not_Null := not From.Can_Be_Null;
               MB.Id           := Id;
               --  append the field pair to MB
               MB.Pairs.Append (FP);
               --  append the MB to TI
               TI.FKDs.Append (MB);
               --  update the table list
               Table_Maps.Replace (TList, Pointer_Table.Name, TI);
               --  set max_id_seen
               Max_Id_Seen := Id;
            else -- there is already a FKD
               --  find the FKD
               CursF := TI.FKDs.First;
               --  advance to the correct FKD
               while CursF /= MFKD_Lists.No_Element loop
                  MB := MFKD_Lists.Element (CursF);
                  exit when MB.Id = Id;
                  CursF := MFKD_Lists.Next (CursF);
               end loop;
               if CursF = MFKD_Lists.No_Element then
                  return;
               end if;
               MB.All_Not_Null := MB.All_Not_Null and (not From.Can_Be_Null);
               --  append the new pair
               MB.Pairs.Append (FP);
               --  update the MB in TI
               TI.FKDs.Replace_Element (CursF, MB);
               --  update the table list
               Table_Maps.Replace (TList, Pointer_Table.Name, TI);
            end if;
         end Process_FKD;

         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            CursT  : Table_Maps.Cursor;
            CursF0 : MFKD_Lists.Cursor;
            TI     : Table_Info;
            MB     : MFK_Block;
            use MFKD_Lists;
         begin
            TList.Insert (T.Name,
                          Table_Info'(Table => T,
                                      FKDs  => MFKD_Lists.Empty_Vector));
            Max_Id_Seen := 0;
            For_Each_FK (T, Process_FKD'Access);

            --  Dump ("before purging table " & T.Name);

            --  now purge FKDs with just one pair
            CursT  := TList.Find (T.Name);
            TI     := Table_Maps.Element (CursT);
            CursF0 := TI.FKDs.First;
            while CursF0 /= MFKD_Lists.No_Element loop
               MB := MFKD_Lists.Element (CursF0);
               if MB.Pairs.Length <= 1 then
                  MB.Pairs.Delete_First;
                  TI.FKDs.Delete (CursF0);
                  TList.Replace (T.Name, TI);
                  TI     := Table_Maps.Element (CursT);
                  CursF0 := TI.FKDs.First;
               else
                  Next (CursF0);
               end if;
            end loop;

            --  Dump ("after purging table " & T.Name);
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, False);
      end Register_Tables_And_MFKs;

      procedure For_Each_MFK
        (T        : Table_Description;
         Callback : access procedure
           (Pointed_Table : in out Table_Description;
            All_Not_Null  : in out Boolean))
      is
         CursT : Table_Maps.Cursor;
         CursF : MFKD_Lists.Cursor;
         TI    : Table_Info;
         MB    : MFK_Block;
         use MFKD_Lists;
      begin
         CursT := TList.Find (T.Name);
         TI    := Table_Maps.Element (CursT);
         CursF := TI.FKDs.First;
         while CursF /= MFKD_Lists.No_Element loop
            MB := MFKD_Lists.Element (CursF);
            if MB.Pairs.Length > 1 then
               Callback (MB.To_Table, MB.All_Not_Null);
            end if;
            Next (CursF);
         end loop;
      end For_Each_MFK;

      function Num_MFKs (T : Table_Description) return Integer is
      begin
         return Integer (TList.Element (T.Name).FKDs.Length);
      end Num_MFKs;
   end All_MFKs;
   use All_MFKs;

   ----------------------------------------------------------------------
   --  Print ordered lines
   ----------------------------------------------------------------------
   package Print_Ordered_Sections is
      procedure Start_Section (New_Key : String);
      --  starts a new section

      procedure Add (Txt : String);
      --  add txt to buffer

      procedure Add_Line (Txt : String);
      --  add txt to buffer and insert the line in current section

      procedure Print_Sections (F : TIO.File_Type);
      --  obviously print all and clear
   end Print_Ordered_Sections;

   package body Print_Ordered_Sections is
      use Ada.Strings.Unbounded;
      use type AC.Count_Type;

      package DLL_Lines is new AC.Doubly_Linked_Lists
        (Element_Type => Unbounded_String, "=" => "=");
      use DLL_Lines;

      function LT (Left, Right : String) return Boolean;
      function LT (Left, Right : String) return Boolean is
      begin
         return Left < Right;
      end LT;

      package Sections_Map is new AC.Indefinite_Ordered_Maps
        (Key_Type     => String,
         Element_Type => DLL_Lines.List, "<" => LT, "=" => "=");
      use Sections_Map;

      Section  : Sections_Map.Map := Empty_Map;
      Line     : DLL_Lines.List   := Empty_List;
      Curr_Key : Unbounded_String := Null_Unbounded_String;
      Buffer   : Unbounded_String := Null_Unbounded_String;

      procedure Add (Txt : String) is
      begin
         if Curr_Key = Null_Unbounded_String then
            raise Program_Error;
         end if;
         Buffer := Buffer & Txt;
      end Add;

      procedure Add_Line (Txt : String) is
      begin
         if Curr_Key = Null_Unbounded_String then
            raise Program_Error;
         end if;
         Buffer := Buffer & Txt;
         Line.Append (Buffer);
         Buffer := Null_Unbounded_String;
      end Add_Line;

      procedure Start_Section (New_Key : String) is
      begin
         if New_Key = "" then
            raise Program_Error;
         end if;
         if To_String (Curr_Key) = New_Key then
            return; -- already in this section
         end if;
         if Curr_Key = Null_Unbounded_String then
            --  this is the first section
            begin
               Section.Insert (New_Key, Empty_List);
            exception
               when others =>
                  TIO.Put_Line ("[1] New Key => " & New_Key &
                                  " already in Section");
                  raise;
            end;
            --  in any case, clear it
            Line.Clear;
            --  grab
            Curr_Key := To_Unbounded_String (New_Key);
            return;
         end if;

         --  check if there was already a section named New_Key (/= Curr_Key)
         if Find (Section, New_Key) /= Sections_Map.No_Element then
            --  Grab buffer and insert in the line in the previous section
            Line.Append (Buffer);
            Buffer := Null_Unbounded_String;
            Section.Insert (To_String (Curr_Key), Line);
            --  now switch to the existing section and recover its lines
            Curr_Key := To_Unbounded_String (New_Key);
            Line := Section.Element (New_Key);
            return;
         end if;

         --  totally new section. Grab buffer and line from previous one
         if Buffer /= Null_Unbounded_String then
            Line.Append (Buffer);
            Buffer := Null_Unbounded_String;

            --  attach list to the current section
            Section.Replace (Key      => To_String (Curr_Key),
                             New_Item => Line);
            Line.Clear;
         elsif Length (Line) > 0 then
            --  there are lines pending from previous section
            Section.Replace (Key      => To_String (Curr_Key),
                             New_Item => Line);
            Line.Clear;
         end if;

         --  start a new section
         Curr_Key := To_Unbounded_String (New_Key);
         begin
            Section.Insert (New_Key, Empty_List);
         exception
            when others =>
               TIO.Put_Line ("[2] New Key => " & New_Key &
                               " already in Section");
               raise;
         end;
      end Start_Section;

      procedure Print_Sections (F : TIO.File_Type) is
         CurS : Sections_Map.Cursor;
         CurL : DLL_Lines.Cursor;
      begin
         --  insert current list if pending text in buffer
         if Buffer /= Null_Unbounded_String then
            Line.Append (Buffer);
            Section.Replace (Key      => To_String (Curr_Key),
                             New_Item => Line);
         elsif Line.Length > 0 then
            Section.Replace (Key      => To_String (Curr_Key),
                             New_Item => Line);
         end if;

         --  clear list and buffer
         Line.Clear;
         Buffer := Null_Unbounded_String;

         --  now print
         CurS := Section.First;
         while Has_Element (CurS) loop
            Line := Element (CurS);
            CurL := Line.First;
            while Has_Element (CurL) loop
               if Element (CurL) = "" then
                  TIO.New_Line (F);
               else
                  TIO.Put_Line (F, To_String (Element (CurL)));
               end if;
               Next (CurL);
            end loop;
            Next (CurS);
         end loop;

         --  finally clear all
         Line.Clear;
         Buffer := Null_Unbounded_String;
         Section.Clear;
         Curr_Key := Null_Unbounded_String;
      end Print_Sections;
   end Print_Ordered_Sections;
   use Print_Ordered_Sections;

   ----------------------------------------------------------------------
   --  Some tools
   ----------------------------------------------------------------------
   package Some_Tools is
      procedure Emit_Section_Title (F          : TIO.File_Type;
                                    SName      : String;
                                    SkipBefore : TIO.Count := 1;
                                    SkipAfter  : TIO.Count := 1);
      procedure Emit_Section_Title (SName      : String;
                                    SkipBefore : TIO.Count := 1;
                                    SkipAfter  : TIO.Count := 1);
      function Image (K : Long_Long_Integer) return String;
      function Fields_Count_Array (T         : Table_Description;
                                   Follow_LJ : Boolean;
                                   DepthMax  : Integer;
                                   FKStop    : Field := No_Field)
                                return Counts_Array;
      function Get_First_PK (T : Table_Description) return Field;
      function Field_Description (F : Field) return String;
   end Some_Tools;
   package body Some_Tools is
      Section_Number : Integer := 0;
      procedure Emit_Section_Title (F          : TIO.File_Type;
                                    SName      : String;
                                    SkipBefore : TIO.Count := 1;
                                    SkipAfter  : TIO.Count := 1) is
         use TIO;
         Len : constant Integer := SName'Length;
      begin
         if SkipBefore > 0 then
            TIO.New_Line (F, SkipBefore);
         end if;
         Section_Number := Section_Number + 1;
         TIO.Put_Line (F, "   " & (1 .. Len + 6 => '-'));
         TIO.Put_Line (F, "   -- " & SName & " --");
         TIO.Put_Line (F, "   " & (1 .. Len + 6 => '-'));
         if SkipAfter > 0 then
            TIO.New_Line (F, SkipAfter);
         end if;
      end Emit_Section_Title;

      procedure Emit_Section_Title (SName      : String;
                                    SkipBefore : TIO.Count := 1;
                                    SkipAfter  : TIO.Count := 1) is
         use TIO;
         Len : constant Integer := SName'Length;
      begin
         for I in 1 .. SkipBefore loop
            Add_Line ("");
         end loop;
         Add_Line ("   " & (1 .. Len + 6 => '-'));
         Add_Line ("   -- " & SName & " --");
         Add_Line ("   " & (1 .. Len + 6 => '-'));
         for I in 1 .. SkipAfter loop
            Add_Line ("");
         end loop;
      end Emit_Section_Title;

      function Image (K : Long_Long_Integer) return String is
         Len : constant Integer := K'Image'Length;
      begin
         if K > 0 then
            return K'Image (K'Image'First + 1 .. Len);
         else
            return K'Image;
         end if;
      end Image;

      function Fields_Count_Array (T         : Table_Description;
                                   Follow_LJ : Boolean;
                                   DepthMax  : Integer;
                                   FKStop    : Field := No_Field)
                                return Counts_Array is
         FK_Stop : Boolean; -- to be reset before each call to fields_count_
         Depth   : Integer := 0;
         Temp    : Counts_Array;

         function Fields_Count (T         : Table_Description;
                                Depth     : Integer;
                                Follow_LJ : Boolean;
                                FKStop    : Field := No_Field) return Integer;
         function Fields_Count (T         : Table_Description;
                                Depth     : Integer;
                                Follow_LJ : Boolean;
                                FKStop    : Field := No_Field)
                             return Integer is
            Result : Integer;
            procedure Process_FK (FK : in out Field);
            procedure Process_FK (FK : in out Field) is
            begin
               if FK = FKStop then
                  FK_Stop := True;
                  return;
               end if;
               if FK_Stop then
                  return;
               end if;
               if Follow_LJ or (not FK.Can_Be_Null) then
                  Result := Result +
                    Fields_Count (Pointed_Table (FK), Depth - 1, Follow_LJ);
               end if;
            end Process_FK;
         begin
            Result := Num_Fields (T);
            if Depth > 0 then
               For_Each_FK (T, Process_FK'Access);
            end if;
            return Result;
         end Fields_Count;

      begin
         while Depth <= DepthMax loop
            FK_Stop := False;
            Temp (Depth) := Fields_Count (T, Depth, Follow_LJ, FKStop);
            Depth := Depth + 1;
         end loop;
         return Temp;
      end Fields_Count_Array;

      function Get_First_PK (T : Table_Description) return Field is
         PK : Field;
      begin
         PK := T.Get_PK;
         if PK = No_Field then
            if T.Super_Table /= No_Table then
               PK := T.Super_Table.Get_PK;
            end if;
         end if;
         return PK;
      end Get_First_PK;

      function Field_Description (F : Field) return String is
         FD : constant String := F.Description;
         Idx : Integer;
      begin
         if FD = "" then
            return FD;
         end if;
         if FD (FD'First) /= '(' then
            return FD;
         end if;
         Idx := Index (FD, ")");
         if not (Idx in FD'Range) then
            return FD;
         end if;
         if Idx = FD'First + 1 then
            return FD;
         end if;
         if Idx = FD'Last then
            return "";
         end if;
         return FD (Idx + 1 .. FD'Last);
      end Field_Description;
   end Some_Tools;
   use Some_Tools;

   ----------------------------------------------------------------------
   --  Emit Spec
   ----------------------------------------------------------------------
   package Emit_Spec is
      procedure Perform (Api        : String;
                         Orm        : String;
                         Output_Dir : String;
                         Schema     : DB_Schema);
   end Emit_Spec;
   package body Emit_Spec is
      Fspec : TIO.File_Type;

      procedure Emit_Spec_Header (API : String; Orm : String);
      procedure Emit_Spec_Abstract_Tables (Schema : DB_Schema);
      procedure Emit_Spec_Types (Schema : DB_Schema);
      procedure Emit_Spec_Elements (Schema : DB_Schema);
      procedure Emit_Spec_Managers_Implementation_Details (Schema : DB_Schema);
      procedure Emit_Spec_Manager_Types (Schema : DB_Schema);
      procedure Emit_Spec_Manager_Tables (Schema : DB_Schema);
      procedure Emit_Spec_Managers (Schema : DB_Schema);
      procedure Emit_Spec_Internals (Schema : DB_Schema);
      procedure Emit_Spec_Private;
      procedure Emit_Spec_DDR_Rows (Schema : DB_Schema);
      procedure Emit_Spec_Detached_Rows (Schema : DB_Schema);
      procedure Emit_Spec_End (Orm : String);

      --------------------------------------------------------------------
      procedure Perform (Api        : String;
                         Orm        : String;
                         Output_Dir : String;
                         Schema     : DB_Schema) is
      begin
         TIO.Create (File => Fspec,
                     Mode => TIO.Out_File,
                     Name => Output_Dir & "/" & Orm & "_new.ads");
         Emit_Spec_Header (Api, Capitalize (Orm));
         Emit_Spec_Abstract_Tables (Schema);
         Emit_Spec_Types (Schema);
         Emit_Spec_Elements (Schema);
         Emit_Spec_Managers_Implementation_Details (Schema);
         Emit_Spec_Manager_Types (Schema);
         Emit_Spec_Manager_Tables (Schema);
         Emit_Spec_Managers (Schema);
         Emit_Spec_Internals (Schema);
         Emit_Spec_Private;
         Emit_Spec_DDR_Rows (Schema);
         Emit_Spec_Detached_Rows (Schema);
         Emit_Spec_End (Capitalize (Orm));
         TIO.Close (Fspec);
      end Perform;

      --------------------------------------------------------------------
      --  No conditions
      --------------------------------------------------------------------
      procedure Emit_Spec_Header (API : String; Orm : String) is
      begin
         TIO.Put_Line (Fspec, "pragma Warnings (Off);");
         TIO.Put_Line (Fspec, "with Ada.Calendar; use Ada.Calendar;");
         TIO.Put_Line (Fspec, "with Ada.Finalization; use Ada.Finalization;");
         TIO.Put_Line (Fspec, "with Ada.Strings.Unbounded; " &
                         "use Ada.Strings.Unbounded;");
         TIO.Put_Line (Fspec, "with GNAT.Calendar; use GNAT.Calendar;");
         TIO.Put_Line (Fspec, "with GNAT.Strings; use GNAT.Strings;");
         TIO.Put_Line (Fspec, "with GNATCOLL.SQL; use GNATCOLL.SQL;");
         TIO.Put_Line (Fspec, "with GNATCOLL.SQL.Exec; " &
                         "use GNATCOLL.SQL.Exec;");
         TIO.Put_Line (Fspec, "with GNATCOLL.SQL.Orm; use GNATCOLL.SQL.Orm;");
         TIO.Put_Line (Fspec, "with GNATCOLL.SQL.Orm.Impl; " &
                         "use GNATCOLL.SQL.Orm.Impl;");
         TIO.Put_Line (Fspec, "with GNATCOLL.SQL.Sessions; " &
                         "use GNATCOLL.SQL.Sessions;");
         TIO.Put_Line (Fspec, "with GNATCOLL.SQL_Fields; " &
                         "use GNATCOLL.SQL_Fields;");
         TIO.Put_Line (Fspec, "with GNATCOLL.Tribooleans; " &
                         "use GNATCOLL.Tribooleans;");
         TIO.Put_Line (Fspec, "with System.Address_Image;");
         TIO.Put_Line (Fspec, "with " & Capitalize (API)
                       & "; use " & Capitalize (API) & ";");
         TIO.Put_Line (Fspec, "pragma Warnings (On);");
         TIO.Put_Line (Fspec, "pragma Style_Checks (Off);");
         TIO.New_Line (Fspec);
         TIO.Put_Line (Fspec, "package " & Capitalize (Orm) & "_New is");
         TIO.Put_Line (Fspec, "   package DBA renames " & Capitalize (API) & ";");
         TIO.Put_Line (Fspec, "   subtype Related_Depth is Integer " &
                         "range 0 .." & Max_Depth'Image & ";");
         TIO.New_Line (Fspec);
      end Emit_Spec_Header;

      --------------------------------------------------------------------
      --  This routine is used by Abstract and Normal Tables
      --------------------------------------------------------------------
      procedure Process_Elements_Field (T : in out Table_Description;
                                        F : in out Field);
      procedure Process_Elements_Field (T : in out Table_Description;
                                        F : in out Field) is
         TName      : constant String := Capitalize (T.Name);
         RName      : constant String := Capitalize (T.Row_Name);
         FName      : constant String := Capitalize (F.Name);
         RType      : constant String := Ada_Return (F);
         Pted_Table : Table_Description;
      begin
         Start_Section (FName & " of " & TName);
         Add ("function " &
                FName & " (Self : " &
                RName & ") " &
                "return " & RType);
         if Is_Abstract (T) then
            Add_Line (" is abstract;");
         else
            Add_Line (";");
         end if;
         if not Is_Abstract (T) then
            Add_Line ("function " &
                        FName & " (Self : Detached_" &
                        RName & ") return " &
                        RType & ";");
            if F.Is_Autoincrement then
               Add_Line ("   --  No Set function as this " &
                           "field is Autoincrement or Serial");
            else
               if T.Get_Kind = Kind_View and then not Internal_Updatable_Views
               then
                  Add_Line ("");
                  return;
               end if;
               Add ("procedure Set_" & FName &
                      " (Self : Detached_" & RName &
                      "; Value : " & RType & ")");
               if Is_Abstract (T) then
                  Add_Line (" is abstract;");
               else
                  Add_Line (";");
               end if;
            end if;
         end if;

         if Is_FK (F) then
            Pted_Table := Pointed_Table (F);
            if Pted_Table /= No_Table then
               declare
                  PTRName : constant String :=
                    Capitalize (Pted_Table.Row_Name);
               begin
                  Add ("function " &
                         FName &
                         " (Self : " &
                         RName & ") return " &
                         PTRName & "'Class");
                  if Is_Abstract (T) then
                     Add_Line (" is abstract;");
                  else
                     Add_Line (";");
                  end if;
                  if not Is_Abstract (T) then
                     Add_Line ("function " &
                                 FName &
                                 " (Self : Detached_" &
                                 RName &
                                 ") return Detached_" &
                                 PTRName & "'Class;");

                     Add ("procedure Set_" & FName);
                     Add_Line (" (Self  : Detached_" & RName & ";");
                     Add_Line (" Value : Detached_" &
                                 PTRName & "'Class);");
                  end if;
               end;
            end if;
         end if;
         if F.Description /= "" then
            Add_Line ("   --  " & Field_Description (F));
         end if;
         Add_Line ("");
      end Process_Elements_Field;

      --------------------------------------------------------------------
      --  See conditions in emit_spec_elements
      --------------------------------------------------------------------
      procedure Emit_Spec_Abstract_Tables (Schema : DB_Schema) is
         T0 : Table_Description;

         procedure Process_Field (F : in out Field);
         procedure Process_Field (F : in out Field) is
         begin
            Process_Elements_Field (T0, F);
         end Process_Field;
         procedure Process_Abstract_Table (T : in out Table_Description);
         procedure Process_Abstract_Table (T : in out Table_Description) is
            TName   : constant String := Capitalize (T.Name);
            RName   : constant String := Capitalize (T.Row_Name);
         begin --  process_table
            if not Is_Abstract (T) then
               return;
            end if;
            Emit_Section_Title (Fspec, "Elements: " & TName,
                                SkipBefore => 0,
                                SkipAfter  => 0);
            TIO.Put_Line (Fspec, "   --  Interfaces corresponding to " &
                            "abstract tables in the schema");
            TIO.New_Line (Fspec);
            TIO.Put_Line (Fspec, "   type " & RName & " is interface;");
            TIO.New_Line (Fspec);
            T0 := T;
            For_Each_Field (T, Process_Field'Access);
            Print_Sections (Fspec);
         end Process_Abstract_Table;
      begin
         For_Each_Table (Schema, Process_Abstract_Table'Access, True);
      end Emit_Spec_Abstract_Tables;

      --------------------------------------------------------------------
      --  No for abstract tables
      --------------------------------------------------------------------
      procedure Emit_Spec_Types (Schema : DB_Schema) is
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName   : constant String := Capitalize (T.Name);
            RName   : constant String := Capitalize (T.Row_Name);
            SuperTable : constant Table_Description := T.Super_Table;
         begin --  process_table
            if Is_Abstract (T) then
               return;
            end if;

            Start_Section (RName & " of " & TName);
            if SuperTable = No_Table then
               Add_Line ("type " & RName &
                               " is new Orm_Element with null record;");
            else
               Add_Line ("type " & RName &
                               " is new Orm_Element and " &
                               Capitalize (SuperTable.Row_Name) &
                               " with null record;");
            end if;
            Add_Line ("type " & RName & "_DDR is new " &
                            "Detached_Data (" &
                            Image (Num_Fields (T) +
                              Num_FKs (T) +
                              Num_MFKs (T), 1) &
                            ") with private;");
            Add_Line ("type Detached_" & RName & " is  " &
                            "--  Get() returns a " & RName & "_DDR");
            if SuperTable = No_Table then
               Add_Line ("new Sessions.Detached_Element " &
                               "with private;");
            else
               Add_Line ("new Sessions.Detached_Element and " &
                               Capitalize (SuperTable.Row_Name) &
                              " with private;");
            end if;
            Add_Line ("type Detached_" & RName & "_Access is " &
                            "access all Detached_" & RName & "'Class;");
            Add_Line ("No_Detached_" & RName & " : constant " &
                            "Detached_" & RName & ";");
            Add_Line ("No_" & RName & " : constant " &
                            RName & ";");
            Add_Line ("");
            Print_Sections (Fspec);
         end Process_Table;

      begin --  emit_spec_types
         Emit_Section_Title (Fspec, "Types",
                             SkipBefore => 0,
                             SkipAfter  => 0);
         TIO.Put_Line (Fspec, "   --  Detached_* elements extract the " &
                               "value from " &
                               "the list and store them");
         TIO.Put_Line (Fspec, "    --  locally. As a result, they " &
                         "remain valid " &
                         "even if the list is modified,");
         TIO.Put_Line (Fspec, "    --  but require more memory to store.");
         TIO.Put_Line (Fspec, "    --");
         TIO.Put_Line (Fspec, "    --  Other elements are only valid " &
                         "while the " &
                         "list from which they are");
         TIO.Put_Line (Fspec, "    --  created is not modified(see " &
                         "Element below). " &
                         "As soon as you iterate the");
         TIO.Put_Line (Fspec, "    --  list this element becomes invalid.");
         TIO.Put_Line (Fspec, "    --");
         TIO.Put_Line (Fspec, "    --  Direct lists are stored in memory, " &
                         "and can be traversed in any order.");
         TIO.Put_Line (Fspec, "    --  Forward lists can only be iterated " &
                         "forward. " &
                         "With some database backends");
         TIO.Put_Line (Fspec, "    --  this is much more efficient since " &
                         "only the " &
                         "current element needs to be");
         TIO.Put_Line (Fspec, "    --  stored in memory(and retrieved from " &
                         "the server).");
         TIO.New_Line (Fspec);

         For_Each_Table (Schema, Process_Table'Access, True);

         TIO.New_Line (Fspec);
      end Emit_Spec_Types;

      --------------------------------------------------------------------
      --  function "=": for tables (no abstract nor views) and num PKs > 0
      --  function <field_name>: for all field names
      --  function Set_<field_name>: for all field names except autoincrement
      --           or serial fields
      --  function Detach: no abstract (can be table or view)
      --  function From_Cache: for tables with has_cache
      --  function New_<table_name>: no abstracts
      --  function New_<table_name> (<PK_Name>...): num PKs > 0 and the PK is
      --           not autoincrement nor serial
      --------------------------------------------------------------------
      procedure Emit_Spec_Elements (Schema : DB_Schema) is
         T0 : Table_Description;

         procedure Process_MFK (Pointed_Table : in out Table_Description;
                                All_Not_Null  : in out Boolean);
         procedure Process_MFK (Pointed_Table : in out Table_Description;
                                All_Not_Null  : in out Boolean) is
            pragma Unreferenced (All_Not_Null);
            PTName  : constant String := Capitalize (Pointed_Table.Name);
            PTRName : constant String := Capitalize (Pointed_Table.Row_Name);
            RName  : constant String := Capitalize (T0.Row_Name);
         begin
            Start_Section (PTName);
            Add_Line ("function " &
                            PTName & " (Self : " &
                            RName & ") return " &
                        PTRName & "'Class;");
            --  Add_Line ("--  PENDING ID");
            Add_Line ("function " &
                            PTName & " (Self : Detached_" &
                            RName & ") return Detached_" &
                            PTRName & "'Class;");
            Add_Line ("procedure Set_" & PTName &
                            " (Self : Detached_" & RName &
                            "; Value : Detached_" & PTRName &
                            "'Class);");
            --  Add_Line ("--  Emit_Body_Set (3)");
            Add_Line ("");
         end Process_MFK;

         procedure Emit_New_With_Parameters (T      : Table_Description;
                                             RName  : String);
         procedure Emit_New_With_Parameters (T      : Table_Description;
                                             RName  : String) is
            First_PK : Boolean := True;
            procedure Process_PK_Parameter (PK : in out Field);
            procedure Process_PK_Parameter (PK : in out Field) is
            begin
               if PK.Is_Autoincrement then
                  return;
               end if;
               if First_PK then
                  First_PK := False;
                  TIO.Put (Fspec, Capitalize (PK.Name) & " : ");
                  TIO.Put (Fspec, Ada_Param (PK));
               else
                  TIO.Put_Line (Fspec, ";");
                  TIO.Put (Fspec, Capitalize (PK.Name) & " : " &
                             Ada_Param (PK));
               end if;
            end Process_PK_Parameter;
         begin
            if not Has_PKs (T) then
               return;
            end if;
            TIO.Put (Fspec, "   function New_" & RName & " (");
            For_Each_PK (T, Process_PK_Parameter'Access, True);
            TIO.Put_Line (Fspec, ")");
            TIO.Put_Line (Fspec, "            " &
                            "return Detached_" &
                            RName & "'Class;");
         end Emit_New_With_Parameters;

         procedure Process_Field (F : in out Field);
         procedure Process_Field (F : in out Field) is
         begin
            Process_Elements_Field (T0, F);
         end Process_Field;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName  : constant String  := Capitalize (T.Name);
            RName  : constant String  := Capitalize (T.Row_Name);
            PK     : Field;
         begin
            if Is_Abstract (T) then
               return;
            end if;

            Emit_Section_Title (Fspec, "Elements: " & TName,
                                SkipBefore => 0);
            if not Is_Abstract (T) then
               if T.Get_Kind = Kind_Table and then Has_PKs (T) then
                  TIO.Put_Line (Fspec, "   function ""="" (" &
                                  "Op1 : " & RName & "; Op2 : " & RName &
                                  ") return Boolean;");
                  TIO.Put_Line (Fspec, "   function ""="" (" &
                                  "Op1 : Detached_" & RName & "; " &
                                  "Op2 : Detached_" & RName & ")" &
                                  " return Boolean;");
                  TIO.Put_Line (Fspec, "   --  Compares two elements " &
                                  "using only the " &
                                  "primary keys. All other fields are");
                  TIO.Put_Line (Fspec, "   --  ignored");
                  TIO.New_Line (Fspec);
               end if;
            end if;
            T0 := T;
            For_Each_Field (T, Process_Field'Access, True);

            Start_Section ("ZZZ1_" & TName);
            Add_Line ("function Detach (Self : " &
                            RName & "'Class) return " &
                            "Detached_" & RName & "'Class;");
            Add_Line ("");
            PK := Get_First_PK (T);
            TIO.New_Line (Fspec);
            if Has_Cache (T) then
               Add_Line ("function From_Cache");
               Add_Line ("(Session "
                         & ": Session_Type;");
               Add_Line ("      " &
                           Capitalize (PK.Name)
                         & ": " &
                           Ada_Param (PK) & ")");
               Add_Line ("     return Detached_" &
                           RName & "'Class;");
               Add_Line ("   --  Check whether there is " &
                           "already " &
                           "an element with this primary key. If");
               Add_Line ("   --  not, the returned value will " &
                           "be a null " &
                           "element (test with Is_Null)");
               Add_Line ("");
            end if;

            --  emit pointed tables if this is the case
            For_Each_MFK (T0, Process_MFK'Access);

            --  print everything
            Print_Sections (Fspec);

            --  now emit the function new
            TIO.Put_Line (Fspec, "   function New_" & RName &
                            " return Detached_" & RName & "'Class;");

            --  now emit the function new with parameters
            if not Has_PKs (T) then
               TIO.Put_Line (Fspec, "   --  No function with primary_keys " &
                               "as the table has no primary keys");
            elsif (not All_PKs_Autoincrement (T)) then
               Emit_New_With_Parameters (T, RName);
            else
               TIO.Put_Line (Fspec, "   --  No function with primary_keys " &
                               "as they are Autoincrement or Serial");
            end if;
            TIO.New_Line (Fspec);
         end Process_Table;

      begin
         For_Each_Table (Schema, Process_Table'Access, False);
      end Emit_Spec_Elements;

      --------------------------------------------------------------------
      --  for no abstracts
      --------------------------------------------------------------------
      procedure Emit_Spec_Managers_Implementation_Details (Schema : DB_Schema)
      is
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName : constant String := Capitalize (T.Name);
         begin
            if not Is_Abstract (T) then
               TIO.Put_Line (Fspec, "   procedure Internal_Query_" & TName);
               TIO.Put_Line (Fspec, "     (Fields    : in out " &
                               "SQL_Field_List;");
               TIO.Put_Line (Fspec, "      From      : out SQL_Table_List;");
               TIO.Put_Line (Fspec, "      Criteria  : in out Sql_Criteria;");
               TIO.Put_Line (Fspec, "      Depth     : Natural;");
               TIO.Put_Line (Fspec, "      Follow_LJ : Boolean;");
               TIO.Put_Line (Fspec, "      Pk_Only   : Boolean := False);");
               TIO.New_Line (Fspec);
            end if;
         end Process_Table;

      begin
         Emit_Section_Title (Fspec, "Managers (Implementation Details)",
                             SkipBefore => 0, SkipAfter => 1);
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Spec_Managers_Implementation_Details;

      --------------------------------------------------------------------
      --  for no abstract
      --------------------------------------------------------------------
      procedure Emit_Spec_Manager_Types (Schema : DB_Schema) is
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName : constant String := Capitalize (T.Name);
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if not Is_Abstract (T) then
               TIO.Put_Line (Fspec, "   type I_" & TName &
                               "_Managers is abstract new " &
                               "Manager with null record;");
               TIO.Put_Line (Fspec, "   package I_" & TName &
                               " is new Generic_Managers");
               TIO.Put_Line (Fspec, "     (I_" & TName & "_Managers, " &
                               RName & ", Related_Depth, " &
                               "DBA." & TName & ",");
               TIO.Put_Line (Fspec, "      Internal_Query_" & TName & ");");
               TIO.Put_Line (Fspec, "   type " & TName &
                               "_Managers is new I_" &
                               TName & ".Manager with null record;");
               TIO.Put_Line (Fspec, "   subtype " & TName & "_Stmt is I_" &
                               TName & ".ORM_Prepared_Statement;");
               TIO.New_Line (Fspec);
               TIO.Put_Line (Fspec, "   subtype " & RName & "_List is I_" &
                               TName & ".List;");
               TIO.Put_Line (Fspec, "   subtype Direct_" &
                               RName & "_List is I_" &
                               TName & ".Direct_List;");
               TIO.Put_Line (Fspec, "   Empty_" & RName & "_List : constant " &
                               RName & "_List := I_" &
                               TName & ".Empty_List;");
               TIO.Put_Line (Fspec, "   Empty_Direct_" & RName &
                               "_List : constant" &
                               " Direct_" & RName & "_List :=");
               TIO.Put_Line (Fspec, "   I_" & TName & ".Empty_Direct_List;");

               TIO.New_Line (Fspec);
            end if;
         end Process_Table;

      begin
         Emit_Section_Title (Fspec, "Manager Types",
                             SkipBefore => 0);
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Spec_Manager_Types;

      --------------------------------------------------------------------
      --  for each table or view but (but no abstract)
      --  In each table, reverse relations if there are any
      --  function <reverse_relation>: in each table if another table has a
      --           FK pointing to this table and with reverse relation
      --  function filter
      --  function get_<table_name>
      --------------------------------------------------------------------
      procedure Emit_Spec_Manager_Tables (Schema : DB_Schema) is
         T0       : Table_Description;

         procedure Process_Field (F : in out Field);
         procedure Process_Field (F : in out Field) is
            FName : constant String := Capitalize (F.Name);
            PType : constant String := Ada_Param (F);
            DType : constant String := Ada_Default_Param (F);
         begin
            TIO.Put_Line (Fspec, ";");
            TIO.Put (Fspec, "      " & FName &
                       " : " &
                       PType & " := " & DType);
         end Process_Field;

         procedure Emit_PKs (PK : in out Field);
         procedure Emit_PKs (PK : in out Field) is
         begin
            TIO.Put (Fspec, "      " & Capitalize (PK.Name));
            TIO.Put_Line (Fspec, " : " & Ada_Param (PK) & ";");
         end Emit_PKs;

         --  This is emitted only it another table has one FK
         --  pointing to some field of T0 and with reverse relation
         procedure Process_RFK (FK : in out Field);
         procedure Process_RFK (FK : in out Field) is
            use GNAT.Strings;
            --  FK is a pointer field in another table pointing to T0
         begin
            if Is_Pointed (T0) and then FK.Reverse_Relation /= null then
               declare
                  T1 : constant Table_Description :=
                    Table_Description (FK.Get_Table); -- pointer table
                  RVName : constant String :=
                    Capitalize (Rev_Relation (FK));
                  --  pointed
                  PTName  : constant String := Capitalize (T0.Name);
                  PTRName : constant String := Capitalize (T0.Row_Name);
                  --  pointing
                  PGName  : constant String := Capitalize (T1.Name);
                  Qual : constant String :=
                    (if Is_Multiple_FK (FK) then
                          "_" & Capitalize (FK.Name)
                     else "");
               begin
                  Start_Section (PGName & "_" & Qual);
                  Add_Line ("function " &
                              RVName & Qual &
                              " (Self : " &
                              PTRName &
                              "'Class) return " &
                              PGName &
                              "_Managers;");
                  Add_Line ("function " &
                              RVName & Qual &
                              " (Self : Detached_" &
                              PTRName &
                              "'Class) return " &
                              PGName &
                              "_Managers;");
                  Add_Line ("function " &
                              RVName & Qual &
                              " (Self : I_" &
                              PTName &
                              "_Managers'Class) return " &
                              PGName &
                              "_Managers;");
                  Add_Line ("");
               end;
            end if;
         end Process_RFK;

         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName : constant String := Capitalize (T.Name);
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            T0 := T;
            Emit_Section_Title (Fspec, "Manager: " & TName,
                                SkipBefore => 0);

            --  This is emitted only it another table has one FK
            --  pointing to some field of T0 and with reverse relation
            For_Each_RFK (T, Process_RFK'Access);
            Print_Sections (Fspec);

            TIO.Put_Line (Fspec, "   function Filter");
            TIO.Put (Fspec, "     (Self " &
                       " : " &
                       TName & "_Managers'Class");
            For_Each_Field (T, Process_Field'Access, True);
            TIO.Put_Line (Fspec, ")");
            TIO.Put_Line (Fspec, "     return " &
                            TName & "_Managers;");
            TIO.New_Line (Fspec);

            if Has_PKs (T) then
               TIO.Put_Line (Fspec, "   function Get_" & RName);
               TIO.Put_Line (Fspec, "     (Session" &
                               " : Session_Type;");
               For_Each_PK (T, Emit_PKs'Access, True);
               TIO.Put_Line (Fspec, "      Depth" &
                               " : " &
                               "Related_Depth := 0;");
               TIO.Put_Line (Fspec, "      Follow_Left_Join" &
                               " : Boolean " &
                               ":= False)");
               TIO.Put_Line (Fspec, "     return Detached_" &
                               RName &
                               "'Class;");
               TIO.New_Line (Fspec);
            end if;
         end Process_Table;

      begin
         TIO.New_Line (Fspec);
         For_Each_Table (Schema, Process_Table'Access, False);
      end Emit_Spec_Manager_Tables;

      --------------------------------------------------------------------
      --  No for abstract
      --------------------------------------------------------------------
      procedure Emit_Spec_Managers (Schema : DB_Schema) is
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName : constant String := Capitalize (T.Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            TIO.Put_Line (Fspec, "   All_" &
                            TName & " : constant " &
                            TName & "_Managers :=");
            TIO.Put_Line (Fspec, "     (I_" &
                            TName & ".All_Managers with null record);");
            TIO.New_Line (Fspec);
         end Process_Table;
      begin
         Emit_Section_Title (Fspec, "Managers", SkipBefore => 0);
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Spec_Managers;

      --------------------------------------------------------------------
      --  No for abstract
      --  procedure Free
      --  procedure Insert_Or_Update
      --  procedure Internal_Delete
      --  function Key
      --  procedure On_Persist: if the table has FK
      --------------------------------------------------------------------
      procedure Emit_Spec_Internals (Schema : DB_Schema) is
         procedure Process_Free (T : in out Table_Description);
         procedure Process_Free (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            TIO.Put_Line (Fspec, "   overriding procedure Free (Self " &
                            ": in out " &
                            RName &
                            "_Ddr);");
         end Process_Free;
         procedure Process_Insert_Or_Update (T : in out Table_Description);
         procedure Process_Insert_Or_Update (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            TIO.Put_Line (Fspec, "   overriding procedure Insert_Or_Update");
            TIO.Put_Line (Fspec, "     (Self        : in out Detached_" &
                            RName & ";");
            TIO.Put_Line (Fspec, "      Pk_Modified : in out Boolean;");
            TIO.Put (Fspec, "      Mask        : Dirty_Mask)");
            if T.Get_Kind = Kind_View and then not Internal_Updatable_Views
            then
               TIO.Put_Line (Fspec, " is null;");
               return;
            else
               TIO.Put_Line (Fspec, ";");
            end if;
         end Process_Insert_Or_Update;
         procedure Process_Internal_Delete (T : in out Table_Description);
         procedure Process_Internal_Delete (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            TIO.Put_Line (Fspec, "   overriding procedure Internal_Delete " &
                            "(Self : Detached_" & RName & ");");
         end Process_Internal_Delete;
         procedure Process_Key (T : in out Table_Description);
         procedure Process_Key (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            TIO.Put_Line (Fspec, "   overriding function Key (Self : " &
                            RName &
                            "_Ddr) return Element_Key;");
         end Process_Key;
         procedure Process_On_Persist (T : in out Table_Description);
         procedure Process_On_Persist (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if T.Get_Kind = Kind_View and then not Internal_Updatable_Views
            then
               return;
            end if;
            --  only for tables with FK
            if Has_FKs (T) then
               TIO.Put_Line (Fspec, "   overriding procedure On_Persist " &
                               "(Self : Detached_" &
                               RName & ");");
            end if;
         end Process_On_Persist;
      begin
         Emit_Section_Title (Fspec, "Internal",
                             SkipBefore => 0,
                             SkipAfter  => 1);
         For_Each_Table (Schema, Process_Free'Access, True);
         TIO.New_Line (Fspec);
         For_Each_Table (Schema, Process_Insert_Or_Update'Access, True);
         TIO.New_Line (Fspec);
         For_Each_Table (Schema, Process_Internal_Delete'Access, True);
         TIO.New_Line (Fspec);
         For_Each_Table (Schema, Process_Key'Access, True);
         TIO.New_Line (Fspec);
         For_Each_Table (Schema, Process_On_Persist'Access, True);
         TIO.New_Line (Fspec);
      end Emit_Spec_Internals;

      --------------------------------------------------------------------
      --  Always
      --------------------------------------------------------------------
      procedure Emit_Spec_Private is
      begin
         TIO.Put_Line (Fspec, "private");
         TIO.New_Line (Fspec);
      end Emit_Spec_Private;

      --------------------------------------------------------------------
      --  No for abstract
      --------------------------------------------------------------------
      procedure Emit_Spec_DDR_Rows (Schema : DB_Schema) is
         procedure Process_MFK (Pointed_Table : in out Table_Description;
                                All_Not_Null  : in out Boolean);
         procedure Process_MFK (Pointed_Table : in out Table_Description;
                                All_Not_Null  : in out Boolean) is
            pragma Unreferenced (All_Not_Null);
            TName : constant String := Capitalize (Pointed_Table.Name);
            RName : constant String := Capitalize (Pointed_Table.Row_Name);
         begin
            Start_Section (New_Key => "ORM_FK_" & TName);
            Add_Line ("ORM_FK_" &
                        TName &
                        " : " &
                        "Detached_" &
                        RName &
                        "_Access := null;");
         end Process_MFK;

         procedure Process_FK (From : Field;
                               To   : Field;
                               Id   : Natural;
                               Ambiguous : Boolean);
         procedure Process_FK (From : Field;
                               To   : Field;
                               Id   : Natural;
                               Ambiguous : Boolean) is
            pragma Unreferenced (Id, Ambiguous);
            T : constant Table_Description := Table_Description (To.Get_Table);
            TName : constant String := Capitalize (T.Name);
            RName : constant String := Capitalize (T.Row_Name);
            FName : constant String := Capitalize (From.Name);
         begin
            Start_Section (New_Key => "ORM_FK_" & FName & " of " & TName);
            Add_Line ("ORM_FK_" &
                        FName &
                        " : " &
                        "Detached_" &
                        RName &
                        "_Access := null;");
         end Process_FK;
         procedure Process_Field (F : in out Field);
         procedure Process_Field (F : in out Field) is
            T : constant Table_Description := Table_Description (F.Get_Table);
            TName : constant String := Capitalize (T.Name);
            FName : constant String := Capitalize (F.Name);
         begin
            Start_Section (New_Key => "ORM_" & FName & " of " & TName);
            Add ("ORM_" & FName & " : " & Ada_Field (F) & " := ");
            if F.Default = "" then
               Add_Line (Ada_Default_Record (F) & ";");
            else
               declare
                  SQL_Name : constant String := SQL_Field (F);
                  Default  : String := F.Default;
                  Idx      : Integer;
               begin
                  if SQL_Name = "SQL_Field_Boolean" then
                     Add_Line (Capitalize (F.Default) & ";");
                  elsif SQL_Name = "SQL_Field_Text" then
                     Default := Replace (Default, "'", """");
                     Idx := Index (Default, "::");
                     if Idx in Default'Range then
                        Add_Line ("To_Unbounded_String (" &
                                    Default (Default'First .. Idx - 1) & ");");
                     else
                        Add_Line ("To_Unbounded_String (" &
                                    Default & ");");
                     end if;
                  else
                     Add_Line (Default & ";");
                  end if;
               end;
            end if;
         end Process_Field;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            TIO.Put_Line (Fspec, "    type " & RName &
                            "_DDR is new Detached_Data (" &
                            Image (Num_Fields (T) +
                              Num_FKs (T) +
                              Num_MFKs (T), 1) &
                            ") with record");
            For_Each_Field (T, Process_Field'Access, True);
            For_Each_FK (T, Process_FK'Access);
            For_Each_MFK (T, Process_MFK'Access);
            Print_Sections (Fspec);
            TIO.Put_Line (Fspec, "    end record;");
            TIO.Put_Line (Fspec, "    type " & RName &
                            "_Data is access all " &
                            RName &
                            "_DDR;");
            TIO.New_Line (Fspec);
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Spec_DDR_Rows;

      --------------------------------------------------------------------
      --  No for abstract
      --------------------------------------------------------------------
      procedure Emit_Spec_Detached_Rows (Schema : DB_Schema) is
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            TIO.New_Line (Fspec);
            TIO.Put_Line (Fspec, "    type Detached_" & RName);
            TIO.Put (Fspec, "       is new Sessions.Detached_Element");
            if Super_Table (T) /= No_Table then
               TIO.Put (Fspec, " and " &
                          Capitalize (Super_Table (T).Row_Name));
            end if;
            TIO.Put_Line (Fspec, " with null record;");
            TIO.Put_Line (Fspec, "    No_" &
                            RName & " : constant " & RName &
                            " := (No_Orm_Element with null record);");
            TIO.Put_Line (Fspec, "    No_Detached_" &
                            RName & " : constant Detached_" & RName & " :=");
            TIO.Put_Line (Fspec, "      (Sessions.Detached_Element with " &
                            "null record);");
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Spec_Detached_Rows;

      --------------------------------------------------------------------
      --  Always
      --------------------------------------------------------------------
      procedure Emit_Spec_End (Orm : String) is
      begin
         TIO.New_Line (Fspec);
         TIO.Put_Line (Fspec, "end " & Capitalize (Orm) & "_New;");
      end Emit_Spec_End;
   end Emit_Spec;

   ----------------------------------------------------------------------
   --  Emit Body
   ----------------------------------------------------------------------
   package Emit_Body is
      procedure Perform (Api        : String;
                         Orm        : String;
                         Output_Dir : String;
                         Schema     : DB_Schema);
   end Emit_Body;
   package body Emit_Body is
      Fbody : TIO.File_Type;

      procedure Emit_Body_Header (Orm : String);
      procedure Emit_Body_Unchecked (Schema : DB_Schema);
      procedure Emit_Body_Constants (Schema : DB_Schema);
      procedure Emit_Body_Detach_No_Lookup_Spec (Schema : DB_Schema);
      procedure Emit_Body_Do_Query_Spec (Schema : DB_Schema);
      procedure Emit_Body_Equals (Schema : DB_Schema);
      procedure Emit_Body_Elements (Schema : DB_Schema);
      procedure Emit_Body_Detach (Schema : DB_Schema);
      procedure Emit_Body_Detach_No_Lookup_Body (Schema : DB_Schema);
      procedure Emit_Body_Do_Query_Body (Schema : DB_Schema);
      procedure Emit_Body_Filter (Schema : DB_Schema);
      procedure Emit_Body_Free (Schema : DB_Schema);
      procedure Emit_Body_From_Cache (Schema : DB_Schema);
      procedure Emit_Body_Get (Schema : DB_Schema);
      procedure Emit_Body_Insert_Or_Update (Schema : DB_Schema);
      procedure Emit_Body_Internal_Delete (Schema : DB_Schema);
      procedure Emit_Body_Internal_Query (Schema : DB_Schema);
      procedure Emit_Body_Key (Schema : DB_Schema);
      procedure Emit_Body_New (Schema : DB_Schema);
      procedure Emit_Body_On_Persit (Schema : DB_Schema);
      procedure Emit_Body_Set (Schema : DB_Schema);
      procedure Emit_Body_End (Orm : String);

      --------------------------------------------------------------------
      procedure Perform (Api        : String;
                         Orm        : String;
                         Output_Dir : String;
                         Schema     : DB_Schema) is
         pragma Unreferenced (Api);
      begin
         TIO.Create (File => Fbody,
                     Mode => TIO.Out_File,
                     Name => Output_Dir & "/" & Orm & "_new.adb");
         Emit_Body_Header (Capitalize (Orm));
         Emit_Body_Unchecked (Schema);
         Emit_Body_Constants (Schema);
         Emit_Body_Detach_No_Lookup_Spec (Schema);
         Emit_Body_Do_Query_Spec (Schema);
         Emit_Body_Equals (Schema);
         Emit_Body_Elements (Schema);
         Emit_Body_Detach (Schema);
         Emit_Body_Detach_No_Lookup_Body (Schema);
         Emit_Body_Do_Query_Body (Schema);
         Emit_Body_Filter (Schema);
         Emit_Body_Free (Schema);
         Emit_Body_From_Cache (Schema);
         Emit_Body_Get (Schema);
         Emit_Body_Insert_Or_Update (Schema);
         Emit_Body_Internal_Delete (Schema);
         Emit_Body_Internal_Query (Schema);
         Emit_Body_Key (Schema);
         Emit_Body_New (Schema);
         Emit_Body_On_Persit (Schema);
         Emit_Body_Set (Schema);
         Emit_Body_End (Capitalize (Orm));
         TIO.Close (Fbody);
      end Perform;

      ----------------------------------------------------------------------
      --  always
      ----------------------------------------------------------------------
      procedure Emit_Body_Header (Orm : String) is
      begin
         TIO.New_Line (Fbody);
         TIO.Put_Line (Fbody, "pragma Warnings (Off);");
         TIO.Put_Line (Fbody, "with Ada.Containers; use Ada.Containers;");
         TIO.Put_Line (Fbody, "with Ada.Unchecked_Deallocation;");
         TIO.Put_Line (Fbody, "pragma Warnings (On);");
         TIO.Put_Line (Fbody, "pragma Style_Checks (Off);");
         TIO.New_Line (Fbody);
         TIO.Put_Line (Fbody, "package body " & Capitalize (Orm) & "_New is");
         TIO.Put_Line (Fbody, "pragma Warnings (Off);");
         TIO.Put_Line (Fbody, "use Sessions.Pointers;");
         TIO.New_Line (Fbody);
      end Emit_Body_Header;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  <row_name>_DDR: for all tables and views
      --  <Detached_<row_name>: for pointed tables
      ----------------------------------------------------------------------
      procedure Emit_Body_Unchecked (Schema : DB_Schema) is
         procedure Process1_Table (T : in out Table_Description);
         procedure Process1_Table (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            TIO.Put_Line (Fbody, "procedure Unchecked_Free is new " &
                            "Ada.Unchecked_Deallocation");
            TIO.Put_Line (Fbody, "(" &
                            RName &
                            "_DDR, " &
                            RName & "_Data);");
         end Process1_Table;
         procedure Process2_Table (T : in out Table_Description);
         procedure Process2_Table (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if T.Get_Kind = Kind_Table and then Is_Pointed (T)
            then
               TIO.Put_Line (Fbody, "procedure Unchecked_Free is new " &
                               "Ada.Unchecked_Deallocation");
               TIO.Put_Line (Fbody, "  (Detached_" &
                               RName &
                               "'Class, Detached_" &
                               RName &
                               "_Access);");
            end if;
         end Process2_Table;
      begin
         For_Each_Table (Schema, Process1_Table'Access, True);
         For_Each_Table (Schema, Process2_Table'Access, True);
         TIO.New_Line (Fbody);
      end Emit_Body_Unchecked;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  F_<field_name>: prefixed with "F_" and then ":=" to a correlative
      --           field number starting in 0
      --  Count_<table_name> only for pointed tables.
      --  Upto__<table_name>_<num>: for each FK
      --  Alias_<table_name>
      ----------------------------------------------------------------------
      procedure Emit_Body_Constants (Schema : DB_Schema) is
         T0  : Table_Description;
         Num : Integer;
         procedure Process_Count (T : Table_Description);
         --  called only for pointed tables
         procedure Process_Count (T : Table_Description) is
            C   : Counts_Array;
            CLJ : Counts_Array;
         begin
            C := Fields_Count_Array (T         => T,
                                     Follow_LJ => False,
                                     DepthMax  => Max_Depth,
                                     FKStop    => No_Field);
            CLJ := Fields_Count_Array (T         => T,
                                       Follow_LJ => True,
                                       DepthMax  => Max_Depth,
                                       FKStop    => No_Field);
            TIO.Put (Fbody, "   Counts_" &
                       Capitalize (T.Name) &
                       " : constant Counts := (");
            for I in 0 .. Max_Depth loop
               TIO.Put (Fbody, "(" & Image (C (I), 1) &
                          "," &
                          Image (CLJ (I), 1) & ")");
               if I < Max_Depth then
                  TIO.Put (Fbody, ",");
               end if;
            end loop;
            TIO.Put_Line (Fbody, ");");
         end Process_Count;
         procedure Process_Upto (FK : in out Field);
         --  called only for each FK
         procedure Process_Upto (FK : in out Field) is
            T : constant Table_Description :=
              Table_Description (FK.Get_Table);
            TName : constant String  := Capitalize (T.Name);
            FKNum : constant Integer := Pointer_Order (FK);
            C     : Counts_Array;
            CLJ   : Counts_Array;
         begin
            C := Fields_Count_Array (T         => T,
                                     Follow_LJ => False,
                                     DepthMax  => Max_Depth,
                                     FKStop    => FK);
            CLJ := Fields_Count_Array (T         => T,
                                       Follow_LJ => True,
                                       DepthMax  => Max_Depth,
                                       FKStop    => FK);
            TIO.Put (Fbody, "   Upto_" & TName &
                       "_" &
                       Image (FKNum, 1) &
                       " : constant Counts := (");
            for I in 0 .. Max_Depth loop
               TIO.Put (Fbody, "(" & Image (C (I), 1) & "," &
                          Image (CLJ (I), 1) & ")");
               if I < Max_Depth then
                  TIO.Put (Fbody, ",");
               end if;
            end loop;
            TIO.Put_Line (Fbody, ");");
         end Process_Upto;
         procedure Process_MFK (Pointed_Table : in out Table_Description;
                                All_Not_Null  : in out Boolean);
         procedure Process_MFK (Pointed_Table : in out Table_Description;
                                All_Not_Null  : in out Boolean) is
            pragma Unreferenced (All_Not_Null);
            TName : constant String  := Capitalize (T0.Name);
            C     : Counts_Array;
            CLJ   : Counts_Array;
            pragma Unreferenced (Pointed_Table);
         begin
            C := Fields_Count_Array (T         => T0,
                                     Follow_LJ => False,
                                     DepthMax  => Max_Depth,
                                     FKStop    => No_Field);
            CLJ := Fields_Count_Array (T         => T0,
                                       Follow_LJ => True,
                                       DepthMax  => Max_Depth,
                                       FKStop    => No_Field);
            TIO.Put (Fbody, "   Upto_" & TName &
                       "_" &
                       Image (Num, 1) &
                       " : constant Counts := (");
            for I in 0 .. Max_Depth loop
               TIO.Put (Fbody, "(" & Image (C (I), 1) & "," &
                          Image (CLJ (I), 1) & ")");
               if I < Max_Depth then
                  TIO.Put (Fbody, ",");
               end if;
            end loop;
            TIO.Put_Line (Fbody, ");");
            Num := Num + 1;
         end Process_MFK;

         procedure Process_Alias (T : Table_Description);
         procedure Process_Alias (T : Table_Description) is
            NumFK : constant Integer :=
              Num_Fields (T) + Num_FKs (T) + Num_MFKs (T);
            Alias : array (1 .. NumFK) of Integer;
         begin
            TIO.Put (Fbody, "   Alias_" &
                       Capitalize (T.Name) &
                       " : constant Alias_Array := (");
            if Num_FKs (T) > 0 then
               for I in 1 .. NumFK loop
                  Alias (I) := -I; -- until we know how to compute it
               end loop;
               for I in 1 .. NumFK - 1 loop
                  TIO.Put (Fbody, Image (Alias (I), 1) & ",");
               end loop;
               TIO.Put (Fbody, Image (Alias (NumFK), 1));
            else
               TIO.Put (Fbody, "0 => -1");
            end if;
            TIO.Put_Line (Fbody, ");");
         end Process_Alias;
         procedure Process_Field (F : in out Field);
         procedure Process_Field (F : in out Field) is
            TName : constant String := Capitalize (T0.Name);
            FName : constant String := Capitalize (F.Name);
         begin
            TIO.Put_Line (Fbody, "F_" &
                            TName & "_" &
                            FName &
                            " : constant := " & Image (Num, 1) & ";");
            Num := Num + 1;
         end Process_Field;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
         begin
            if Is_Abstract (T) then
               return;
            end if;
            Num := 0;
            T0 := T;
            For_Each_Field (T, Process_Field'Access, True);
            if Is_Pointed (T) then
               Process_Count (T);
            end if;
            For_Each_FK (T, Process_Upto'Access);
            Num := Num_FKs (T);
            For_Each_MFK (T, Process_MFK'Access);
            Process_Alias (T);
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
         TIO.New_Line (Fbody);
      end Emit_Body_Constants;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  Follow_LJ not for views
      --  Pk_Only emitted only for tables with PKs
      ----------------------------------------------------------------------
      procedure Emit_Body_Detach_No_Lookup_Spec (Schema : DB_Schema) is
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            TIO.Put_Line (Fbody, "function Detach_No_Lookup");
            TIO.Put_Line (Fbody, "  (Self    : " & RName & "'Class;");
            TIO.Put_Line (Fbody, "Session : Session_Type)");
            TIO.Put_Line (Fbody, "  return Detached_" & RName & "'Class;");
            --  TIO.Put_Line (Fbody,
            --                "--  Same as Detach, but does not check " &
            --                "the session cache");
         end Process_Table;
      begin
         TIO.Put_Line (Fbody, "pragma Warnings (On);");
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Detach_No_Lookup_Spec;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  For tables (no views): emit PK_Only if num PKs > 0
      ----------------------------------------------------------------------
      procedure Emit_Body_Do_Query_Spec (Schema : DB_Schema) is
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName : constant String := Capitalize (T.Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            TIO.Put_Line (Fbody, "procedure Do_Query_" & TName);
            TIO.Put_Line (Fbody, "  (Fields    : in out SQL_Field_List;");
            TIO.Put_Line (Fbody, "From      : out SQL_Table_List;");
            TIO.Put_Line (Fbody, "Criteria  : in out Sql_Criteria;");
            TIO.Put_Line (Fbody, "Base      : Natural;");
            TIO.Put_Line (Fbody, "Aliases   : Alias_Array;");
            TIO.Put_Line (Fbody, "Depth  : Natural;");
            TIO.Put (Fbody,      "      Follow_LJ : Boolean");
            if T.Get_Kind = Kind_Table and then Has_PKs (T) then
               TIO.Put_Line (Fbody, ";");
               TIO.Put_Line (Fbody, "Pk_Only   : Boolean := False);");
            else
               TIO.Put_Line (Fbody, ");");
            end if;
            TIO.New_Line (Fbody);
         end Process_Table;
      begin
         TIO.New_Line (Fbody);
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Do_Query_Spec;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  Only for tables with PKs
      ----------------------------------------------------------------------
      procedure Emit_Body_Equals (Schema : DB_Schema) is
         RemPKs : Integer;
         NumPKs : Integer;
         Disp : Integer := 0;
         procedure Process_PK (F : in out Field);
         procedure Process_PK (F : in out Field) is
            FName : constant String := Capitalize (F.Name);
         begin
            TIO.Put (Fbody, Ada_Param (F) & "'(Op1." &
                       FName & ") = Op2." &
                       FName);
            RemPKs := RemPKs - 1;
            if RemPKs > 0 then
               TIO.New_Line (Fbody);
               TIO.Put (Fbody, (1 .. Disp => ' ') & "      and ");
            else
               TIO.Put_Line (Fbody, ";");
            end if;
         end Process_PK;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if T.Get_Kind = Kind_Table and then Has_PKs (T) then
               Emit_Section_Title (Fbody, """=""", SkipBefore => 0);
               TIO.Put_Line (Fbody, "function ""="" (" &
                               "Op1 : " & RName & "; Op2 : " & RName &
                               ") return Boolean is");
               TIO.Put_Line (Fbody, "begin");
               TIO.Put (Fbody, "      return ");
               NumPKs := Num_PKs (T);
               RemPKs := NumPKs;
               Disp := 0;
               For_Each_PK (T, Process_PK'Access, True);
               TIO.Put_Line (Fbody, "end ""="";");

               Emit_Section_Title (Fbody, """=""");
               TIO.Put (Fbody, "   function ""=""");
               TIO.Put (Fbody, " (" &
                          "Op1 : Detached_" & RName & "; ");
               TIO.Put (Fbody, "Op2 : Detached_" & RName & ")");
               TIO.Put_Line (Fbody, " return Boolean is");
               TIO.Put_Line (Fbody, "begin");
               TIO.Put_Line (Fbody, "if Op1.Is_Null then");
               TIO.Put_Line (Fbody, "return Op2.Is_Null;");
               TIO.Put_Line (Fbody, "elsif Op2.Is_Null then");
               TIO.Put_Line (Fbody, "return False;");
               TIO.Put_Line (Fbody, "else");
               TIO.Put (Fbody, "         return ");
               NumPKs := Num_PKs (T);
               RemPKs := NumPKs;
               Disp := 3;
               For_Each_PK (T, Process_PK'Access, True);
               TIO.Put_Line (Fbody, "end if;");
               TIO.Put_Line (Fbody, "end ""="";");
               TIO.New_Line (Fbody);
            end if;
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Equals;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  function <field_name> (<row_name>) return <type>
      --  function <field_name> (Detached_<row_name>) return <type>
      --           for strings, use to_string
      --  if the field is also FK then
      --  function <field_name> (<row_name>) return <row_name>'Class
      --           also if the FK can be null then
      --           "and then Self.Data.Follow_LJ"
      --  function <field_name> (Detached_<row_name>) return <row_name>'Class
      --  For the explicit reverse relations, emit:
      --     function <reverse_relation_name> (Self : <row_name>)
      --     function <reverse_relation_name> (Self : Detached_<row_name>)
      --     function <reverse_relation_name> (Self : I_<row_name>:Managers...
      --  For the implicit reverse relations
      --  (formed as <pointing_table>_<FK_name>_id, emit:
      --  same
      ----------------------------------------------------------------------
      procedure Emit_Body_Elements (Schema : DB_Schema) is
         T0 : Table_Description;
         Num : Integer;

         --  used by Process_Field and Process_MFK
         procedure Emit_Function_Get_Params (TPointer   : Table_Description;
                                             Pted_Table : Table_Description;
                                             With_ORM   : Boolean);
         procedure Emit_Function_Get_Params (TPointer   : Table_Description;
                                             Pted_Table : Table_Description;
                                             With_ORM   : Boolean)
         is
            NPK : Integer;
            procedure Emit_Get_Params_PK (PK : in out Field);
            procedure Emit_Get_Params_PK (PK : in out Field)
            is
               PKName : constant String := Capitalize (PK.Name);
               FK : constant Field := Pointer_Field (PK, TPointer);
               FKName : constant String := Capitalize (FK.Name);
            begin
               Add (PKName & " => ");
               if With_ORM then
                  if Ada_Param (FK) = "String" then
                     Add ("To_String (");
                     Add ("D.ORM_" & FKName & ")");
                  else
                     Add ("D.ORM_" & FKName);
                  end if;
               else
                  Add ("Self." & FKName);
               end if;
               NPK := NPK + 1;
               if NPK < Num_PKs (Pted_Table) then
                  Add (",");
               end if;
            end Emit_Get_Params_PK;
         begin
            NPK := 0;
            For_Each_PK (Pted_Table, Emit_Get_Params_PK'Access, True);
         end Emit_Function_Get_Params;

         --  emit parts 1, 2, 3 and 4
         procedure Process_Field (T : in out Table_Description;
                                  F : in out Field);
         procedure Process_Field (T : in out Table_Description;
                                  F : in out Field) is
            TName      : constant String := Capitalize (T.Name);
            RName      : constant String := Capitalize (T.Row_Name);
            FName      : constant String := Capitalize (F.Name);
            RType      : constant String := Ada_Return (F);
            Pted_Table : Table_Description;
         begin
            if Is_Abstract (T) then
               return;
            end if;

            Start_Section (FName & " of " & TName & " (1)");
            Emit_Section_Title (TName & "." & FName & " (1)", 0, 1);
            Add_Line ("function " &
                        FName & " (Self : " &
                        RName & ") return " &
                        RType & " is");
            Add_Line ("begin");
            Add_Line ("return " &
                        Value_From_DB (F) &
                        " (Self, F_" &
                        TName & "_" &
                        FName & ");");
            Add_Line ("end " &
                        FName & ";");

            Start_Section (FName & " of " & TName & " (2)");
            Emit_Section_Title (TName & "." & FName & " (2)", 1, 1);
            Add_Line ("function " &
                        FName &
                        " (Self : Detached_" &
                        RName &
                        ") return " &
                        Ada_Return (F) & " is");
            Add_Line ("begin");
            if Ada_Return (F) = "String" then
               Add_Line ("return " &
                           "To_String (" &
                           RName &
                           "_Data (Self.Unchecked_Get).ORM_" &
                           FName & ");");
            else
               Add_Line ("return " &
                           RName &
                           "_Data (Self.Unchecked_Get).ORM_" &
                           FName & ";");
            end if;
            Add_Line ("end " & FName & ";");

            if Is_FK (F) then
               Pted_Table := Pointed_Table (F);
               if Pted_Table /= No_Table then
                  declare
                     TPointer : constant Table_Description
                       := Table_Description (F.Get_Table);
                     Pted_FName : constant String :=
                       Capitalize (Pointed_Field (F).Name);
                     PName  : constant String := Capitalize (Pted_Table.Name);
                     PRName : constant String
                       := Capitalize (Pted_Table.Row_Name);
                  begin
                     Start_Section (FName & " of " & TName & " (3)");
                     Emit_Section_Title (TName & "." & FName & " (3)", 1, 1);
                     Add_Line ("function " &
                                 FName &
                                 " (Self : " &
                                 RName &
                                 ") return " &
                                 PRName &
                                 "'Class is");
                     Add_Line ("begin");
                     Add_Line ("if Current (Self.Current) /= " &
                                 "Self.Index then");
                     Add_Line ("raise Cursor_Has_Moved;");
                     Add_Line ("end if;");
                     Add_Line ("");
                     Add ("if Self.Depth > 0 ");
                     if F.Can_Be_Null then
                        Add ("and then Self.Data.Follow_LJ ");
                     end if;
                     Add_Line ("then");
                     Add_Line ("return I_" &
                                 PName &
                                 ".Internal_Element");
                     Add_Line (" (Self,");
                     Add_Line (" Upto_" &
                                 TName &
                                 "_" &
                                 Image (Pointer_Order (F), 1) &
                                 " (Self.Depth, " &
                                 "Self.Data.Follow_LJ));");
                     Add_Line ("else");
                     Add_Line ("if not Dynamic_Fetching then");
                     Add_Line ("raise Field_Not_Available with ");
                     Add_Line ("""Dynamic fetching disabled for " &
                                 FName & """;");
                     Add_Line ("end if;");
                     Add_Line ("");
                     Add_Line ("return Filter (All_" &
                                 PName &
                                 ", ");
                     Add_Line (Pted_FName & " => ");
                     Add_Line ("Self." & FName);
                     Add_Line (")");
                     Add_Line (".Limit (1).Get (Self.Data.Session).Element;");
                     Add_Line ("end if;");
                     Add_Line ("end " & FName & ";");

                     Start_Section (FName & " of " & TName & " (4)");
                     Emit_Section_Title (TName & "." & FName & " (4)", 1, 1);
                     Add_Line ("function " &
                                 FName &
                                 " (Self : Detached_" &
                                 RName &
                                 ") return Detached_" &
                                 PRName &
                                 "'Class");
                     Add_Line ("is");
                     Add_Line ("D : constant " &
                                 RName &
                                 "_Data := " &
                                 RName &
                                 "_Data (Self.Unchecked_Get);");
                     Add_Line ("S : Session_Type;");
                     Add_Line ("begin");
                     Add_Line ("if D.ORM_FK_" &
                                 FName & " = null then");
                     Add_Line ("if not Dynamic_Fetching" &
                                 " then");
                     Add_Line ("   raise Field_" &
                                 "Not_Available " &
                                 "with");
                     Add_Line (" ""Dynamic fetching " &
                                 "disabled for " &
                                 FName & """;");
                     Add_Line ("end if;");
                     Add_Line ("S := Session (Self);");
                     Add_Line ("if S = No_Session then");
                     Add_Line ("raise Field_Not_Available with");
                     Add_Line (" ""Element is detached from any session"";");
                     Add_Line ("end if;");
                     Add_Line ("D.ORM_FK_" &
                                 FName &
                                 " := new Detached_" &
                                 PRName &
                                 "'Class'");
                     Add_Line ("(Get_" &
                                 PRName & " (S, ");
                     Emit_Function_Get_Params (TPointer   => TPointer,
                                               Pted_Table => Pted_Table,
                                               With_ORM   => True);
                     Add_Line ("));");
                     Add_Line ("end if;");
                     Add_Line ("return D.ORM_FK_" &
                                 FName & ".all;");
                     Add_Line ("end " &
                                 FName & ";");
                  end;
               end if;
            end if;
            Add_Line ("");
         end Process_Field;

         --  emits parts 5, 6 and 7
         procedure Process_Reverse_Relation (FK : in out Field);
         procedure Process_Reverse_Relation (FK : in out Field) is
            Rev : constant GNAT.Strings.String_Access := FK.Reverse_Relation;
            use GNAT.Strings;
            procedure Process_Effective_Reverse_Relation (FK      : Field;
                                                          RevName : String);
            procedure Process_Effective_Reverse_Relation (FK      : Field;
                                                          RevName : String) is
               FName    : constant String := Capitalize (FK.Name);
               TPointed : constant Table_Description := Pointed_Table (FK);
               NPointed : constant String := Capitalize (TPointed.Name);
               RPointed : constant String := Capitalize (TPointed.Row_Name);
               PFName   : constant String :=
                 Capitalize (Pointed_Field (FK).Name);
               TPointer : constant Table_Description
                 := Table_Description (FK.Get_Table);
               NPointer : constant String := Capitalize (TPointer.Name);
               RPointer : constant String := Capitalize (TPointer.Row_Name);
               Qual0 : constant String :=
                 (if Is_Multiple_FK (FK) then "." & FName
                  else "");
               Qual1 : constant String :=
                 (if Is_Multiple_FK (FK)
                  then " (" & RPointer & "." & FName & ")"
                  else "");
               Qual : constant String :=
                 (if Is_Multiple_FK (FK) then "_" & FName
                  else "");
            begin
               Start_Section (RevName & Qual0 & " (5)");
               Emit_Section_Title (RevName & Qual1 & " (5)", 0, 1);
               Add_Line ("function " &
                           RevName & Qual &
                           " (Self : " &
                           RPointed &
                           "'Class)");
               Add_Line ("return " &
                           NPointer &
                           "_Managers is");
               Add_Line ("begin");
               Add_Line ("return Filter (All_" &
                           NPointer &
                           ", ");
               Add_Line (FName &
                           " => " &
                           "Self." & PFName);
               Add_Line (");");
               Add_Line ("end " &
                           RevName & Qual &
                           ";");

               Start_Section (RevName & Qual0 & " (6)");
               Emit_Section_Title (RevName & Qual1 & " (6)", 1, 1);
               Add_Line ("function " &
                           RevName & Qual &
                           " (Self : Detached_" &
                           RPointed &
                           "'Class) return " &
                           NPointer &
                           "_Managers");
               Add_Line ("is");
               Add_Line ("begin");
               Add_Line ("return Filter (All_" &
                           NPointer &
                           ", ");
               Add_Line (FName &
                           " => " &
                           "Self." & PFName);

               Add_Line (");");
               Add_Line ("end " &
                           RevName & Qual &
                           ";");

               Start_Section (RevName & Qual0 & " (7)");
               Emit_Section_Title (RevName & Qual1 & " (7)", 1, 1);
               Add_Line ("function " &
                           RevName & Qual &
                           " (Self : I_" &
                           NPointed &
                           "_Managers'Class) return " &
                           NPointer &
                           "_Managers");
               Add_Line ("  is");
               Add_Line ("Q : constant SQL_Query :=");
               Add_Line ("  I_" &
                           NPointed &
                           ".Build_Query (Self, +DBA." &
                           NPointed &
                           "." &
                           PFName &
                           ");");
               Add_Line ("begin");
               Add_Line ("return All_" &
                           NPointer &
                           ".Filter (SQL_In (DBA." &
                           NPointer &
                           "." &
                           FName &
                           ", Q));");
               Add_Line ("end " &
                           RevName & Qual &
                           ";");
               Add_Line ("");
            end Process_Effective_Reverse_Relation;
         begin
            if Rev = null or else Rev.all = "" then
               declare
                  FName    : constant String := Capitalize (FK.Name);
                  NPointer : constant String := Capitalize (FK.Get_Table.Name);
               begin
                  Process_Effective_Reverse_Relation
                    (FK, NPointer & "_" & FName & "_Id");
               end;
            else
               declare
                  RevName : constant String := Capitalize (Rev.all);
               begin
                  Process_Effective_Reverse_Relation (FK, RevName);
               end;
            end if;
         end Process_Reverse_Relation;

         --  emit part 8 and 9
         procedure Process_MFK (Pted_Table   : in out Table_Description;
                                All_Not_Null : in out Boolean);
         procedure Process_MFK (Pted_Table    : in out Table_Description;
                                All_Not_Null  : in out Boolean) is
            PTName : constant String := Capitalize (Pted_Table.Name);
            PRName : constant String := Capitalize (Pted_Table.Row_Name);
            RName  : constant String := Capitalize (T0.Row_Name);
         begin
            Start_Section (PTName & " (8)");
            Emit_Section_Title (PTName & " (8)", 1, 1);
            Add_Line ("function " &
                            PTName & " (Self : " &
                            RName & ") return " &
                            PRName & "'Class is");
            Add_Line ("begin");
            Add_Line ("if Current (Self.Current) /= " &
                            "Self.Index then");
            Add_Line ("raise Cursor_Has_Moved;");
            Add_Line ("end if;");
            Add_Line ("");
            Add ("if Self.Depth > 0 ");
            if not All_Not_Null then
               Add ("and then Self.Data.Follow_LJ ");
            end if;
            Add_Line ("then");
            Add_Line ("return");
            Add_Line ("I_" &
                            PTName & ".Internal_Element");
            Add_Line ("(Self, Upto_Persons_" &
                            Image (Num, 1) &
                            "(Self.Depth, Self.Data.Follow_LJ));");
            Add_Line ("else");
            Add_Line ("if not Dynamic_Fetching then");
            Add_Line ("raise Field_Not_Available");
            Add_Line ("with ""Dynamic fetching disabled " &
                            "for " &
                            PTName & """;");
            Add_Line ("end if;");
            Add_Line ("");
            Add_Line ("return");
            Add_Line ("Filter (All_" &
                            PTName & ", ");
            Emit_Function_Get_Params (TPointer   => T0,
                                      Pted_Table => Pted_Table,
                                      With_ORM   => False);
            Add_Line (").Limit (1).Get");
            Add_Line ("(Self.Data.Session)");
            Add_Line (".Element;");
            Add_Line ("end if;");
            Add_Line ("end " & PTName & ";");

            Start_Section (PTName & " (9)");
            Emit_Section_Title (PTName & " (9)", 1, 1);
            Add_Line ("function " &
                            PTName & " (Self : Detached_" &
                            RName & ") return Detached_" &
                            PRName & "'Class is");
            Add_Line ("D : constant " &
                            RName & "_Data := " &
                            RName & "_Data (Self.Unchecked_Get);");
            Add_Line ("S : Session_Type;");
            Add_Line ("begin");
            Add_Line ("if D.ORM_FK_" &
                            PTName & " = null then");
            Add_Line ("if not Dynamic_Fetching then");
            Add_Line ("raise Field_Not_Available");
            Add_Line ("with ""Dynamic fetching " &
                            "disabled for " &
                            PTName & """;");
            Add_Line ("end if;");
            Add_Line ("S := Session (Self);");
            Add_Line ("if S = No_Session then");
            Add_Line ("raise Field_Not_Available");
            Add_Line ("with ""Element is detached from" &
                            " any session"";");
            Add_Line ("end if;");
            Add_Line ("D.ORM_FK_" &
                            PTName & " :=");
            Add_Line ("new Detached_" &
                            PRName & "'Class'");
            Add_Line ("(Get_" &
                            PRName & " (S, ");
            Emit_Function_Get_Params (TPointer   => T0,
                                      Pted_Table => Pted_Table,
                                      With_ORM   => True);
            Add_Line ("));");
            Add_Line ("end if;");
            Add_Line ("return D.ORM_FK_" &
                            PTName & ".all;");
            Add_Line ("end " & PTName & ";");
            Add_Line ("");
            Num := Num + 1;
         end Process_MFK;

         procedure Process_Table_MFK (T : in out Table_Description);
         procedure Process_Table_MFK (T : in out Table_Description) is
         begin
            T0 := T;
            Num := Num_FKs (T);
            For_Each_MFK (T, Process_MFK'Access);
            Add_Line ("");
         end Process_Table_MFK;

         procedure Process_Table_FK (T : in out Table_Description);
         procedure Process_Table_FK (T : in out Table_Description) is
         begin
            T0 := T;
            For_Each_FK (T, Process_Reverse_Relation'Access);
         end Process_Table_FK;
      begin
         For_Each_Ordered_Field (Process_Field'Access);

         For_Each_Table (Schema, Process_Table_MFK'Access, True);
         --  Print_Sections (Fbody);

         For_Each_Table (Schema, Process_Table_FK'Access, True);
         Print_Sections (Fbody);
      end Emit_Body_Elements;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  There are two versions: one for tables with one Pk integer
      --  and the other for all other tables and views
      ----------------------------------------------------------------------
      procedure Emit_Body_Detach (Schema : DB_Schema) is
         First_Table : Boolean := True;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
            PK    : Field;
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if First_Table then
               First_Table := False;
               Emit_Section_Title (Fbody, "Detach " & RName, 0, 1);
            else
               Emit_Section_Title (Fbody, "Detach " & RName, 1, 1);
            end if;
            TIO.Put_Line (Fbody, "function Detach (Self : " &
                            RName &
                            "'Class) return Detached_" &
                            RName & "'Class");
            TIO.Put_Line (Fbody, "is");
            if Has_Cache (T) then
               PK := Get_First_PK (T);
               TIO.Put_Line (Fbody, "R : constant Detached_" &
                               RName &
                               "'Class := From_Cache (Self.Data.Session," &
                               " Self." &
                               Capitalize (PK.Name) & ");");
               TIO.Put_Line (Fbody, "begin");
               TIO.Put_Line (Fbody, "if R.Is_Null then");
               TIO.Put_Line (Fbody, "return Detach_No_Lookup " &
                               "(Self, Self.Data.Session);");
               TIO.Put_Line (Fbody, "else");
               TIO.Put_Line (Fbody, "return R;");
               TIO.Put_Line (Fbody, "end if;");
            else
               TIO.Put_Line (Fbody, "begin");
               TIO.Put_Line (Fbody, "return Detach_No_Lookup " &
                               "(Self, Self.Data.Session);");
            end if;
            TIO.Put_Line (Fbody, "end Detach;");
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, False);
      end Emit_Body_Detach;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  Some conditions to emit some part of the code:
      --     num_FKs(T) > 0
      --     FK.Can_Be_Null
      ----------------------------------------------------------------------
      procedure Emit_Body_Detach_No_Lookup_Body (Schema : DB_Schema) is
         T0  : Table_Description;
         Num : Integer;

         procedure Process_MFK_As_Locally_Declared
           (Pted_Table   : in out Table_Description;
            All_Not_Null : in out Boolean);
         procedure Process_MFK_As_Locally_Declared
           (Pted_Table : in out Table_Description;
            All_Not_Null  : in out Boolean)
         is
            pragma Unreferenced (All_Not_Null);
            PTName  : constant String := Capitalize (Pted_Table.Name);
            PTRName : constant String := Capitalize (Pted_Table.Row_Name);
         begin
            --  Start_Section ("Fk_" & PTName);
            --  Add_Line ("Fk_" & PTName &
            --              " : Detached_" &
            --              PTRName &
            --              "_Access;");
            TIO.Put_Line (Fbody, "Fk_" & PTName &
                            " : Detached_" &
                            PTRName &
                            "_Access;");
         end Process_MFK_As_Locally_Declared;

         procedure Process_FK_As_Locally_Declared (FK : in out Field);
         procedure Process_FK_As_Locally_Declared (FK : in out Field) is
            --  TName  : constant String := Capitalize (T0.Name);
            FKName : constant String := Capitalize (FK.Name);
            PT     : Table_Description;
         begin
            PT := Pointed_Table (FK); --  cannot be no_table
            --  Start_Section ("Fk_" & FKName & " of " & TName);
            --  Add_Line ("Fk_" & FKName &
            --                  " : Detached_" &
            --                  Capitalize (PT.Row_Name) &
            --                  "_Access;");
            --  Start_Section ("Fk_" & FKName & " of " & TName);
            TIO.Put_Line (Fbody, "Fk_" & FKName &
                            " : Detached_" &
                            Capitalize (PT.Row_Name) &
                            "_Access;");
         end Process_FK_As_Locally_Declared;

         procedure Process_MFK_Field (Pted_Table   : in out Table_Description;
                                      All_Not_Null : in out Boolean);
         procedure Process_MFK_Field (Pted_Table   : in out Table_Description;
                                      All_Not_Null : in out Boolean) is
            TName   : constant String := Capitalize (T0.Name);
            PTName  : constant String := Capitalize (Pted_Table.Name);
            PTRName : constant String := Capitalize (Pted_Table.Row_Name);
         begin
            --  Start_Section ("FK_" & PTName);
            --  if not All_Not_Null then
            --     Add_Line ("if LJ then");
            --  end if;
            --  Add_Line ("FK_" & PTName &
            --              " := new Detached_" &
            --              PTRName & "'Class'(");
            --  Add_Line ("I_" & PTName & ".Internal_Element");
            --  Add_Line (" (Self, Upto_" &
            --              TName &
            --              "_" &
            --              Image (Num, 1) &
            --              " (Self.Depth, LJ)).Detach);");
            --  if not All_Not_Null then
            --     Add_Line ("end if;");
            --  end if;
            --  Start_Section ("FK_" & PTName);
            if not All_Not_Null then
               TIO.Put_Line (Fbody, "if LJ then");
            end if;
            TIO.Put_Line (Fbody, "FK_" & PTName &
                            " := new Detached_" &
                            PTRName & "'Class'(");
            TIO.Put_Line (Fbody, "I_" & PTName & ".Internal_Element");
            TIO.Put_Line (Fbody, " (Self, Upto_" &
                            TName &
                            "_" &
                            Image (Num, 1) &
                            " (Self.Depth, LJ)).Detach);");
            if not All_Not_Null then
               TIO.Put_Line (Fbody, "end if;");
            end if;
            Num := Num + 1;
         end Process_MFK_Field;

         procedure Process_FK_Field (FK : in out Field);
         procedure Process_FK_Field (FK : in out Field) is
            TName  : constant String := Capitalize (T0.Name);
            FName : constant String := Capitalize (FK.Name);
            PT : constant Table_Description := Pointed_Table (FK);
            PTName  : constant String := Capitalize (PT.Name);
            PTRName : constant String := Capitalize (PT.Row_Name);
         begin
            --  Start_Section ("FK_" & FName);
            --  if FK.Can_Be_Null then
            --     Add_Line ("if LJ then");
            --  end if;
            --  Add_Line ("FK_" & FName &
            --              " := new Detached_" &
            --              PTRName & "'Class'(");
            --  Add_Line ("I_" &
            --              PTName & ".Internal_Element");
            --  Add_Line (" (Self, Upto_" &
            --              TName &
            --              "_" &
            --              Image (Pointer_Order (FK), 1) &
            --              " (Self.Depth, LJ)).Detach);");
            --  if FK.Can_Be_Null then
            --     Add_Line ("end if;");
            --  end if;
            --  Start_Section ("FK_" & FName);
            if FK.Can_Be_Null then
               TIO.Put_Line (Fbody, "if LJ then");
            end if;
            TIO.Put_Line (Fbody, "FK_" & FName &
                            " := new Detached_" &
                            PTRName & "'Class'(");
            TIO.Put_Line (Fbody, "I_" &
                            PTName & ".Internal_Element");
            TIO.Put_Line (Fbody, " (Self, Upto_" &
                            TName &
                            "_" &
                            Image (Pointer_Order (FK), 1) &
                            " (Self.Depth, LJ)).Detach);");
            if FK.Can_Be_Null then
               TIO.Put_Line (Fbody, "end if;");
            end if;
         end Process_FK_Field;

         procedure Process_MFK_TMP_ORM
           (Pted_Table   : in out Table_Description;
            All_Not_Null : in out Boolean);
         procedure Process_MFK_TMP_ORM
           (Pted_Table   : in out Table_Description;
            All_Not_Null : in out Boolean) is
            pragma Unreferenced (All_Not_Null);
            PTName  : constant String := Capitalize (Pted_Table.Name);
         begin
            Start_Section ("Tmp.ORM_FK_" & PTName);
            Add_Line ("Tmp.ORM_FK_" & PTName &
                            " := FK_" & PTName & ";");
         end Process_MFK_TMP_ORM;

         procedure Process_FK_TMP_ORM (FK : in out Field);
         procedure Process_FK_TMP_ORM (FK : in out Field) is
            TName    : constant String := Capitalize (T0.Name);
            FName : constant String := Capitalize (FK.Name);
         begin
            Start_Section ("Tmp.ORM_FK_" & FName & " of " & TName);
            Add_Line ("Tmp.ORM_FK_" & FName &
                            " := FK_" & FName & ";");
         end Process_FK_TMP_ORM;

         procedure Process_Field (F : in out Field);
         procedure Process_Field (F : in out Field) is
            FName    : constant String := Capitalize (F.Name);
            TName    : constant String := Capitalize (T0.Name);
            SQL_Name : constant String := SQL_Field (F);
         begin
            Start_Section ("Tmp.ORM_" & FName & " of " & TName);
            Add ("Tmp.ORM_" & FName &
                       " := ");
            if SQL_Name = "SQL_Field_Text" then
               Add ("To_Unbounded_String (" &
                          "String_Value");
               Add_Line (" (Self, F_" &
                               TName & "_" &
                               FName & "));");
            else
               Add (Value_From_DB (F));
               Add_Line (" (Self, F_" &
                               TName & "_" &
                               FName & ");");
            end if;
         end Process_Field;

         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            RName  : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            T0 := T;
            Emit_Section_Title (Fbody, "Detach_No_Lookup " & RName, 1, 1);
            TIO.Put_Line (Fbody, "function Detach_No_Lookup");
            TIO.Put_Line (Fbody, "  (Self    : " &
                            RName &
                            "'Class;");
            TIO.Put_Line (Fbody, "Session : Session_Type)");
            TIO.Put_Line (Fbody, "  return Detached_" &
                            RName &
                            "'Class");
            TIO.Put_Line (Fbody, "is");
            TIO.Put_Line (Fbody, "Default" &
                            " : Detached_" &
                            RName & ";");
            TIO.Put_Line (Fbody, "Result " &
                            " : Detached_" &
                            RName &
                            "'Class := Detached_" &
                            RName &
                            "'Class (Session.Factory (Self, Default));");
            if Has_FKs (T) then
               For_Each_FK (T, Process_FK_As_Locally_Declared'Access);
               --  Print_Sections (Fbody);
               For_Each_MFK (T, Process_MFK_As_Locally_Declared'Access);
               --  Print_Sections (Fbody);

               TIO.Put_Line (Fbody, "Lj" &
                               " : " &
                               "constant Boolean" &
                               " := Self.Data.Follow_LJ;");
            end if;
            TIO.Put_Line (Fbody, "Tmp    " &
                            " : " &
                            RName &
                            "_Data;");
            TIO.Put_Line (Fbody, "begin");
            TIO.Put_Line (Fbody, "if Result.Is_Null then");
            TIO.Put_Line (Fbody, "Result.Set (" &
                            RName &
                            "_DDR'");
            TIO.Put_Line (Fbody, "     (Detached_Data with " &
                            "Field_Count => " &
                            Image (Num_Fields (T) +
                              Num_FKs (T) +
                              Num_MFKs (T), 1) &
                            ", others => <>));");
            TIO.Put_Line (Fbody, "end if;");
            TIO.Put_Line (Fbody, "");
            TIO.Put_Line (Fbody, "Tmp := " & RName &
                            "_Data (Result.Unchecked_Get);");
            if Has_FKs (T) then
               TIO.Put_Line (Fbody, "if Self.Depth > 0 then");
               For_Each_FK (T, Process_FK_Field'Access);
               --  Print_Sections (Fbody);
               Num := Num_FKs (T);
               For_Each_MFK (T, Process_MFK_Field'Access);
               --  Print_Sections (Fbody);
               TIO.Put_Line (Fbody, "end if;");
            end if;
            TIO.New_Line (Fbody);
            For_Each_Table_Ordered_Field (T, Process_Field'Access);
            if Has_FKs (T) then
               For_Each_FK (T, Process_FK_TMP_ORM'Access);
               For_Each_MFK (T, Process_MFK_TMP_ORM'Access);
            end if;
            Print_Sections (Fbody);
            TIO.Put_Line (Fbody, "Session.Persist (Result);");
            TIO.Put_Line (Fbody, "return Result;");
            TIO.Put_Line (Fbody, "end Detach_No_Lookup;");
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Detach_No_Lookup_Body;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  For tables (no views): emit PK_Only if num PKs > 0
      --  Other conditions: see the code
      ----------------------------------------------------------------------
      procedure Emit_Body_Do_Query_Body (Schema : DB_Schema) is
         T0 : Table_Description;
         First_Field : Boolean;
         Last_FK_That_Cannot_Be_Null : Integer;
         Num : Integer;

         procedure Emit_Body_MFK (Pted_Table   : in out Table_Description;
                                  All_Not_Null : in out Boolean);
         procedure Emit_Body_MFK (Pted_Table   : in out Table_Description;
                                  All_Not_Null : in out Boolean) is
            PTName : constant String  := Capitalize (Pted_Table.Name);
         begin
            TIO.New_Line (Fbody);
            if not All_Not_Null then
               TIO.Put_Line (Fbody, "if Follow_LJ then");
            end if;
            TIO.Put_Line (Fbody, "C2 := No_Criteria;");
            TIO.Put_Line (Fbody, "Do_Query_" &
                            PTName &
                            " (Fields, T, C2, Aliases (Base + " &
                            Image (Num, 1) &
                            "),");
            TIO.Put_Line (Fbody, "   Aliases, " &
                            "Depth - 1, Follow_LJ);");
            TIO.Put_Line (Fbody, "if Depth > 1 then");
            TIO.Put_Line (Fbody, "   Criteria := Criteria and C2;");
            TIO.Put_Line (Fbody, "end if;");
            if All_Not_Null then
               TIO.Put_Line (Fbody, "From := From & T;");
            else
               TIO.Put_Line (Fbody, "end if;");
            end if;
            Num := Num + 1;
         end Emit_Body_MFK;

         procedure Emit_Body_FK (FK : in out Field);
         procedure Emit_Body_FK (FK : in out Field) is
            procedure Emit_Body_FK_Can_Be_Null (FK : Field);
            procedure Emit_Body_FK_Can_Be_Null (FK : Field) is
               PT     : constant Table_Description := Pointed_Table (FK);
               PTName : constant String  := Capitalize (PT.Name);
               Num    : constant Integer := Pointer_Order (FK) + 1;
            begin
               if Num > 1 then
                  TIO.New_Line (Fbody);
               end if;
               TIO.Put_Line (Fbody, "if Follow_LJ then");
               TIO.Put_Line (Fbody, "C2 := No_Criteria;");
               TIO.Put_Line (Fbody, "Do_Query_" &
                               PTName);
               TIO.Put_Line (Fbody, "(Fields,");
               TIO.Put_Line (Fbody, "T,");
               TIO.Put_Line (Fbody, "C2,");
               TIO.Put_Line (Fbody, "Aliases (Base + " &
                               Image (Num, 1) & "),");
               TIO.Put_Line (Fbody, "Aliases,");
               TIO.Put_Line (Fbody, "Depth - 1,");
               TIO.Put_Line (Fbody, "Follow_LJ);");
               TIO.Put_Line (Fbody, "if Depth > 1 then");
               TIO.Put_Line (Fbody, "Criteria := Criteria and C2;");
               TIO.Put_Line (Fbody, "end if;");
               TIO.Put_Line (Fbody, "end if;");
            end Emit_Body_FK_Can_Be_Null;
            procedure Emit_Body_FK_Cannot_Be_Null (FK : Field);
            procedure Emit_Body_FK_Cannot_Be_Null (FK : Field) is
               PT     : constant Table_Description := Pointed_Table (FK);
               PTName : constant String  := Capitalize (PT.Name);
               Num    : constant Integer := Pointer_Order (FK) + 1;
            begin
               if First_Field then
                  First_Field := False;
               else
                  TIO.New_Line (Fbody);
               end if;
               TIO.Put_Line (Fbody, "C2 := No_Criteria;");
               TIO.Put_Line (Fbody, "Do_Query_" &
                               PTName &
                               " (Fields, T, C2, Aliases (Base + " &
                               Image (Num, 1) &
                               "),");
               TIO.Put_Line (Fbody, "   Aliases, " &
                               "Depth - 1, Follow_LJ);");
               TIO.Put_Line (Fbody, "if Depth > 1 then");
               TIO.Put_Line (Fbody, "   Criteria := Criteria and C2;");
               TIO.Put_Line (Fbody, "end if;");
               TIO.Put_Line (Fbody, "From := From & T;");
               if Num < Num_FKs (T0) then
                  TIO.New_Line (Fbody);
               end if;
            end Emit_Body_FK_Cannot_Be_Null;
         begin
            if FK.Can_Be_Null then
               Emit_Body_FK_Can_Be_Null (FK);
            else
               Emit_Body_FK_Cannot_Be_Null (FK);
            end if;
         end Emit_Body_FK;

         procedure Emit_FK_Section;
         procedure Emit_FK_Section is
            First_Left_Join : Boolean := True;
            First_Join      : Boolean := True;

            procedure Emit_Declarations_MFK
              (Pted_Table   : in out Table_Description;
               All_Not_Null : in out Boolean);
            procedure Emit_Declarations_MFK
              (Pted_Table   : in out Table_Description;
               All_Not_Null : in out Boolean)
            is
               pragma Unreferenced (All_Not_Null);
               PTName : constant String := Capitalize (Pted_Table.Name);
            begin
               TIO.Put_Line (Fbody, "   FK" &
                               Image (Num, 1) &
                               " : T_Numbered_" &
                               PTName &
                               " (Aliases (Aliases (Base + " &
                               Image (Num, 1) &
                               ")));");
               Num := Num + 1;
            end Emit_Declarations_MFK;

            procedure Emit_Declarations_FK (FK : in out Field);
            procedure Emit_Declarations_FK (FK : in out Field) is
               PT     : constant Table_Description := Pointed_Table (FK);
               PTName : constant String := Capitalize (PT.Name);
               Num    : constant Integer := Pointer_Order (FK) + 1;
            begin
               TIO.Put_Line (Fbody, "   FK" &
                               Image (Num, 1) &
                               " : T_Numbered_" &
                               PTName &
                               " (Aliases (Aliases (Base + " &
                               Image (Num, 1) &
                               ")));");
            end Emit_Declarations_FK;

            procedure Pointed_MField_List
              (Pted_Table   : in out Table_Description;
               All_Not_Null : in out Boolean);
            procedure Pointed_MField_List
              (Pted_Table   : in out Table_Description;
               All_Not_Null : in out Boolean)
            is
               procedure Emit_And (FK : in out Field);
               procedure Emit_And (FK : in out Field) is
                  FName  : constant String := Capitalize (FK.Name);
                  PFName : constant String :=
                    Capitalize (Pointed_Field (FK).Name);
               begin
                  if Table_Description (Pointed_Field (FK).Get_Table) =
                    Pted_Table
                  then
                     if All_Not_Null then
                        TIO.Put (Fbody, " and Table." &
                                   FName &
                                   " = FK" &
                                   Image (Num, 1) &
                                   "." &
                                   PFName);
                     end if;
                  end if;
               end Emit_And;
            begin
               For_Each_FK (T0, Emit_And'Access);
               Num := Num + 1;
            end Pointed_MField_List;

            procedure Pointed_Field_List (FK : in out Field);
            procedure Pointed_Field_List (FK : in out Field) is
               PF     : constant Field   := Pointed_Field (FK);
               PFName : constant String  := Capitalize (PF.Name);
               FName  : constant String  := Capitalize (FK.Name);
               Num    : constant Integer := Pointer_Order (FK) + 1;
            begin
               if not FK.Can_Be_Null then
                  TIO.Put (Fbody, " and Table." &
                             FName &
                             " = FK" &
                             Image (Num, 1) &
                             "." &
                             PFName);
                  if Field_Order (T0, FK) = Last_FK_That_Cannot_Be_Null
                  then
                     null;
                     --  TIO.Put_Line (Fbody, ";");
                  else
                     null;
                     --  TIO.New_Line (Fbody);
                  end if;
               end if;
            end Pointed_Field_List;

            procedure Emit_Join (FK : in out Field);
            procedure Emit_Join (FK : in out Field) is
               PF     : constant Field   := Pointed_Field (FK);
               PFName : constant String  := Capitalize (PF.Name);
               FName  : constant String  := Capitalize (FK.Name);
               Num    : constant Integer := Pointer_Order (FK) + 1;
            begin
               if FK.Can_Be_Null then
                  if First_Join then
                     First_Join := False;
                     TIO.Put_Line (Fbody, "(Table, ");
                  end if;
                  TIO.Put (Fbody, "FK" &
                             Image (Num, 1) & ", Table." & FName &
                             " = FK" & Image (Num, 1) & "." & PFName &
                             ")");
                  if Field_Order (T0, FK) < Last_FK_Can_Be_Null (T0) then
                     TIO.Put_Line (Fbody, ",");
                  end if;
               end if;
            end Emit_Join;

            procedure Emit_MJoin (Pted_Table   : in out Table_Description;
                                  All_Not_Null : in out Boolean);
            procedure Emit_MJoin (Pted_Table   : in out Table_Description;
                                  All_Not_Null : in out Boolean) is
               First_Equal : Boolean := True;
               procedure Emit_FK_Equals (FK : in out Field);
               procedure Emit_FK_Equals (FK : in out Field) is
                  FName  : constant String := Capitalize (FK.Name);
                  PFName : constant String :=
                    Capitalize (Pointed_Field (FK).Name);
               begin
                  if Table_Description (Pointed_Field (FK).Get_Table) =
                    Pted_Table
                  then
                     if First_Equal then
                        First_Equal := False;
                     else
                        TIO.Put (Fbody, " and ");
                     end if;
                     TIO.Put (Fbody, " Table." &
                                FName &
                                " = FK" &
                                Image (Num, 1) &
                                "." &
                                PFName);
                  end if;
               end Emit_FK_Equals;
            begin
               if All_Not_Null then
                  Num := Num + 1;
                  return;
               end if;
               TIO.Put_Line (Fbody, ",");
               TIO.Put_Line (Fbody, "FK" & Image (Num, 1) & ",");
               For_Each_FK (T0, Emit_FK_Equals'Access);
               TIO.Put_Line (Fbody, ")");
               Num := Num + 1;
            end Emit_MJoin;

            procedure Emit_Left_Join (FK : in out Field);
            procedure Emit_Left_Join (FK : in out Field) is
            begin
               if FK.Can_Be_Null then
                  if First_Left_Join then
                     TIO.Put_Line (Fbody, "+Left_Join");
                     First_Left_Join := False;
                  else
                     TIO.Put_Line (Fbody, "(Left_Join");
                  end if;
               end if;
            end Emit_Left_Join;

            procedure Emit_MFK_Left_Join
              (Pted_Table   : in out Table_Description;
               All_Not_Null : in out Boolean);
            procedure Emit_MFK_Left_Join
              (Pted_Table   : in out Table_Description;
               All_Not_Null : in out Boolean)
            is
               pragma Unreferenced (Pted_Table);
            begin
               if not All_Not_Null then
                  TIO.Put_Line (Fbody, "(Left_Join");
               end if;
            end Emit_MFK_Left_Join;
         begin
            TIO.Put_Line (Fbody, "if Depth > 0 then");
            TIO.New_Line (Fbody);
            TIO.Put_Line (Fbody, "declare");
            For_Each_FK (T0, Emit_Declarations_FK'Access);
            Num := Num_FKs (T0) + 1;
            For_Each_MFK (T0, Emit_Declarations_MFK'Access);
            TIO.Put_Line (Fbody, "begin");
            if Some_FK_Cannot_Be_Null (T0) then
               TIO.Put_Line (Fbody, "Criteria := Criteria");
               For_Each_FK (T0, Pointed_Field_List'Access);
               Num := Num_FKs (T0) + 1;
               For_Each_MFK (T0, Pointed_MField_List'Access);
               TIO.Put_Line (Fbody, ";");
            end if;
            if Some_FK_Can_Be_Null (T0) then
               TIO.Put_Line (Fbody, "if Follow_LJ then");
               TIO.Put_Line (Fbody, "From :=");
               For_Each_FK (T0, Emit_Left_Join'Access);
               For_Each_MFK (T0, Emit_MFK_Left_Join'Access);
               For_Each_FK (T0, Emit_Join'Access);
               Num := Num_FKs (T0) + 1;
               For_Each_MFK (T0, Emit_MJoin'Access);
               TIO.Put_Line (Fbody, ";");
               TIO.Put_Line (Fbody, "else");
               TIO.Put_Line (Fbody, "From := +Table;");
               TIO.Put_Line (Fbody, "end if;");
            else
               TIO.Put_Line (Fbody, "From := +Table;");
            end if;
            For_Each_FK (T0, Emit_Body_FK'Access);
            Num := Num_FKs (T0) + 1;
            For_Each_MFK (T0, Emit_Body_MFK'Access);
            TIO.Put_Line (Fbody, "end;");
            TIO.Put_Line (Fbody, "end if;");
         end Emit_FK_Section;

         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName : constant String := Capitalize (T.Name);
            procedure Process_Fields_Only_PK (PK : in out Field);
            procedure Process_Fields_Only_PK (PK : in out Field) is
               PKName : constant String := Capitalize (PK.Name);
            begin
               TIO.Put (Fbody, " & Table." & PKName);
            end Process_Fields_Only_PK;
            procedure Process_Fields_All_Fields (F : in out Field);
            procedure Process_Fields_All_Fields (F : in out Field) is
               FName : constant String := Capitalize (F.Name);
            begin
               if First_Field then
                  First_Field := False;
                  TIO.Put (Fbody, "& Table." & FName);
               else
                  TIO.New_Line (Fbody);
                  if T0.Get_Kind = Kind_Table then
                     TIO.Put (Fbody, "         & Table." & FName);
                  else
                     TIO.Put (Fbody, "      & Table." & FName);
                  end if;
               end if;
            end Process_Fields_All_Fields;
         begin
            if Is_Abstract (T) then
               return;
            end if;
            T0 := T;
            Last_FK_That_Cannot_Be_Null := Last_FK_Cannot_Be_Null (T0);
            Emit_Section_Title (Fbody, "Do_Query_" & TName, 1, 1);
            TIO.Put_Line (Fbody, "procedure Do_Query_" & TName);
            TIO.Put_Line (Fbody, "  (Fields    : in out SQL_Field_List;");
            TIO.Put_Line (Fbody, "From      : out SQL_Table_List;");
            TIO.Put_Line (Fbody, "Criteria  : in out Sql_Criteria;");
            TIO.Put_Line (Fbody, "Base      : Natural;");
            TIO.Put_Line (Fbody, "Aliases   : Alias_Array;");
            TIO.Put_Line (Fbody, "Depth     : Natural;");
            TIO.Put (Fbody, "      Follow_LJ : Boolean");
            if T.Get_Kind = Kind_Table and then Has_PKs (T) then
               TIO.Put_Line (Fbody, ";");
               TIO.Put_Line (Fbody, "Pk_Only   : Boolean := False)");
            else
               TIO.Put_Line (Fbody, ")");
            end if;
            TIO.Put_Line (Fbody, "is");
            if T.Get_Kind = Kind_Table and then Has_PKs (T) then
               if not Has_FKs (T) then
                  TIO.Put_Line (Fbody, "pragma Unreferenced (" &
                                  "Criteria, Depth, Follow_LJ);");
               end if;
            elsif not Has_FKs (T) then
               TIO.Put_Line (Fbody, "pragma Unreferenced (" &
                               "From, Criteria, Depth, Follow_LJ);");
            end if;
            TIO.Put_Line (Fbody, "Table : T_Numbered_" &
                            TName & " (Aliases (Base));");
            if Has_FKs (T) then
               TIO.Put_Line (Fbody, "C2    : Sql_Criteria;");
               TIO.Put_Line (Fbody, "T     : SQL_Table_List;");
            end if;
            TIO.Put_Line (Fbody, "begin");
            if T.Get_Kind = Kind_Table and then Has_PKs (T) then
               TIO.Put_Line (Fbody, "if PK_Only then");
               TIO.Put (Fbody, "         Fields := Fields ");
               First_Field := True;
               For_Each_PK (T, Process_Fields_Only_PK'Access, True);
               TIO.Put_Line (Fbody, ";");
               TIO.Put_Line (Fbody, "else");
               TIO.Put (Fbody, "         Fields := Fields ");
            else
               TIO.Put (Fbody, "      Fields := Fields ");
            end if;
            First_Field := True;
            For_Each_Field (T, Process_Fields_All_Fields'Access, True);
            Print_Sections (Fbody);
            TIO.Put_Line (Fbody, ";");
            if T.Get_Kind = Kind_Table and then Has_PKs (T) then
               TIO.Put_Line (Fbody, "end if;");
            end if;
            if T.Get_Kind = Kind_Table and then Has_PKs (T) then
               TIO.Put_Line (Fbody, "From := Empty_Table_List;");
            end if;
            if Has_FKs (T) then
               First_Field := True;
               Emit_FK_Section;
            end if;
            TIO.Put_Line (Fbody, "end Do_Query_" &
                            TName & ";");
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Do_Query_Body;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  Special cases for strings and booleans
      ----------------------------------------------------------------------
      procedure Emit_Body_Filter (Schema : DB_Schema) is
         T0     : Table_Description;
         Num    : Integer;
         procedure Emit_Field_Declarations (F : in out Field);
         procedure Emit_Field_Declarations (F : in out Field) is
            FName : constant String := Capitalize (F.Name);
         begin
            Num := Num + 1;
            TIO.Put (Fbody, "      " &
                       FName &
                       " : " &
                       Ada_Param (F) &
                       " := ");
            if Ada_Param (F) = "String" then
               TIO.Put (Fbody, "No_Update");
            else
               TIO.Put (Fbody, Ada_Default_Param (F));
            end if;
            if Num = Num_Fields (T0) then
               TIO.Put_Line (Fbody, ")");
            else
               TIO.Put_Line (Fbody, ";");
            end if;
         end Emit_Field_Declarations;
         procedure Emit_Field (F : in out Field);
         procedure Emit_Field (F : in out Field) is
            FName : constant String := Capitalize (F.Name);
            TName : constant String := Capitalize (T0.Name);
         begin
            TIO.Put (Fbody, "      if " &
                       FName & " /= ");
            if Ada_Param (F) = "String" then
               TIO.Put (Fbody, "No_Update");
            else
               TIO.Put (Fbody, Ada_Default_Param (F));
            end if;
            TIO.Put_Line (Fbody, " then");
            TIO.Put (Fbody, "         C := C and DBA." &
                       TName & "." &
                       FName & " = ");
            if Ada_Param (F) = "Triboolean" then
               TIO.Put_Line (Fbody, "To_Boolean (" &
                               FName & ");");
            else
               TIO.Put_Line (Fbody, FName & ";");
            end if;
            TIO.Put_Line (Fbody, "end if;");
         end Emit_Field;

         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName : constant String := Capitalize (T.Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            T0  := T;
            Num := 0;
            Emit_Section_Title (Fbody, "Filter", 1, 1);
            TIO.Put_Line (Fbody, "function Filter");
            TIO.Put_Line (Fbody, "  (Self" &
                          " : " &
                            TName & "_Managers'Class;");
            For_Each_Field (T, Emit_Field_Declarations'Access, True);
            TIO.Put_Line (Fbody, "  return " &
                            TName & "_Managers");
            TIO.Put_Line (Fbody, "is");
            TIO.Put_Line (Fbody, "C      : Sql_Criteria := " &
                            "No_Criteria;");
            TIO.Put_Line (Fbody, "Result : " &
                            TName & "_Managers;");
            TIO.Put_Line (Fbody, "begin");
            For_Each_Field (T, Emit_Field'Access, True);
            TIO.Put_Line (Fbody, "Copy (Self.Filter (C), " &
                            "Into => Result);");
            TIO.Put_Line (Fbody, "return Result;");
            TIO.Put_Line (Fbody, "end Filter;");
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, False);
      end Emit_Body_Filter;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  Special process for FK
      ----------------------------------------------------------------------
      procedure Emit_Body_Free (Schema : DB_Schema) is
         procedure Process_MFK_Field (Pted_Table   : in out Table_Description;
                                      All_Not_Null : in out Boolean);
         procedure Process_MFK_Field (Pted_Table   : in out Table_Description;
                                      All_Not_Null : in out Boolean)
         is
            pragma Unreferenced (All_Not_Null);
            PTName : constant String := Capitalize (Pted_Table.Name);
         begin
            TIO.Put (Fbody, "      Unchecked_Free (Self.ORM_FK_");
            TIO.Put (Fbody, PTName);
            TIO.Put_Line (Fbody, ");");
         end Process_MFK_Field;

         procedure Process_FK_Field (FK : in out Field);
         procedure Process_FK_Field (FK : in out Field) is
            FName : constant String := Capitalize (FK.Name);
         begin
            TIO.Put (Fbody, "      Unchecked_Free (Self.ORM_FK_");
            TIO.Put (Fbody, FName);
            TIO.Put_Line (Fbody, ");");
         end Process_FK_Field;

         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            Emit_Section_Title (Fbody, "Free", 1, 1);
            TIO.Put_Line (Fbody, "overriding procedure Free (Self " &
                            ": in out " &
                            RName &
                            "_Ddr) is");
            TIO.Put_Line (Fbody, "begin");
            if Has_FKs (T) then
               For_Each_FK (T, Process_FK_Field'Access);
               For_Each_MFK (T, Process_MFK_Field'Access);
               TIO.New_Line (Fbody);
            end if;
            TIO.Put_Line (Fbody, "Free (Detached_Data (Self));");
            TIO.Put_Line (Fbody, "end Free;");
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Free;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  Base_Key is always incremented.
      --  Only if the table has one PK and it is integer
      ----------------------------------------------------------------------
      procedure Emit_Body_From_Cache (Schema : DB_Schema) is
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            RName  : constant String  := Capitalize (T.Row_Name);
            PK     : Field;
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if Has_Cache (T) then
               PK := Get_First_PK (T);
               Emit_Section_Title (Fbody, "From_Cache", 1, 1);
               TIO.Put_Line (Fbody, "function From_Cache");
               TIO.Put_Line (Fbody, "  (Session " &
                               ": Session_Type;");
               TIO.Put_Line (Fbody, "" &
                               Capitalize (PK.Name) &
                               ": " &
                               Ada_Param (PK) & ")");
               TIO.Put_Line (Fbody, "  return Detached_" &
                               RName & "'Class is");
               TIO.Put_Line (Fbody, "begin");
               TIO.Put (Fbody, "return Detached_" &
                          RName &
                          "'Class (Session.From_Cache ((" &
                          Image (Base_Key (T)) & ", ");
               if Ada_Param (PK) /= "Long_Long_Integer" then
                  TIO.Put (Fbody, "Long_Long_Integer (" &
                             Capitalize (PK.Name) & ")");
               else
                  TIO.Put (Fbody, Capitalize (PK.Name));
               end if;
               TIO.Put (Fbody, "), No_Detached_" & RName & "));");
               TIO.Put_Line (Fbody, "end From_Cache;");
            end if;
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, False);
      end Emit_Body_From_Cache;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  No for views
      --  Special code for tables with one PK integer
      --  Emitted even it the table has no PKs (different from original)
      ----------------------------------------------------------------------
      procedure Emit_Body_Get (Schema : DB_Schema) is
         Num      : Integer;
         T0       : Table_Description;
         procedure Emit1_PKs (PK : in out Field);
         procedure Emit1_PKs (PK : in out Field) is
         begin
            TIO.Put (Fbody, "      " & Capitalize (PK.Name));
            TIO.Put_Line (Fbody, " : " & Ada_Param (PK) & ";");
         end Emit1_PKs;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName  : constant String  := Capitalize (T.Name);
            RName  : constant String  := Capitalize (T.Row_Name);
            PK     : Field;

            procedure Emit_Body (D : String);
            procedure Emit_Body (D : String) is
               procedure Emit2_PKs (PK : in out Field);
               procedure Emit2_PKs (PK : in out Field) is
                  PKName : constant String := Capitalize (PK.Name);
               begin
                  Num := Num + 1;
                  TIO.Put (Fbody, D & "            " &
                             PKName & " => " & PKName);
                  if Num < Num_PKs (T0) then
                     TIO.Put_Line (Fbody, ",");
                  end if;
               end Emit2_PKs;
            begin
               TIO.New_Line (Fbody);
               TIO.Put_Line (Fbody, D & "      declare");
               TIO.Put_Line (Fbody, D & "         M : " &
                               TName &
                               "_Managers := Filter");
               TIO.Put (Fbody, D & "           (All_" & TName);
               if Has_PKs (T) then
                  TIO.Put (Fbody, ",");
                  For_Each_PK (T, Emit2_PKs'Access, True);
               end if;
               TIO.Put_Line (Fbody, ");");
               TIO.Put_Line (Fbody, D & "         L : I_" &
                               TName & ".List;");
               TIO.Put_Line (Fbody, D & "      begin");
               TIO.Put_Line (Fbody, D & "         M.Select_Related");
               TIO.Put_Line (Fbody, D & "           (Depth, Follow_" &
                               "Left_Join " &
                               "=> Follow_Left_Join);");
               TIO.Put_Line (Fbody, D & "         M.Limit (1);");
               TIO.Put_Line (Fbody, D & "         L := M.Get (Session);");
               TIO.Put_Line (Fbody, D & "         if not L.Has_Row then");
               TIO.Put_Line (Fbody, D & "            return No_Detached_" &
                               RName & ";");
               TIO.Put_Line (Fbody, D & "         else");
               TIO.New_Line (Fbody);
               TIO.Put_Line (Fbody, D & "            declare");
               TIO.Put_Line (Fbody, D & "               E : constant " &
                               RName & " := L.Element;");
               TIO.Put_Line (Fbody, D & "            begin");
               TIO.Put_Line (Fbody, D & "               --  Workaround bug " &
                               "in gnat which is missing a call");
               TIO.Put_Line (Fbody, D & "               --  to Finalize if " &
                               "we do not reset the list (K321-012)");
               TIO.Put_Line (Fbody, D & "               L := I_" &
                               TName & ".Empty_List;");
               TIO.Put_Line (Fbody, D & "               return E.Detach_No_" &
                               "Lookup (Session);");
               TIO.Put_Line (Fbody, D & "            end;");
               TIO.Put_Line (Fbody, D & "         end if;");
               TIO.Put_Line (Fbody, D & "      end;");
            end Emit_Body;
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if Has_PKs (T) then
               Emit_Section_Title (Fbody, "Get_" & RName, 1, 1);
               Num := 0;
               T0 := T;
               TIO.Put_Line (Fbody, "function Get_" & RName);
               TIO.Put_Line (Fbody, "  (Session" &
                               " : Session_Type;");
               For_Each_PK (T, Emit1_PKs'Access, True);
               TIO.Put_Line (Fbody, "Depth" &
                               " : " &
                               "Related_Depth := 0;");
               TIO.Put_Line (Fbody, "Follow_Left_Join" &
                               " : Boolean " &
                               ":= False)");
               TIO.Put (Fbody, "     return Detached_" &
                          RName & "'Class");

               if Has_Cache (T) then
                  PK := Get_First_PK (T);
                  TIO.New_Line (Fbody);
                  TIO.Put_Line (Fbody, "is");
                  TIO.Put_Line (Fbody, "R : constant Detached_" &
                                  RName &
                                  "'Class := From_Cache (Session, " &
                                  Capitalize (PK.Name) & ");");
                  TIO.Put_Line (Fbody, "begin");
                  TIO.Put_Line (Fbody, "if not R.Is_Null then");
                  TIO.Put_Line (Fbody, "return R;");
                  TIO.Put_Line (Fbody, "else");
                  Emit_Body ("   ");
                  TIO.Put_Line (Fbody, "end if;");
               else
                  TIO.Put_Line (Fbody, " is");
                  TIO.Put_Line (Fbody, "begin");
                  Emit_Body ("");
               end if;
               TIO.Put_Line (Fbody, "end Get_" &
                               RName & ";");
            end if;
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Get;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  Differences from the original:
      --    Multi-keys tables are processed
      --    Mask is also tested for primary keys as they can be modified
      --      in section emit_body_set
      --    Field Mask is enumerated from Field_Order, not by F.Id
      ----------------------------------------------------------------------
      procedure Emit_Body_Insert_Or_Update (Schema : DB_Schema) is
         Num : Integer;
         T0  : Table_Description;
         procedure Process_Missing_PK (F : in out Field);
         procedure Process_Missing_PK (F : in out Field) is
            FName : constant String := Capitalize (F.Name);
         begin
            Num := Num + 1;
            TIO.Put (Fbody, "D.ORM_" &
                       FName & " = ");
            if F.Default = "" then
               TIO.Put (Fbody, Ada_Default_Record (F));
            else
               TIO.Put (Fbody, F.Default);
            end if;
            if Num < Num_PKs (T0) then
               TIO.Put_Line (Fbody, " or else");
               TIO.Put (Fbody, (1 .. 39 => ' '));
            else
               TIO.Put_Line (Fbody, ";");
            end if;
         end Process_Missing_PK;
         procedure Process_Field_Mask (F : in out Field);
         procedure Process_Field_Mask (F : in out Field) is
            TName : constant String := Capitalize (T0.Name);
            FName : constant String := Capitalize (F.Name);
            TPointed : constant Table_Description := Pointed_Table (F);
            FPointed : constant Field := Pointed_Field (F);
         begin
            TIO.Put_Line (Fbody, "if Mask (" &
                            Image (Field_Order (T0, F), 1) &
                            ") then");
            if Is_FK (F) then
               --  pointed table and pointed field are then OK
               declare
                  TPName : constant String := Capitalize (TPointed.Row_Name);
                  FPName : constant String := Capitalize (FPointed.Name);
               begin
                  TIO.Put_Line (Fbody, "if D.ORM_" &
                                  FName & " /= " &
                                  Ada_Default_Record (F) & " then");
                  TIO.Put (Fbody, "            A := A & (DBA." &
                             TName & "." & FName &
                             " = ");
                  if Ada_Param (F) = "String" then
                     TIO.Put (Fbody,  "To_String (");
                     TIO.Put_Line (Fbody, "D.ORM_" & FName & "));");
                  else
                     TIO.Put_Line (Fbody, "D.ORM_" & FName & ");");
                  end if;
                  TIO.Put_Line (Fbody, "else");
                  TIO.New_Line (Fbody);
                  TIO.Put_Line (Fbody, "   declare");
                  TIO.Put_Line (Fbody, "      D2 : constant " &
                                  TPName & "_Data :=");
                  TIO.Put_Line (Fbody, "      " &
                                  TPName & "_data (D.ORM_FK_" &
                                  FName & ".Unchecked_Get);");
                  TIO.Put_Line (Fbody, "   begin");
                  TIO.Put_Line (Fbody, "      if D2.ORM_" &
                                  FPName & " = " &
                                  Ada_Default_Record (F) & " then");
                  TIO.Put_Line (Fbody, "         Self.Session." &
                                  "Insert_Or_Update");
                  TIO.Put_Line (Fbody, "           (D.ORM_FK_" &
                                  FName & ".all);");
                  TIO.Put_Line (Fbody, "      end if;");
                  TIO.Put (Fbody, "               A := A & (DBA." &
                             TName & "." & FName & " = ");
                  if Ada_Param (F) = "String" then
                     TIO.Put (Fbody,  "To_String (");
                     TIO.Put_Line (Fbody, "D2.ORM_" & FPName & "));");
                  else
                     TIO.Put_Line (Fbody, "D2.ORM_" & FPName & ");");
                  end if;
                  TIO.Put_Line (Fbody, "   end;");
                  TIO.Put_Line (Fbody, "end if;");
               end;
            else
               TIO.Put (Fbody, "         A := A & (DBA." &
                          TName & "." &
                          FName & " = ");
               if Ada_Param (F) = "String" then
                  TIO.Put (Fbody,  "To_String (");
                  TIO.Put_Line (Fbody, "D.ORM_" & FName & "));");
               else
                  TIO.Put_Line (Fbody, "D.ORM_" & FName & ");");
               end if;
            end if;
            if F.Is_PK then
               TIO.Put_Line (Fbody,  "Insert := True;");
            end if;
            TIO.Put_Line (Fbody, "end if;");
         end Process_Field_Mask;
         procedure Process_SQL_PK (PK : in out Field);
         procedure Process_SQL_PK (PK : in out Field) is
            TName  : constant String := Capitalize (T0.Name);
            PKName : constant String := Capitalize (PK.Name);
         begin
            Num := Num + 1;
            TIO.Put (Fbody, (1 .. 17 => ' ') & "DBA." &
                       TName & "." &
                       PKName & " = ");
            if Ada_Param (PK) = "String" then
               TIO.Put (Fbody,  "To_String (");
               TIO.Put (Fbody, "D.ORM_" & PKName & ")");
            else
               TIO.Put (Fbody, "D.ORM_" & PKName);
            end if;
            if Num < Num_PKs (T0) then
               TIO.Put_Line (Fbody, " and");
            end if;
         end Process_SQL_PK;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName  : constant String := Capitalize (T.Name);
            RName  : constant String := Capitalize (T.Row_Name);
            PK     : Field;
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if T.Get_Kind = Kind_View and then not Internal_Updatable_Views
            then
               return;
            end if;
            T0 := T;
            Emit_Section_Title (Fbody, "Insert_Or_Update", 1, 1);
            TIO.Put_Line (Fbody, "overriding procedure Insert_Or_Update");
            TIO.Put_Line (Fbody, "(Self : in out Detached_" &
                            RName & ";");
            TIO.Put_Line (Fbody, "Pk_Modified : in out Boolean;");
            TIO.Put_Line (Fbody, "Mask        : Dirty_Mask)");
            TIO.Put_Line (Fbody, "is");
            if not Has_PKs (T) then
               TIO.Put_Line (Fbody, "pragma Unreferenced " &
                               "(Pk_Modified);");
               TIO.Put_Line (Fbody, "D : constant " &
                               RName & "_Data := " &
                               RName & "_Data (Self.Unchecked_Get);");
               TIO.Put_Line (Fbody, "Q : SQL_Query;");
               TIO.Put_Line (Fbody, "A : Sql_Assignment := " &
                               "No_Assignment;");
            else
               TIO.Put_Line (Fbody, "D          : constant " &
                               RName & "_Data := " &
                               RName & "_Data (Self.Unchecked_Get);");
               TIO.Put_Line (Fbody, "Q          : SQL_Query;");
               TIO.Put_Line (Fbody, "A          : Sql_Assignment := " &
                               "No_Assignment;");
               TIO.Put (Fbody, "      Missing_Pk : constant Boolean := ");
               Num := 0;
               For_Each_PK (T, Process_Missing_PK'Access, True);
               TIO.Put_Line (Fbody, "Insert : Boolean := False;");
            end if;
            if Can_R_Fetch (T) then
               TIO.Put_Line (Fbody, "R : Forward_Cursor;");
            end if;

            TIO.Put_Line (Fbody, "begin");
            For_Each_Field (T, Process_Field_Mask'Access, True);

            if Has_PKs (T) then
               TIO.Put_Line (Fbody, "if Missing_PK or Insert then");
               TIO.Put_Line (Fbody, "Q := SQL_Insert (A);");
               TIO.Put_Line (Fbody, "else");
               TIO.Put_Line (Fbody, "Q := SQL_Update (DBA." &
                               TName & ", A,");
               Num := 0;
               For_Each_PK (T, Process_SQL_PK'Access, True);
               TIO.Put_Line (Fbody, ");");
               TIO.Put_Line (Fbody, "end if;");
               if Can_R_Fetch (T) then
                  TIO.Put_Line (Fbody, "R.Fetch (Self.Session.DB, Q);");
               else
                  TIO.Put_Line (Fbody, "Execute (Self.Session.DB, Q);");
               end if;
               TIO.New_Line (Fbody);
               TIO.Put_Line (Fbody, "if (Missing_PK or Insert) " &
                               "and then Success " &
                               "(Self.Session.DB) then");
               TIO.Put_Line (Fbody, "PK_Modified := True;");
               if Can_R_Fetch (T) then
                  PK := Get_First_PK (T);
                  if PK /= No_Field then
                     declare
                        PKName : constant String := Capitalize (PK.Name);
                     begin
                        TIO.Put_Line (Fbody, "D.ORM_" &
                                        PKName &
                                        " := R.Last_Id " &
                                        "(Self.Session.DB, DBA." &
                                        TName & "." &
                                        PKName &
                                        ");");
                     end;
                  end if;
               else
                  TIO.Put_Line (Fbody, "null;");
                  if Num_PKs (T) > 1 then
                     TIO.Put_Line (Fbody, "         --  Can't retrieve " &
                                     "multi-key PK");
                  else
                     TIO.Put_Line (Fbody, "         --  Can only retrieve " &
                                     "integer autoincrement/serial PK");
                  end if;
               end if;
               TIO.Put_Line (Fbody, "end if;");
            else
               TIO.Put_Line (Fbody, "Q := SQL_Insert (A);");
               TIO.Put_Line (Fbody, "R.Fetch (Self.Session.DB, Q);");
            end if;
            TIO.Put_Line (Fbody, "end Insert_Or_Update;");
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Insert_Or_Update;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  For views or tables with no PKs emit a raise program error
      ----------------------------------------------------------------------
      procedure Emit_Body_Internal_Delete (Schema : DB_Schema) is
         Num : Integer;
         T0  : Table_Description;
         procedure Process_PK (PK : in out Field);
         procedure Process_PK (PK : in out Field) is
            TName  : constant String := Capitalize (T0.Name);
            PKName : constant String := Capitalize (PK.Name);
         begin
            Num := Num + 1;
            TIO.Put_Line (Fbody, "DBA." &
                          TName & "." &  PKName & " = ");
            if Ada_Param (PK) = "String" then
               TIO.Put (Fbody,  "To_String (");
               TIO.Put (Fbody, "D.ORM_" & PKName & ")");
            else
               TIO.Put (Fbody, "D.ORM_" & PKName);
            end if;
            if Num < Num_PKs (T0) then
               TIO.Put_Line (Fbody, " and");
            end if;
         end Process_PK;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName  : constant String := Capitalize (T.Name);
            RName  : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            T0 := T;
            Emit_Section_Title (Fbody, "Internal_Delete", 1, 1);
            TIO.Put_Line (Fbody, "overriding procedure Internal_Delete " &
                            "(Self : Detached_" &
                            RName & ") is");
            if not Has_PKs (T) then
               TIO.Put_Line (Fbody, "pragma Unreferenced (Self);");
               TIO.Put_Line (Fbody, "begin");
               TIO.Put_Line (Fbody, "raise Program_Error with """ &
                               "Table " &
                               TName &
                               " has no primary key"";");
            else
               TIO.Put_Line (Fbody, "D : constant " &
                               RName & "_Data := " &
                               RName & "_Data (Self.Unchecked_Get);");
               TIO.Put_Line (Fbody, "begin");
               TIO.Put_Line (Fbody, "Execute");
               TIO.Put_Line (Fbody, "(Self.Session.DB,");
               TIO.Put_Line (Fbody, "SQL_Delete");
               TIO.Put_Line (Fbody, "(DBA." & TName & ", ");
               Num := 0;
               For_Each_PK (T, Process_PK'Access, True);
               TIO.Put_Line (Fbody, "));");
            end if;
            TIO.Put (Fbody, "end Internal_Delete;");
            TIO.New_Line (Fbody);
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Internal_Delete;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  For views or tables with no PKs emit a raise program error
      ----------------------------------------------------------------------
      procedure Emit_Body_Internal_Query (Schema : DB_Schema) is
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            TName : constant String := Capitalize (T.Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            Emit_Section_Title (Fbody, "Internal_Query_" & TName, 1, 1);
            TIO.Put_Line (Fbody, "   procedure Internal_Query_" & TName);
            TIO.Put_Line (Fbody, "     (Fields    : in out " &
                            "SQL_Field_List;");
            TIO.Put_Line (Fbody, "      From      : out SQL_Table_List;");
            TIO.Put_Line (Fbody, "      Criteria  : in out Sql_Criteria;");
            TIO.Put_Line (Fbody, "      Depth     : Natural;");
            TIO.Put_Line (Fbody, "      Follow_LJ : Boolean;");
            TIO.Put_Line (Fbody, "      Pk_Only   : Boolean := False)");
            TIO.Put_Line (Fbody, "is");
            TIO.Put_Line (Fbody, "begin");
            if not Has_PKs (T) then
               TIO.Put_Line (Fbody, "if PK_Only then");
               TIO.Put (Fbody, "raise Program_Error with """);
               TIO.Put_Line (Fbody, "Table " &
                               TName & " has no primary key"";");
               TIO.Put_Line (Fbody, "end  if;");
            end if;
            TIO.Put_Line (Fbody, "Do_Query_" & TName);
            TIO.Put (Fbody, "(Fields, From, Criteria, 0, Alias_" &
                       TName & ", Depth, Follow_LJ");
            if not Has_PKs (T) then
               TIO.Put_Line (Fbody, ");");
            else
               TIO.Put_Line (Fbody, ", PK_Only);");
            end if;
            TIO.Put_Line (Fbody, "end Internal_Query_" & TName & ";");
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Internal_Query;

      ----------------------------------------------------------------------
      --  No abstract tables
      ----------------------------------------------------------------------
      procedure Emit_Body_Key (Schema : DB_Schema) is
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
            PK    : Field;
         begin
            if Is_Abstract (T) then
               return;
            end if;
            Emit_Section_Title (Fbody, "Key", 1, 1);
            TIO.Put_Line (Fbody, "overriding function Key (Self : " &
                            RName &
                            "_Ddr) return Element_Key is");
            if Has_Cache (T) then
               PK := Get_First_PK (T);
               TIO.Put_Line (Fbody, "begin");
               TIO.Put_Line (Fbody, "if Self.ORM_" &
                               Capitalize (PK.Name) &
                               " = " & Ada_Default_Record (PK) &
                               " then");
               TIO.Put_Line (Fbody, "return (" &
                               Image (Base_Key (T)) &
                               ", No_Primary_Key);");
               TIO.Put_Line (Fbody, "else");
               TIO.Put (Fbody, "return (" &
                               Image (Base_Key (T)) & ", ");
               if Ada_Param (PK) /= "Long_Long_Integer" then
                  TIO.Put (Fbody, "Long_Long_Integer (" &
                             "Self.ORM_" & Capitalize (PK.Name) & ")");
               else
                  TIO.Put (Fbody, "Self.ORM_" & Capitalize (PK.Name));
               end if;
               TIO.Put_Line (Fbody, ");");
               TIO.Put_Line (Fbody, "end if;");
            else
               TIO.Put_Line (Fbody, "pragma Unreferenced (Self);");
               TIO.Put_Line (Fbody, "begin");
               TIO.Put_Line (Fbody, "      --  Not cachable, since the PK " &
                               "is not a single integer field");
               TIO.Put_Line (Fbody, "return (" &
                               Image (Base_Key (T)) &
                               ", No_Primary_Key);");
            end if;
            TIO.Put_Line (Fbody, "end Key;");
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_Key;

      ----------------------------------------------------------------------
      --  No abstract tables
      ----------------------------------------------------------------------
      procedure Emit_Body_New (Schema : DB_Schema) is
         procedure Emit_New_With_Parameters (T      : Table_Description;
                                             RName  : String);
         procedure Emit_New_With_Parameters (T      : Table_Description;
                                             RName  : String) is
            First_PK : Boolean := True;
            procedure Process_PK_Parameter (PK : in out Field);
            procedure Process_PK_Parameter (PK : in out Field) is
            begin
               if PK.Is_Autoincrement then
                  return;
               end if;
               if First_PK then
                  First_PK := False;
                  TIO.Put (Fbody, Capitalize (PK.Name) & " : ");
                  TIO.Put (Fbody, Ada_Param (PK));
               else
                  TIO.Put_Line (Fbody, ";");
                  TIO.Put (Fbody, Capitalize (PK.Name) & " : " &
                             Ada_Param (PK));
               end if;
            end Process_PK_Parameter;
            procedure Process_PK_Assignment (PK : in out Field);
            procedure Process_PK_Assignment (PK : in out Field) is
            begin
               if PK.Is_Autoincrement then
                  return;
               end if;
               TIO.Put_Line (Fbody, "Set_" &
                               Capitalize (PK.Name) & " (Result, " &
                               Capitalize (PK.Name) & ");");
            end Process_PK_Assignment;
         begin
            if not Has_PKs (T) then
               return;
            end if;
            Emit_Section_Title (Fbody, "New_" & RName, 1, 1);
            TIO.Put (Fbody, "   function New_" & RName & " (");
            For_Each_PK (T, Process_PK_Parameter'Access, True);
            TIO.Put_Line (Fbody, ")");
            TIO.Put_Line (Fbody, "            " &
                            "return Detached_" &
                            RName & "'Class is");
            TIO.Put_Line (Fbody, "Result : Detached_" &
                            RName & ";");
            TIO.Put_Line (Fbody, "Data   : " &
                            RName & "_Ddr;");
            TIO.Put_Line (Fbody, "begin");
            TIO.Put_Line (Fbody, "Result.Set (Data);");
            For_Each_PK (T, Process_PK_Assignment'Access, True);
            TIO.Put_Line (Fbody, "return Result;");
            TIO.Put_Line (Fbody, "end New_" &
                            RName & ";");
         end Emit_New_With_Parameters;
         procedure Process_Table (T : in out Table_Description);
         procedure Process_Table (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            --  emit the function new
            Emit_Section_Title (Fbody, "New_" & RName, 1, 1);
            TIO.Put_Line (Fbody, "   function New_" & RName &
                            " return Detached_" & RName & "'Class is");
            TIO.Put_Line (Fbody, "Result : Detached_" &
                            RName & ";");
            TIO.Put_Line (Fbody, "Data   : " &
                            RName & "_Ddr;");
            TIO.Put_Line (Fbody, "begin");
            TIO.Put_Line (Fbody, "Result.Set (Data);");
            TIO.Put_Line (Fbody, "return Result;");
            TIO.Put_Line (Fbody, "end New_" &
                            RName & ";");

            --  now emit the function new with parameters
            if not Has_PKs (T) then
               null;
            elsif (not All_PKs_Autoincrement (T)) then
               Emit_New_With_Parameters (T, RName);
            end if;
         end Process_Table;
      begin
         For_Each_Table (Schema, Process_Table'Access, True);
      end Emit_Body_New;

      ----------------------------------------------------------------------
      --  No abstract tables
      ----------------------------------------------------------------------
      procedure Emit_Body_On_Persit (Schema : DB_Schema) is
         procedure Process_MFK (Pted_Table   : in out Table_Description;
                                All_Not_Null : in out Boolean);
         procedure Process_MFK (Pted_Table   : in out Table_Description;
                                All_Not_Null : in out Boolean) is
            pragma Unreferenced (All_Not_Null);
            PTName  : constant String := Capitalize (Pted_Table.Name);
         begin
            TIO.Put_Line (Fbody, "if D.ORM_FK_" &
                            PTName & " /= null then");
            TIO.Put_Line (Fbody, "Self.Session.Persist (D.ORM_FK_" &
                            PTName & ".all);");
            TIO.Put_Line (Fbody, "end if;");
         end Process_MFK;
         procedure Process_FK (FK : in out Field);
         procedure Process_FK (FK : in out Field) is
            FName : constant String := Capitalize (FK.Name);
         begin
            TIO.Put_Line (Fbody, "if D.ORM_FK_" &
                            FName & " /= null then");
            TIO.Put_Line (Fbody, "Self.Session.Persist (D.ORM_FK_" &
                            FName & ".all);");
            TIO.Put_Line (Fbody, "end if;");
         end Process_FK;
         procedure Process_On_Persist (T : in out Table_Description);
         procedure Process_On_Persist (T : in out Table_Description) is
            RName : constant String := Capitalize (T.Row_Name);
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if T.Get_Kind = Kind_View and then not Internal_Updatable_Views
            then
               return;
            end if;
            --  only for tables with FK
            if Has_FKs (T) then
               Emit_Section_Title (Fbody, "On_Persist", 1, 1);
               TIO.Put_Line (Fbody, "   overriding procedure On_Persist " &
                               "(Self : Detached_" &
                               RName & ") is");
               TIO.Put_Line (Fbody, "D : constant " &
                               RName &
                               "_Data := " &
                               RName &
                               "_Data (Self.Unchecked_Get);");
               TIO.Put_Line (Fbody, "begin");
               TIO.Put_Line (Fbody, "if Persist_Cascade (Self.Session) then");
               For_Each_FK (T, Process_FK'Access);
               For_Each_MFK (T, Process_MFK'Access);
               TIO.Put_Line (Fbody, "end if;");
               TIO.Put_Line (Fbody, "end On_Persist;");
            end if;
         end Process_On_Persist;
      begin
         For_Each_Table (Schema, Process_On_Persist'Access, True);
      end Emit_Body_On_Persit;

      ----------------------------------------------------------------------
      --  No abstract tables
      --  Second function only for FKs
      ----------------------------------------------------------------------
      procedure Emit_Body_Set (Schema : DB_Schema) is
         T0  : Table_Description;

         --  used by process_field and by process_pk
         procedure Emit_Field (Qual  : String;
                               FName : String;
                               RName : String;
                               RType : String;
                               T     : Table_Description;
                               F     : Field);
         procedure Emit_Field (Qual  : String;
                               FName : String;
                               RName : String;
                               RType : String;
                               T     : Table_Description;
                               F     : Field) is
         begin
            Start_Section (FName & Qual);
            Emit_Section_Title ("Set_" & FName & Qual, 1, 1);
            Add_Line ("   procedure Set_" & FName &
                            " (Self : Detached_" & RName &
                            "; Value : " & RType & ") is");
            Add_Line ("D : constant " &
                            RName &
                            "_Data := " &
                            RName &
                            "_Data (Self.Unchecked_Get);");
            Add_Line ("begin");
            if F.Is_FK then
               Add_Line ("Unchecked_Free (D.ORM_FK_" &
                               FName & ");");
            end if;
            Add ("D.ORM_" &
                       FName & " := ");
            if SQL_Field (F) = "SQL_Field_Text" then
               Add_Line ("To_Unbounded_String (Value);");
            else
               Add_Line ("Value;");
            end if;
            Add_Line ("Self.Set_Modified (" &
                            Image (Field_Order (T, F), 1) & ");");
            Add_Line ("end Set_" & FName & ";");
         end Emit_Field;

         procedure Process_PK (F : in out Field);
         procedure Process_PK (F : in out Field) is
            TName : constant String := Capitalize (T0.Name);
            RName : constant String := Capitalize (T0.Row_Name);
            FName : constant String := Capitalize (F.Name);
            RType : constant String := Ada_Return (F);
         begin
            if Is_Abstract (T0) then
               return;
            end if;
            if F.Is_Autoincrement then
               return;
            end if;
            if F.Is_FK then
               return; --  because it was emitted in (2)
            end if;

            Emit_Field (" of " & TName & " (4)", FName, RName, RType, T0, F);
         end Process_PK;

         procedure Process_Field (T : in out Table_Description;
                                  F : in out Field);
         procedure Process_Field (T : in out Table_Description;
                                  F : in out Field) is
            TName      : constant String := Capitalize (T.Name);
            RName      : constant String := Capitalize (T.Row_Name);
            FName      : constant String := Capitalize (F.Name);
            RType      : constant String := Ada_Return (F);
            Pted_Table : Table_Description;
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if T.Get_Kind = Kind_View and then not Internal_Updatable_Views
            then
               return;
            end if;
            if F.Is_Autoincrement then
               return;
            end if;
            if Is_PK (F) and then not Is_FK (F) then
               return;
            end if;

            Emit_Field (" of " & TName & " (1)", FName, RName, RType, T, F);

            if Is_FK (F) then
               Pted_Table := Pointed_Table (F);
               if Pted_Table /= No_Table then
                  declare
                     PTRName : constant String :=
                       Capitalize (Pted_Table.Row_Name);
                     PF     : constant Field := Pointed_Field (F);
                     PFName : constant String := Capitalize (PF.Name);
                  begin
                     Start_Section (FName & " of " & TName & " (2)");
                     Emit_Section_Title ("Set_" &
                                           FName & " of " & TName & " (2)",
                                         1, 1);
                     Add_Line ("procedure Set_" & FName);
                     Add_Line (" (Self  : Detached_" & RName & ";");
                     Add_Line (" Value : Detached_" &
                                 PTRName & "'Class) is");
                     Add_Line ("D : constant " &
                                 RName &
                                 "_Data := " &
                                 RName &
                                 "_Data (Self.Unchecked_Get);");
                     Add_Line ("begin");
                     Add_Line ("Unchecked_Free (D.ORM_FK_" &
                                 FName & ");");
                     Add ("D.ORM_" & FName & " := ");
                     if SQL_Field (F) = "SQL_Field_Text" then
                        Add_Line ("To_Unbounded_String (" &
                                    "Value." & PFName & ");");
                     else
                        Add_Line ("Value." & PFName & ";");
                     end if;
                     Add_Line ("D.ORM_FK_" &
                                     FName &
                                     " := new Detached_" &
                                     PTRName & "'Class'(Value);");
                     Add_Line ("");
                     Add_Line ("Self.Set_Modified (" &
                                     Image (Field_Order (T, F), 1) & ");");
                     Add_Line ("if Persist_Cascade " &
                                     "(Self.Session) then");
                     Add_Line ("Self.Session.Persist " &
                                     "(D.ORM_FK_" &
                                     FName & ".all);");
                     Add_Line ("end if;");
                     Add_Line ("end Set_" &
                                     FName & ";");
                  end;
               else
                  raise Program_Error;
               end if;
            end if;
         end Process_Field;
         procedure Emit_MFK_Set (Pted_Table   : in out Table_Description;
                                 All_Not_Null : in out Boolean);
         procedure Emit_MFK_Set (Pted_Table   : in out Table_Description;
                                 All_Not_Null : in out Boolean) is
            pragma Unreferenced (All_Not_Null);
            PTName  : constant String := Capitalize (Pted_Table.Name);
            PTRName : constant String := Capitalize (Pted_Table.Row_Name);
            RName   : constant String := Capitalize (T0.Row_Name);

            procedure Process_FK_Set (FK : in out Field);
            procedure Process_FK_Set (FK : in out Field) is
               Pted_Field : constant Field := Pointed_Field (FK);
            begin
               if Table_Description (Pted_Field.Get_Table) /= Pted_Table then
                  return;
               end if;
               Add_Line ("Self.Set_Modified (" &
                           Image (FK.Id, 1) & ");");
            end Process_FK_Set;
            procedure Process_FK (FK : in out Field);
            procedure Process_FK (FK : in out Field) is
               Pted_Field : constant Field := Pointed_Field (FK);
               FName : constant String := Capitalize (FK.Name);
               PName : constant String := Capitalize (Pted_Field.Name);
            begin
               if Table_Description (Pted_Field.Get_Table) /= Pted_Table then
                  return;
               end if;
               Add ("D.ORM_" &
                          FName &
                          " := ");
               if Ada_Param (FK) = "String" then
                  Add_Line ("To_Unbounded_String (Value." &
                                  PName & ")");
               else
                  Add_Line ("Value." & PName);
               end if;
               Add_Line (";");
            end Process_FK;
         begin
            Start_Section ("Set_" & PTName & " (3)");
            Emit_Section_Title ("Set_" & PTName, 1, 1);
            Add_Line ("procedure Set_" & PTName &
                        " (Self : Detached_" & RName &
                        "; Value : Detached_" & PTRName &
                        "'Class) is");
            Add_Line ("D : constant " &
                            RName &
                            "_Data := " &
                            RName &
                            "_Data (Self.Unchecked_Get);");
            Add_Line ("begin");
            Add_Line ("Unchecked_Free (D.ORM_FK_" &
                               PTName & ");");
            For_Each_FK (T0, Process_FK'Access);
            Add_Line ("D.ORM_FK_" &
                            PTName & " := new Detached_" &
                            PTRName & "'Class'(Value);");
            Add_Line ("");
            For_Each_FK (T0, Process_FK_Set'Access);
            Add_Line ("if Persist_Cascade (Self.Session) then");
            Add_Line ("Self.Session.Persist (D.ORM_FK_" &
                            PTName & ".all);");
            Add_Line ("end if;");
            Add_Line ("end Set_" & PTName & ";");
         end Emit_MFK_Set;
         procedure Emit_Table_MFK (T : in out Table_Description);
         procedure Emit_Table_MFK (T : in out Table_Description) is
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if T.Get_Kind = Kind_View and then not Internal_Updatable_Views
            then
               return;
            end if;
            T0 := T;
            For_Each_MFK (T, Emit_MFK_Set'Access);
         end Emit_Table_MFK;
         procedure Emit_Table_PK (T : in out Table_Description);
         procedure Emit_Table_PK (T : in out Table_Description) is
         begin
            if Is_Abstract (T) then
               return;
            end if;
            if T.Get_Kind = Kind_View and then not Internal_Updatable_Views
            then
               return;
            end if;
            T0 := T;
            For_Each_PK (T, Process_PK'Access, True);
            Print_Sections (Fbody);
         end Emit_Table_PK;
      begin
         For_Each_Ordered_Field (Process_Field'Access);
         For_Each_Table (Schema, Emit_Table_MFK'Access, True);
         Print_Sections (Fbody);
         For_Each_Table (Schema, Emit_Table_PK'Access, True);
      end Emit_Body_Set;

      ----------------------------------------------------------------------
      procedure Emit_Body_End (Orm : String) is
      begin
         TIO.Put_Line (Fbody, "end " & Capitalize (Orm) & "_New;");
      end Emit_Body_End;
   end Emit_Body;

   --------------------------------------------------------------------
   --  Main procedure
   --------------------------------------------------------------------
   procedure Main (Api             : String;
                   Orm             : String;
                   Output_Dir      : String;
                   Schema          : DB_Schema;
                   Updatable_Views : Boolean := False) is
   begin
      --  register schema information
      Register_Ada_Types;                  --  known Ada Types
      Register_Tables_And_Fields (Schema); --  field ordered in each table
      Register_FKs (Schema);               --  FKs per table
      Register_Fields (Schema);            --  fields alphabetically ordered
      Register_Tables_And_MFKs (Schema);   --  tables and multiple FKDs

      Internal_Updatable_Views := Updatable_Views;
      Emit_Spec.Perform (Api, Orm, Output_Dir, Schema);
      Emit_Body.Perform (Api, Orm, Output_Dir, Schema);

   end Main;
end GNATCOLL.DB2Ada.Generate_Ada;
