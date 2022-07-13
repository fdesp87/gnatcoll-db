with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Calendar;          use Ada.Calendar;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with GNATCOLL.Utils;        use GNATCOLL.Utils;

procedure Parse_Postgresql_Interval is
   function Main (Str : String) return Duration is
      Incorrect_Interval_String : exception;
      YY_Idx, MM_Idx, DD_Idx : Natural;
      HH_Idx, MI_Idx, SS_Idx : Natural;
      T_Idx : Natural;
      YY, MM, DD, HH, MI : Integer;
      SS : Ada.Calendar.Day_Duration;
      Result : Duration;

      procedure Parse_ISO8601_Basic;
      procedure Parse_ISO8601_Basic is
      begin
         if Str (Str'First + 2) = 'T' then
            YY := 0;
            MM := 0;
            DD := 0;
            T_Idx := Str'First + 2;
         else
            YY := Integer'Value (Str (Str'First + 1 .. Str'First + 4));
            MM := Integer'Value (Str (Str'First + 5 .. Str'First + 6));
            DD := Integer'Value (Str (Str'First + 7 .. Str'First + 8));
            T_Idx := Str'First + 9;
         end if;
         if Str (T_Idx) = 'T' then
            HH := Integer'Value (Str (T_Idx + 1 .. T_Idx + 2));
            MI := Integer'Value (Str (T_Idx + 3 .. T_Idx + 4));
            SS := Duration'Value (Str (T_Idx + 5 .. Str'Last));
         else
            raise Incorrect_Interval_String;
         end if;
      end Parse_ISO8601_Basic;

      procedure Parse_ISO8601;
      procedure Parse_ISO8601 is
         CSet : Character_Set := To_Set ("YMDHS");
      begin
         if not (Index (Str, Cset) in Str'Range) then
            Parse_ISO8601_Basic;
            return;
         end if;
         T_Idx := Index (Str, "T");
         if T_Idx in Str'Range then
            YY_Idx := Index (Str, "Y");
            if YY_Idx in Str'First + 1 .. Str'Last then
               YY := Integer'Value (Str (Str'First + 1 .. YY_Idx - 1));
            else
               YY_Idx := Str'First;
               YY := 0;
            end if;
            MM_Idx := Index (Str (YY_Idx + 1 .. T_Idx), "M");
            if MM_Idx in YY_Idx + 1 .. T_Idx - 1 then
               MM := Integer'Value (Str (YY_Idx + 1 .. MM_Idx - 1));
            else
               MM_Idx := YY_Idx;
               MM := 0;
            end if;
            DD_Idx := Index (Str (MM_Idx + 1 .. Str'Last), "D");
            if DD_Idx in MM_Idx + 1 .. Str'Last then
               DD := Integer'Value (Str (MM_Idx + 1 .. DD_Idx - 1));
            else
               DD_Idx := MM_Idx;
               DD := 0;
            end if;
         else
            HH := 0;
            MI := 0;
            SS := 0.0;
            return;
         end if;
         if T_Idx in DD_Idx + 1 .. Str'Last - 2 then
            HH_Idx := Index (Str (T_Idx .. Str'Last), "H");
            if HH_Idx in T_Idx + 1 .. Str'Last then
               HH := Integer'Value (Str (T_Idx + 1 .. HH_Idx - 1));
            else
               HH_Idx := T_Idx;
               HH := 0;
            end if;
            MI_Idx := Index (Str (HH_Idx .. Str'Last), "M");
            if MI_Idx in HH_Idx + 1 .. Str'Last then
               MI := Integer'Value (Str (HH_Idx + 1 .. MI_Idx - 1));
            else
               MI_Idx := T_Idx;
               MI := 0;
            end if;
            SS_Idx := Index (Str (MI_Idx .. Str'Last), "S");
            if SS_Idx in MI_Idx + 1 .. Str'Last then
               SS := Duration'Value (Str (MI_Idx + 1 .. SS_Idx - 1));
            else
               SS_Idx := T_Idx;
               SS := 0.0;
            end if;
         else
            HH := 0;
            MI := 0;
            SS := 0.0;
         end if;
      end Parse_ISO8601;

      procedure Parse_Time (Start : Integer);
      procedure Parse_Time (Start : Integer) is
      begin
         HH_Idx := Index (Str (Start + 1 .. Str'Last), ":");
         MI_Idx := Index (Str (HH_Idx + 1 .. Str'Last), ":");
         --      SS_Idx := Index (Str (MI_Idx + 1 .. Str'Last), ":");
         HH := Integer'Value (Str (Start + 1 .. HH_Idx - 1));
         MI := Integer'Value (Str (HH_Idx + 1 .. MI_Idx - 1));
         SS := Duration'Value (Str (MI_Idx + 1 .. Str'Last));
      end Parse_Time;

      procedure Parse_ISO8601_Extended;
      procedure Parse_ISO8601_Extended is
      begin
         YY_Idx := Index (Str, "-");
         if YY_Idx in Str'Range then
            MM_Idx := Index (Str (YY_Idx + 1 .. Str'Last), "-");
            T_Idx := Index (Str (MM_Idx + 1 .. Str'Last), "T");
            if not (T_Idx in MM_Idx + 1 .. Str'Last) then
               raise Incorrect_Interval_String;
            end if;

            DD_Idx := T_Idx;
            YY := Integer'Value (Str (Str'First + 1 .. YY_Idx - 1));
            MM := Integer'Value (Str (YY_Idx + 1 .. MM_Idx - 1));
            DD := Integer'Value (Str (MM_Idx + 1 .. DD_Idx - 1));
         else
            MM_Idx := YY_Idx;
            DD_Idx := YY_Idx;
            YY := 0;
            MM := 0;
            DD := 0;
         end if;
         if T_Idx in Str'Range then
            Parse_Time (Start => T_Idx);
         else
            HH := 0;
            MI := 0;
            SS := 0.0;
         end if;
      end Parse_ISO8601_Extended;

      procedure Parse_SQL_Standard;
      procedure Parse_SQL_Standard is
      begin
         YY_Idx := Index (Str (Str'First + 2 .. Str'Last), "-");
         if YY_Idx in Str'First + 2 .. Str'Last then
            YY := Integer'Value (Str (Str'First .. YY_Idx - 1));
            MM_Idx := Index (Str (YY_Idx .. Str'Last), " ");
            MM := Integer'Value (Str (YY_Idx .. MM_Idx - 1));
         else
            YY := 0;
            MM := 0;
            MM_Idx := YY_Idx;
         end if;
         DD_Idx := Index (Str (MM_Idx + 1 .. Str'Last), " ");
         if DD_Idx in MM_Idx + 1 .. Str'Last then
            DD := Integer'Value (Str (MM_Idx .. DD_Idx - 1));
         else
            DD := 0;
            DD_Idx := MM_Idx;
         end if;
         HH_Idx := Index (Str (DD_Idx + 1 .. Str'Last), ":");
         if HH_Idx in DD_Idx + 1 .. Str'Last then
            Parse_Time (Start => DD_Idx);
         else
            HH := 0;
            MI := 0;
            SS := Duration'Value (Str (DD_Idx + 1 .. Str'Last));
         end if;
      end Parse_SQL_Standard;

      procedure Parse_Postgres_Verbose;
      procedure Parse_Postgres_Verbose is
      begin
         YY_Idx := Index (Str, "year");
         if YY_Idx in Str'Range then
            YY := Integer'Value (Str (Str'First + 1 .. YY_Idx - 1));
            if YY_Idx + 3 < Str'Last and then Str (YY_Idx + 4) = 's' then
               YY_Idx := YY_Idx + 5;
            else
               YY_Idx := YY_Idx + 4;
            end if;
         else
            YY := 0;
         end if;
         MM_Idx := Index (Str, "mon");
         if MM_Idx in YY_Idx + 1 .. Str'Last then
            MM := Integer'Value (Str (YY_Idx + 1 .. MM_Idx - 1));
            if MM_Idx + 2 < Str'Last and then Str (MM_Idx + 3) = 's' then
               MM_Idx := MM_Idx + 4;
            else
               MM_Idx := MM_Idx + 3;
            end if;
         else
            MM := 0;
         end if;
         DD_Idx := Index (Str, "day");
         if DD_Idx in MM_Idx + 1 .. Str'Last then
            DD := Integer'Value (Str (MM_Idx + 1 .. DD_Idx - 1));
            if DD_Idx + 2 < Str'Last and then Str (DD_Idx + 3) = 's' then
               DD_Idx := DD_Idx + 4;
            else
               DD_Idx := DD_Idx + 3;
            end if;
         else
            DD := 0;
         end if;
         HH_Idx := Index (Str, "hour");
         if HH_Idx in DD_Idx + 1 .. Str'Last then
            HH := Integer'Value (Str (DD_Idx + 1 .. HH_Idx - 1));
            if HH_Idx + 2 < Str'Last and then Str (HH_Idx + 3) = 's' then
               HH_Idx := HH_Idx + 5;
            else
               HH_Idx := HH_Idx + 4;
            end if;
         else
            HH := 0;
         end if;
         MI_Idx := Index (Str, "min");
         if MI_Idx in HH_Idx + 1 .. Str'Last then
            MI := Integer'Value (Str (HH_Idx + 1 .. MI_Idx - 1));
            if MI_Idx + 2 < Str'Last and then Str (MI_Idx + 3) = 's' then
               MI_Idx := MI_Idx + 4;
            else
               HH_Idx := MI_Idx + 3;
            end if;
         else
            MM := 0;
         end if;
         SS_Idx := Index (Str, "sec");
         if SS_Idx in HH_Idx + 1 .. Str'Last then
            SS := Duration'Value (Str (MI_Idx + 1 .. SS_Idx - 1));
            if SS_Idx + 2 < Str'Last and then Str (SS_Idx + 3) = 's' then
               SS_Idx := SS_Idx + 4;
            else
               SS_Idx := SS_Idx + 3;
            end if;
         else
            MM := 0;
         end if;
         if Index (Str (SS_Idx .. Str'Last), "ago") in Str'Range then
            YY := -YY;
         end if;

      end Parse_Postgres_Verbose;

      procedure Parse_Postgres_Style;
      procedure Parse_Postgres_Style is
         CSet : Character_Set := To_Set ("ymd");
      begin
         if not (Index (Str, Cset) in Str'Range) then
            Parse_SQL_Standard;
            return;
         end if;

         YY_Idx := Index (Str, "year");
         if YY_Idx in Str'Range then
            YY := Integer'Value (Str (Str'First .. YY_Idx - 1));
            if YY_Idx + 3 < Str'Last and then Str (YY_Idx + 4) = 's' then
               YY_Idx := YY_Idx + 5;
            else
               YY_Idx := YY_Idx + 4;
            end if;
         else
            YY := 0;
         end if;
         MM_Idx := Index (Str, "mon");
         if MM_Idx in YY_Idx + 1 .. Str'Last then
            MM := Integer'Value (Str (YY_Idx + 1 .. MM_Idx - 1));
            if MM_Idx + 2 < Str'Last and then Str (MM_Idx + 3) = 's' then
               MM_Idx := MM_Idx + 4;
            else
               MM_Idx := MM_Idx + 3;
            end if;
         else
            MM := 0;
         end if;
         DD_Idx := Index (Str, "day");
         if DD_Idx in MM_Idx + 1 .. Str'Last then
            DD := Integer'Value (Str (MM_Idx + 1 .. DD_Idx - 1));
            if DD_Idx + 2 < Str'Last and then Str (DD_Idx + 3) = 's' then
               DD_Idx := DD_Idx + 4;
            else
               DD_Idx := DD_Idx + 3;
            end if;
         else
            DD := 0;
         end if;
         HH_Idx := Index (Str (DD_Idx .. Str'Last), ":");
         if HH_Idx in DD_Idx .. Str'Last then
            Parse_Time (Start => DD_Idx);
         else
            HH := 0;
            MI := 0;
            SS := 0.0;
         end if;
      end Parse_Postgres_Style;

   begin
      -- parse
      if Str (Str'First) = 'P' then
         if Index (Str, "-") in Str'Range or else Index (Str, ":") in Str'Range
         then
            Parse_ISO8601_Extended;
         else
            Parse_ISO8601; -- may call Parse_ISO8601_Basic
         end if;
      elsif Str (Str'First) = '@' then
         Parse_Postgres_Verbose;
      else
         Parse_Postgres_Style; -- may call Parse_Sql_Standard
      end if;

      -- adjust overflows
      while SS >= 60.0 loop
         SS := SS - 60.0;
         MI := MI + 1;
      end loop;
      while MI >= 60 loop
         MI := MI - 60;
         HH := HH + 1;
      end loop;
      while HH >= 24 loop
         HH := HH - 24;
         DD := DD + 1;
      end loop;
      while DD >= 31 loop
         DD := DD - 31;
         MM := MM + 1;
      end loop;
      while MM >= 12 loop
         MM := MM - 12;
         YY := YY + 1;
      end loop;

      -- adjust underflows
      while SS < 0.0 loop
         MI := MI - 1;
         SS := 60.0 + SS;
      end loop;
      while MI < 0 loop
         HH := HH - 1;
         MI := 60 + MI;
      end loop;
      while HH < 0 loop
         DD := DD - 1;
         HH := 24 + HH;
      end loop;
      while DD < 0 loop
         MM := MM - 1;
         DD := 30 + DD;
      end loop;
      while MM < 0 loop
         YY := YY - 1;
         MM := 12 + MM;
      end loop;

      -- compute Ada Duration
      Result := Duration (365.0) * YY +
                Duration (30.0) * MM +
                Duration (1.0) * DD;            -- total days
      Result := Result * 24.0 +
                Duration (1.0) * HH;            -- total hours
      Result := Result * 60.0 +
                Duration (1.0) * MI;            -- total minutes
      Result := Result * 60.0 + SS;             -- total seconds
      return Result;
   end Main;

   procedure Test (Str : String) is
      Result : Duration;
   begin
      Result := Main (str);
      Put_Line (Str'Image & "*"(60 - Str'Length, ' ') & Result'Image);
   end Test;
begin
   Test ("0 years 0 mons 1 days 01:01:1.5 ");               -- postgres
   Test ("@ 0 years 0 mons 1 days 1 hours 1 mins 1.5 sec"); -- verbose
   Test ("P1DT1H1M1.5S");                                   -- iso8601
   Test ("P00000001T010101.5");                             -- iso8601_basic
   Test ("P0000-00-01T01:01:01.5");                         -- iso8601_extended
   Test ("+0-0 +1 +1:1:1.5");                               -- sql standard
   Test ("120.34");
end Parse_Postgresql_Interval;
