with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;

package GNATCOLL.DB2Ada.Generate_Ada is
   procedure Main (Api             : String;
                   Orm             : String;
                   Output_Dir      : String;
                   Schema          : DB_Schema;
                   Updatable_Views : Boolean := False);
end GNATCOLL.DB2Ada.Generate_Ada;
