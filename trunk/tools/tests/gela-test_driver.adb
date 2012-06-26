------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Characters.Conversions;
with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;
with Ada.Exceptions;

with Gela.Bitten_Coverage;
with Gela.Bitten_Report;
with Gela.Build;
with Gela.Conv;
with Gela.Host;
with Gela.Test_Cases;
with Gela.Test_Iterators.ACATS;
with Gela.Test_Iterators.Append;
with Gela.Test_Iterators.Dir;

--  with League.Application;
with League.Strings;
with GNAT.OS_Lib;

procedure Gela.Test_Driver is
   use type League.Strings.Universal_String;

   Source   : constant League.Strings.Universal_String :=
     Gela.Host.Source_Root & "/tests/asis";
   --  Path to directory containing tests' sources (trunk/tests/)

   Build    : constant League.Strings.Universal_String :=
     Gela.Host.Build_Root;
   --  Directory where build tests (/tmp/build/)

   Output   : constant League.Strings.Universal_String :=
     Build & "/report.xml";
   --  Where to store report file (/tmp/build/report.xml)

   Coverage : constant League.Strings.Universal_String :=
     Build & "/coverage.xml";
   --  Where to store coverage report file (/tmp/build/coverage.xml)

   Test     : Gela.Test_Cases.Test_Case_Access;
   Report   : League.Strings.Universal_String;
   Failed   : Boolean := False;
begin
   Ada.Wide_Wide_Text_IO.Put_Line
     ("Build root = " & Build.To_Wide_Wide_String);

   Ada.Wide_Wide_Text_IO.Put_Line
     ("Source root = " & Gela.Host.Source_Root.To_Wide_Wide_String);

   --  ACATS test (ts_00018) require this env to correctly distinguish
   --  predefined compilation units.
   GNAT.OS_Lib.Setenv ("GELA_INCLUDE_PATH", "../../../source/asis/spec");

   Gela.Build;

   declare
      use type Gela.Test_Iterators.Append.Iterator;
      Dirs  : constant Gela.Test_Iterators.Iterator'Class :=
        Gela.Test_Iterators.Dir.Create (Source, Build);
      ACATS : constant Gela.Test_Iterators.Iterator'Class :=
        Gela.Test_Iterators.ACATS.Create (Source, Build);
      Iterator : Gela.Test_Iterators.Append.Iterator := Dirs + ACATS;
   begin
      Iterator.Start;

      while Iterator.Has_More_Tests loop
         Iterator.Next (Test);
         Test.Run;

         case Test.Status is
         when Gela.Test_Cases.Failure
            | Gela.Test_Cases.Error =>

            Failed := True;
         when others =>
            null;
         end case;
      end loop;

      Gela.Bitten_Report.Generate (Iterator, Report);
   end;

   declare
      File : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Create
        (File, Name => Conv.To_String (Output));
      Ada.Wide_Wide_Text_IO.Put_Line (File, Report.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Close (File);
   end;

   Gela.Bitten_Coverage.Generate (Build, Report);

   declare
      File : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Create
        (File, Name => Conv.To_String (Coverage));
      Ada.Wide_Wide_Text_IO.Put_Line (File, Report.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Close (File);
   end;

   if Failed then
      Ada.Wide_Wide_Text_IO.Put_Line ("Some tests failed");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
exception
   when E : others =>
      Ada.Wide_Wide_Text_IO.Put_Line
        (Ada.Characters.Conversions.To_Wide_Wide_String
           (Ada.Exceptions.Exception_Information (E)));

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Gela.Test_Driver;
