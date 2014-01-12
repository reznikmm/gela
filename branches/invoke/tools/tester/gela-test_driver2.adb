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

with Gela.Bitten_Report;
with Gela.Host;
with Gela.Test_Cases;
with Gela.Test_Iterators.Dir2;
with Gela.Test_Iterators.ACATS;
with Gela.Test_Iterators.Append;

with League.Strings;

procedure Gela.Test_Driver2 is
   use type League.Strings.Universal_String;

   Tests : constant League.Strings.Universal_String :=
     Gela.Host.Source_Root & "/tests";
   --  Path to directory containing tests' sources (trunk/tests/)

   ACATS_Dir : constant League.Strings.Universal_String :=
     Gela.Host.Source_Root & "/../acats/";
   --  Path to directory with ACATS tests (trunk/../acats/)

   Build    : constant League.Strings.Universal_String :=
     Gela.Host.Build_Root;
   --  Directory where build tests (/tmp/build/)

   Output   : constant League.Strings.Universal_String :=
     Build & "/report.xml";
   --  Where to store report file (/tmp/build/report.xml)

--     procedure Generate is
--        Grammars_Directory : constant League.Strings.Universal_String :=
--          Gela.Host.Source_Root & "/source/grammars";
--        Generated_Directory : constant League.Strings.Universal_String :=
--          Grammars_Directory & "/generated";
--        AG_Y : constant League.Strings.Universal_String :=
--          Generated_Directory & "/ag.y";
--     begin
--        Gela.Test_Tools.Write_File
--          (AG_Y, Gela.Test_Tools.Read_File (Grammars_Directory & "/ag.y"));
--        command_line_interface.Push_Arguments (AG_Y.To_UTF_8_String);
--        Ayacc;
--
--        --  FIXME run uaflex2 here
--     end Generate;

   Test     : Gela.Test_Cases.Test_Case_Access;
   Report   : League.Strings.Universal_String;
   Failed   : Boolean := False;
begin
   Ada.Wide_Wide_Text_IO.Put_Line
     ("Build root = " & Build.To_Wide_Wide_String);

   Ada.Wide_Wide_Text_IO.Put_Line
     ("Source root = " & Gela.Host.Source_Root.To_Wide_Wide_String);

   declare
      use type Gela.Test_Iterators.Append.Iterator;
      Dirs  : constant Gela.Test_Iterators.Iterator'Class :=
        Gela.Test_Iterators.Dir2.Create
          (Tests & "/grammars", Build & "/tests/grammars");
      Lexer : constant Gela.Test_Iterators.Iterator'Class :=
        Gela.Test_Iterators.ACATS.Create
          (Command   => Build & "/tests/gela/lexer_test",
           List_File => Tests & "/gela/lexer/list.txt",
           ACATS     => ACATS_Dir);
      Iterator : Gela.Test_Iterators.Append.Iterator := Dirs + Lexer;
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
        (File, Name => Output.To_UTF_8_String);
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
end Gela.Test_Driver2;
