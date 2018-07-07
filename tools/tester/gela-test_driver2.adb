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
with System.Multiprocessors;
with Gela.Bitten_Report;
with Gela.Host;
with Gela.Test_Cases;
with Gela.Test_Iterators.Dir2;
--  We don't use Dir2 now, but we would like to compile it to avoid errors.
pragma Unreferenced (Gela.Test_Iterators.Dir2);
with Gela.Test_Iterators.ACATS;
with Gela.Test_Iterators.Append;
with Gela.Test_Tools;

with League.Strings;

procedure Gela.Test_Driver2 is
   use type Gela.Test_Cases.Test_Case_Access;
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

   Compiler_Test : constant League.Strings.Universal_String :=
     Gela.Host.Source_Root & "/compiler/testsuite";
   --  Path to directory containing compiler tests sources
   --  (trunk/compiler/testsuite)

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

   protected Queue is
      entry Put (Test : Gela.Test_Cases.Test_Case_Access);
      entry Get (Test : out Gela.Test_Cases.Test_Case_Access);
   private
      Data : Gela.Test_Cases.Test_Case_Access;
   end Queue;

   protected body Queue is

      entry Put (Test : Gela.Test_Cases.Test_Case_Access)
        when Data = null is
      begin
         Data := Test;
      end Put;

      entry Get (Test : out Gela.Test_Cases.Test_Case_Access) when Data /= null
      is
      begin
         Test := Data;
         Data := null;
      end Get;

   end Queue;

   task type Worker is
      entry Complete (Failed : in out Boolean);
   end Worker;

   task body Worker is
      Failed : Boolean := False;
      Test   : Gela.Test_Cases.Test_Case_Access;
   begin
      loop
         select
            accept Complete (Failed : in out Boolean) do
               Failed := Failed or Worker.Failed;
            end Complete;

            exit;
         else
            select
               Queue.Get (Test);
               Test.Run;

               case Test.Status is
                  when Gela.Test_Cases.Failure
                     | Gela.Test_Cases.Error =>

                     Failed := True;
                  when others =>
                     null;
               end case;

            or
               delay 0.5;
            end select;
         end select;
      end loop;
   exception
      when E : others =>
         Ada.Wide_Wide_Text_IO.Put_Line ("Worker dead!");
         Ada.Wide_Wide_Text_IO.Put_Line
           (Ada.Characters.Conversions.To_Wide_Wide_String
              (Ada.Exceptions.Exception_Information (E)));
   end Worker;

   CPU : constant System.Multiprocessors.CPU_Range :=
     System.Multiprocessors.Number_Of_CPUs;

   Workers  : array (1 .. CPU) of Worker;
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
      Lexer : constant Gela.Test_Iterators.Iterator'Class :=
        Gela.Test_Iterators.ACATS.Create
          (Command   => Build & "/tests/gela/lexer_test",
           List_File => Tests & "/gela/lexer/list.txt",
           Fixture   => League.Strings.To_Universal_String ("lexer_test"),
           ACATS     => ACATS_Dir);
      Parser : constant Gela.Test_Iterators.Iterator'Class :=
        Gela.Test_Iterators.ACATS.Create
          (Command   => Build & "/tests/gela/parser_test",
           List_File => Tests & "/gela/parser/list.txt",
           Fixture   => League.Strings.To_Universal_String ("parser_test"),
           ACATS     => ACATS_Dir);
      Def_Name : constant Gela.Test_Iterators.Iterator'Class :=
        Gela.Test_Iterators.ACATS.Create
          (Command   => Build & "/asis/def_name",
           List_File => Tests & "/asis/def_name/list.txt",
           Fixture   => League.Strings.To_Universal_String ("def_name"),
           ACATS     => ACATS_Dir);
      ASIS2XML : constant Gela.Test_Iterators.Iterator'Class :=
        Gela.Test_Iterators.ACATS.Create
          (Command   => Build & "/asis/gela2xml",
           List_File => Tests & "/asis/asis2xml.gpl/list.txt",
           Fixture   => League.Strings.To_Universal_String ("asis2xml"),
           ACATS     => ACATS_Dir);
      Run_Compiler : constant Gela.Test_Iterators.Iterator'Class :=
        Gela.Test_Iterators.ACATS.Create
          (Command   => Compiler_Test & "/comp.sh",
           List_File => Compiler_Test & "/list.txt",
           Fixture   => League.Strings.To_Universal_String ("comp.sh"),
           ACATS     => ACATS_Dir);
      Iterator : Gela.Test_Iterators.Append.Iterator :=
        Lexer + Parser + Def_Name + ASIS2XML + Run_Compiler;
   begin
      Iterator.Start;

      while Iterator.Has_More_Tests loop
         Iterator.Next (Test);
         Ada.Wide_Wide_Text_IO.Put_Line
           (Test.Fixture.To_Wide_Wide_String & "/" &
              Test.Name.To_Wide_Wide_String);
         Queue.Put (Test);
      end loop;

      for J in Workers'Range loop
         Workers (J).Complete (Failed);
      end loop;

      Gela.Bitten_Report.Generate (Iterator, Report);
   end;

   Gela.Test_Tools.Write_File
     (File => Output,
      Text => Report);

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
