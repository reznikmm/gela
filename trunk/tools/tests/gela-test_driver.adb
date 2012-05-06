------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;

with Gela.Bitten_Report;
with Gela.Conv;
with Gela.Test_Cases;
with Gela.Test_Iterators;

with League.Application;
with League.Strings;
with League.String_Vectors;

procedure Gela.Test_Driver is
   Args     : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
   Source   : constant League.Strings.Universal_String := Args.Element (1);
   --  Path to directory containing tests' sources (trunk/tests/)
   Build    : constant League.Strings.Universal_String := Args.Element (2);
   --  Directory where build tests (/tmp/build/)
   Output   : constant League.Strings.Universal_String := Args.Element (3);
   --  Where to store report file (/tmp/build/output.xml)
   Iterator : Gela.Test_Iterators.Iterator :=
     Gela.Test_Iterators.Create (Source, Build);
   Test     : Gela.Test_Cases.Test_Case_Access;
   Report   : League.Strings.Universal_String;
   Failed   : Boolean := False;
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

   declare
      File : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Create
        (File, Name => Conv.To_String (Output));
      Ada.Wide_Wide_Text_IO.Put_Line (File, Report.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Close (File);
   end;

   if Failed then
      Ada.Wide_Wide_Text_IO.Put_Line ("Some tests failed");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Gela.Test_Driver;
