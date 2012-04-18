------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Bitten_Report;
with Gela.Test_Cases;
with Gela.Test_Iterators;

with League.Application;
with League.Strings;
with League.String_Vectors;
with Ada.Wide_Wide_Text_IO;

procedure Gela.Test_Driver is
   Args     : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
   Iterator : Gela.Test_Iterators.Iterator :=
     Gela.Test_Iterators.Create (Args.Element (1));
   Test     : Gela.Test_Cases.Test_Case_Access;
   Report   : League.Strings.Universal_String;
begin
   Iterator.Start;

   while Iterator.Has_More_Tests loop
      Iterator.Next (Test);
      Test.Run;
   end loop;

   Gela.Bitten_Report.Generate (Iterator, Report);
   Ada.Wide_Wide_Text_IO.Put_Line (Report.To_Wide_Wide_String);
end Gela.Test_Driver;
