------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Test_Cases;
with Gela.Test_Iterators;
with League.Application;
--  with League.Strings;
with League.String_Vectors;

procedure Gela.Test_Driver is
   Args     : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
   Iterator : Gela.Test_Iterators.Iterator :=
     Gela.Test_Iterators.Create (Args.Element (1));
   Test : Gela.Test_Cases.Test_Case_Access;
begin
   Iterator.Start;

   while Iterator.Has_More_Tests loop
      Iterator.Next (Test);
      Test.Run;
   end loop;
end Gela.Test_Driver;
