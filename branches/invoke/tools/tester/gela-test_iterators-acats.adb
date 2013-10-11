------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Test_Tools;
with Gela.Test_Cases.Runner;

with League.String_Vectors;

with Ada.IO_Exceptions;

package body Gela.Test_Iterators.ACATS is

   use type League.Strings.Universal_String;

   ------------
   -- Create --
   ------------

   function Create
     (Command   : League.Strings.Universal_String;
      List_File : League.Strings.Universal_String;
      ACATS     : League.Strings.Universal_String)
     return Iterator
   is

      New_Line  : constant Wide_Wide_Character := Wide_Wide_Character'Val (10);
      List_Text : League.Strings.Universal_String;
      List      : League.String_Vectors.Universal_String_Vector;

      Result : Iterator;
   begin
      List_Text := Gela.Test_Tools.Read_File (List_File);

      List := List_Text.Split (New_Line, League.Strings.Skip_Empty);

      for J in 1 .. List.Length loop
         declare
            Test : Gela.Test_Cases.Test_Case_Access;
            Args : League.String_Vectors.Universal_String_Vector;
            Line : constant League.String_Vectors.Universal_String_Vector :=
              List.Element (J).Split (' ');
            File : constant League.Strings.Universal_String :=
              ACATS & Line.Element (1);
            Dir  : constant League.Strings.Universal_String :=
              File.Slice (1, File.Last_Index ('/'));
            Name : constant League.Strings.Universal_String :=
              File.Slice (File.Last_Index ('/') + 1, File.Length);
         begin
            Args.Append ("-I" & Dir);
            Args.Append (Name);

            Test := new Gela.Test_Cases.Runner.Test_Case'
              (Gela.Test_Cases.Runner.Create
                 (Command   => Command,
                  Arguments => Args,
                  Info      =>
                    (Name    => Name,
                     Fixture => League.Strings.To_Universal_String ("ACATS"),
                     File    => <>)));

            Result.List.Append (Test);
         end;
      end loop;

      return Result;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return Result;
   end Create;

   --------------------
   -- Has_More_Tests --
   --------------------

   function Has_More_Tests (Self : Iterator) return Boolean is
   begin
      return Lists.Has_Element (Self.Next);
   end Has_More_Tests;

   ----------
   -- Next --
   ----------

   procedure Next
     (Self : in out Iterator;
      Test : out Gela.Test_Cases.Test_Case_Access) is
   begin
      Test := Lists.Element (Self.Next);
      Lists.Next (Self.Next);
   end Next;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Iterator) is
   begin
      Self.Next := Self.List.First;
   end Start;

end Gela.Test_Iterators.ACATS;
