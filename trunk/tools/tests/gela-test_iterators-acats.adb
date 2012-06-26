------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Conv;
with Gela.Test_Cases.Input;

with League.String_Vectors;

with Ada.IO_Exceptions;
with Ada.Directories;

package body Gela.Test_Iterators.ACATS is

   use type League.Strings.Universal_String;

   ------------
   -- Create --
   ------------

   function Create
     (Source, Build : League.Strings.Universal_String)
     return Iterator
   is

      New_Line  : constant Wide_Wide_Character := Wide_Wide_Character'Val (10);
      List_File : constant League.Strings.Universal_String :=
        Source & "/ts_00018/single_file_tests";
      List_Text : League.Strings.Universal_String;
      List      : League.String_Vectors.Universal_String_Vector;

      Result : Iterator;
      Dir    : Ada.Directories.Directory_Entry_Type;
   begin
      declare
         Search : Ada.Directories.Search_Type;
      begin
         Ada.Directories.Start_Search
           (Search,
            Conv.To_String (Source),
            "ts_00018");

         if Ada.Directories.More_Entries (Search) then
            Ada.Directories.Get_Next_Entry (Search, Dir);
         else
            return Result;
         end if;
      end;

      List_Text := Conv.Read_File (List_File);

      List := List_Text.Split (New_Line, League.Strings.Skip_Empty);

      for J in 1 .. List.Length loop
         declare
            Test : Gela.Test_Cases.Test_Case_Access;
            Line : constant League.String_Vectors.Universal_String_Vector :=
               List.Element (J).Split (' ');
         begin
            Test := Gela.Test_Cases.Test_Case_Access
              (Gela.Test_Cases.Input.Create
                 (Dir, Build,
                  Line.Element (1),
                  Line.Element (2) & New_Line));

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
