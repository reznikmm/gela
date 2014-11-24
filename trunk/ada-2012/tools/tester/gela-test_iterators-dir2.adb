------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Directories;
with Gela.Test_Tools;
with Gela.Test_Cases.Input;
with Gela.Test_Cases.Execute;
with Gela.Test_Cases.Valgrind;

package body Gela.Test_Iterators.Dir2 is

   use type League.Strings.Universal_String;

--     function "+"
--       (Text : Wide_Wide_String)
--           return League.Strings.Universal_String
--           renames League.Strings.To_Universal_String;

   procedure Add_Each_Input
     (Result : in out Iterator;
      Dir    : Ada.Directories.Directory_Entry_Type;
      Build  : League.Strings.Universal_String;
      Found  : out Boolean);

   --------------------
   -- Add_Each_Input --
   --------------------

   procedure Add_Each_Input
     (Result : in out Iterator;
      Dir    : Ada.Directories.Directory_Entry_Type;
      Build  : League.Strings.Universal_String;
      Found  : out Boolean)
   is
      use Ada.Directories;

      Each  : Search_Type;
      Item  : Directory_Entry_Type;
      Run   : Gela.Test_Cases.Execute.Test_Case_Access;
      Test  : Gela.Test_Cases.Test_Case_Access;
      Input : League.Strings.Universal_String;
      Expect : League.Strings.Universal_String;
   begin
      Start_Search
        (Each,
         Full_Name (Dir),
         "*.in",
         (Ordinary_File => True, others => False));

      Found := False;

      while More_Entries (Each) loop
         Get_Next_Entry (Each, Item);

         Input := League.Strings.From_UTF_8_String (Simple_Name (Item));

         declare
            Full_Path : constant String := Full_Name (Item);
            Out_Path  : constant String := Full_Path
              (Full_Path'First .. Full_Path'Last - 2) & "out";
         begin
            if Ada.Directories.Exists (Out_Path) then
               Expect := Gela.Test_Tools.Read_File (Out_Path);
            else
               Expect := Gela.Test_Cases.Ok;
            end if;
         end;

         Run := Gela.Test_Cases.Input.Create (Dir, Build, Input, Expect);
         Test := Gela.Test_Cases.Test_Case_Access (Run);

         Result.List.Append (Test);

         --  run with Valgrind

         Run := Gela.Test_Cases.Input.Create
           (Dir, Build, Input, Expect);

         Test := Gela.Test_Cases.Valgrind.Create
           (Run);

         Result.List.Append (Test);

         Found := True;
      end loop;
   end Add_Each_Input;

   ------------
   -- Create --
   ------------

   function Create
     (Source, Build : League.Strings.Universal_String)
     return Iterator
   is
      use Ada.Directories;

      Root : constant String := Source.To_UTF_8_String;
      Each : Search_Type;
      Item : Directory_Entry_Type;
      Test : Gela.Test_Cases.Test_Case_Access;
      Run  : Gela.Test_Cases.Execute.Test_Case_Access;

      Result : Iterator;
      Found  : Boolean;
   begin

      Start_Search (Each, Root, "ts_*", (Directory => True, others => False));

      while More_Entries (Each) loop
         Get_Next_Entry (Each, Item);

         Add_Each_Input (Result, Item, Build, Found);

         if not Found then
            declare
               Expect    : League.Strings.Universal_String;
               Full_Path : constant String := Full_Name (Item);
               Out_Path  : constant String :=
                 Full_Path & "/" & Simple_Name (Item) & ".out";
            begin
               if Ada.Directories.Exists (Out_Path) then
                  Expect := Gela.Test_Tools.Read_File (Out_Path);
               else
                  Expect := Gela.Test_Cases.Ok;
               end if;

               Test := Gela.Test_Cases.Test_Case_Access
                 (Gela.Test_Cases.Execute.Create (Item, Build, Expect));

               Result.List.Append (Test);

               Run := Gela.Test_Cases.Execute.Create (Item, Build, Expect);

               Test := Gela.Test_Cases.Valgrind.Create (Run);

               Result.List.Append (Test);
            end;
         end if;
      end loop;

      return Result;
   end Create;

   ----------------------
   -- Create_GPR_Build --
   ----------------------


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

end Gela.Test_Iterators.Dir2;
