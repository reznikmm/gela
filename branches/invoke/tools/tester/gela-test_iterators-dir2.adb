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
with Gela.Test_Cases.Append;
with Gela.Test_Cases.Build;
with Gela.Test_Cases.Input;
with Gela.Test_Cases.Execute;
with Gela.Test_Cases.Valgrind;
with League.String_Vectors;

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

   function Create_GPR_Build
     (Dir    : Ada.Directories.Directory_Entry_Type;
      Build  : League.Strings.Universal_String)
      return Test_Cases.Build.Test_Case_Access;

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
      use Gela.Test_Cases.Append;

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

         Run := Gela.Test_Cases.Input.Create
           (Dir, Build, Input, Expect);

         Result.List.Append (Create_GPR_Build (Dir, Build) + Run);

         --  run with Valgrind

         Run := Gela.Test_Cases.Input.Create
           (Dir, Build, Input, Expect);

         Test := Gela.Test_Cases.Valgrind.Create
           (Create_GPR_Build (Dir, Build) + Run);

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
      use Gela.Test_Cases.Append;

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

               Result.List.Append (Create_GPR_Build (Item, Build) + Test);

               Run := Create_GPR_Build (Item, Build) +
                 Gela.Test_Cases.Execute.Create (Item, Build, Expect);

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

   function Create_GPR_Build
     (Dir    : Ada.Directories.Directory_Entry_Type;
      Build  : League.Strings.Universal_String)
         return Test_Cases.Build.Test_Case_Access
   is
      Dir_Name  : constant League.Strings.Universal_String :=
        League.Strings.From_UTF_8_String (Ada.Directories.Simple_Name (Dir));
      Full_Path : constant League.Strings.Universal_String :=
        League.Strings.From_UTF_8_String (Ada.Directories.Full_Name (Dir));
      GPR_Build : Gela.Test_Cases.Build.Test_Case_Access;
      Options   : League.String_Vectors.Universal_String_Vector;
   begin
      Options.Append (Dir_Name & ".adb");
      Options.Append ("-XSOURCE_DIR=" & Dir_Name);
      Options.Append ("-XOBJECT_DIR=" & Build & "/" & Dir_Name);

      GPR_Build := new Gela.Test_Cases.Build.Test_Case'
        (Gela.Test_Cases.Build.Create
           (Source  => Full_Path & "/../../..",
            Build   => Build,
            Project => Full_Path & "/../simple.gpr",
            Options => Options));

      return GPR_Build;
   end Create_GPR_Build;

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
