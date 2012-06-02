------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Directories;
with Gela.Conv;
with Gela.Run_Test_Cases;
with Gela.Build_Test_Cases;
with Gela.Input_Test_Cases;
with Gela.Valgrind_Test_Cases;
with League.String_Vectors;

package body Gela.Test_Iterators is

   use type League.Strings.Universal_String;

   function "+"
     (Text : Wide_Wide_String)
         return League.Strings.Universal_String
         renames League.Strings.To_Universal_String;

   procedure Add_Each_Input
     (Result : in out Iterator;
      Dir    : Ada.Directories.Directory_Entry_Type;
      Build  : League.Strings.Universal_String;
      Found  : out Boolean);

   procedure Add_Build_ASIS
     (Result : in out Iterator;
      Source : League.Strings.Universal_String;
      Build  : League.Strings.Universal_String;
      Gcov   : Boolean := False);

   procedure Apply_Gcov_Options
     (Test : Gela.Build_Test_Cases.Test_Case_Access);

   --------------------
   -- Add_Build_ASIS --
   --------------------

   procedure Add_Build_ASIS
     (Result : in out Iterator;
      Source : League.Strings.Universal_String;
      Build  : League.Strings.Universal_String;
      Gcov   : Boolean := False)
   is
      use Gela.Build_Test_Cases;

      Test : constant Test_Case_Access := new Test_Case'
        (Create (Source & "/../..", Build, +"gela_asis.gpr"));
   begin
      if Gcov then
         Apply_Gcov_Options (Test);
      end if;

      Result.List.Append (Gela.Test_Cases.Test_Case_Access (Test));
   end Add_Build_ASIS;

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
      Run   : Gela.Run_Test_Cases.Test_Case_Access;
      Test  : Gela.Test_Cases.Test_Case_Access;
      Input : League.Strings.Universal_String;

   begin
      Start_Search
        (Each,
         Full_Name (Dir),
         "*.in",
         (Ordinary_File => True, others => False));

      Found := False;

      while More_Entries (Each) loop
         Get_Next_Entry (Each, Item);

         Input := Conv.To_Universal_String (Simple_Name (Item));

         Run := new Gela.Run_Test_Cases.Test_Case'Class'
           (Gela.Run_Test_Cases.Create (Dir, Build));

         Test := new Gela.Run_Test_Cases.Test_Case'Class'
           (Gela.Input_Test_Cases.Create (Run, Input));

         Result.List.Append (Test);

         --  run with Valgrind
         Run := new Gela.Run_Test_Cases.Test_Case'Class'
           (Gela.Run_Test_Cases.Create (Dir, Build));

         Run := new Gela.Run_Test_Cases.Test_Case'Class'
           (Gela.Input_Test_Cases.Create (Run, Input));

         Test := new Gela.Valgrind_Test_Cases.Test_Case'
           (Gela.Valgrind_Test_Cases.Create (Run));

         Result.List.Append (Test);

         --  run with Gcov
         Input := Conv.To_Universal_String (Simple_Name (Item));

         Run := new Gela.Run_Test_Cases.Test_Case'Class'
           (Gela.Run_Test_Cases.Create (Dir, Build & "/gcov"));

         Apply_Gcov_Options (Run.GPR_Build);

         Run := new Gela.Run_Test_Cases.Test_Case'Class'
           (Gela.Input_Test_Cases.Create (Run, Input));

         Run.Set_Name ("gcov " & Run.Name);

         Result.List.Append (Gela.Test_Cases.Test_Case_Access (Run));

         Found := True;
      end loop;
   end Add_Each_Input;

   ------------------------
   -- Apply_Gcov_Options --
   ------------------------

   procedure Apply_Gcov_Options
     (Test : Gela.Build_Test_Cases.Test_Case_Access)
   is
      Options : League.String_Vectors.Universal_String_Vector := Test.Options;
   begin
      Options.Append (+"-XGELA_SOURCE=/../ada");
      Options.Append (+"-cargs");
      Options.Append (+"-fprofile-arcs");
      Options.Append (+"-ftest-coverage");
      Options.Append (+"-largs");
      Options.Append (+"-fprofile-arcs");
      Test.Set_Options (Options);
   end Apply_Gcov_Options;

   ------------
   -- Create --
   ------------

   function Create
     (Source, Build : League.Strings.Universal_String)
     return Iterator
   is
      use Ada.Directories;

      Root : constant String := Conv.To_String (Source);
      Each : Search_Type;
      Item : Directory_Entry_Type;
      Test : Gela.Test_Cases.Test_Case_Access;
      Run  : Gela.Run_Test_Cases.Test_Case_Access;

      Result : Iterator;
      Found  : Boolean;
   begin
      Add_Build_ASIS (Result, Source, Build);
      Add_Build_ASIS (Result, Source, Build & "/gcov", Gcov => True);

      Start_Search (Each, Root, "ts_*", (Directory => True, others => False));

      while More_Entries (Each) loop
         Get_Next_Entry (Each, Item);

         Add_Each_Input (Result, Item, Build, Found);

         if not Found then
            Test := new Gela.Run_Test_Cases.Test_Case'Class'
              (Gela.Run_Test_Cases.Create (Item, Build));

            Result.List.Append (Test);

            Run := new Gela.Run_Test_Cases.Test_Case'Class'
              (Gela.Run_Test_Cases.Create (Item, Build));

            Test := new Gela.Valgrind_Test_Cases.Test_Case'
              (Gela.Valgrind_Test_Cases.Create (Run));

            Result.List.Append (Test);
         end if;
      end loop;

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

end Gela.Test_Iterators;
