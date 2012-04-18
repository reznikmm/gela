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
with Gela.Ada_Test_Cases;

package body Gela.Test_Iterators is

   ------------
   -- Create --
   ------------

   function Create (Path : League.Strings.Universal_String) return Iterator is
      use Ada.Directories;

      Root : constant String := Conv.To_String (Path);
      Each : Search_Type;
      Item : Directory_Entry_Type;
      Test : Gela.Test_Cases.Test_Case_Access;

      Result : Iterator;
   begin
      Start_Search (Each, Root, "ts_*", (Directory => True, others => False));

      while More_Entries (Each) loop
         Get_Next_Entry (Each, Item);

         Test := new Gela.Test_Cases.Test_Case'Class'
           (Gela.Ada_Test_Cases.Create (Item));

         Result.Map.Insert
           (Conv.To_Universal_String (Simple_Name (Item)), Test);
      end loop;

      return Result;
   end Create;

   --------------------
   -- Has_More_Tests --
   --------------------

   function Has_More_Tests (Self : Iterator) return Boolean is
   begin
      return Maps.Has_Element (Self.Next);
   end Has_More_Tests;

   ----------
   -- Next --
   ----------

   procedure Next
     (Self : in out Iterator;
      Test : out Gela.Test_Cases.Test_Case_Access) is
   begin
      Test := Maps.Element (Self.Next);
      Maps.Next (Self.Next);
   end Next;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Iterator) is
   begin
      Self.Next := Self.Map.First;
   end Start;

end Gela.Test_Iterators;
