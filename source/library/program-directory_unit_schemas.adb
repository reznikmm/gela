--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;
with Interfaces.C_Streams;

package body Program.Directory_Unit_Schemas is

   function Find_File
     (Self      : Directory_Unit_Schema'Class;
      Base_Name : Program.Text) return Program.Text;

   function Exists (Name : Wide_Wide_String) return Boolean;

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory
     (Self : in out Directory_Unit_Schema;
      Path : Program.Text)
   is
   begin
      if Ada.Strings.Wide_Wide_Unbounded.Length (Self.Path) = 0 then
         Self.Path := Ada.Strings.Wide_Wide_Unbounded.
           To_Unbounded_Wide_Wide_String (Path);
      elsif Self.Next = null then
         Self.Next := new Directory_Unit_Schema'
           (Self.Base_Name.all'Unchecked_Access, others => <>);
      else
         Self.Next.Add_Directory (Path);
      end if;
   end Add_Directory;

   --------------------
   -- Body_Text_Name --
   --------------------

   overriding function Body_Text_Name
     (Self : Directory_Unit_Schema; Name : Program.Text) return Program.Text
   is
      Base : constant Program.Text := Self.Base_Name.Body_Text_Name (Name);
   begin
      return Self.Find_File (Base);
   end Body_Text_Name;

   ---------------------------
   -- Declaration_Text_Name --
   ---------------------------

   overriding function Declaration_Text_Name
     (Self : Directory_Unit_Schema; Name : Program.Text) return Program.Text
   is
      Base : constant Program.Text :=
        Self.Base_Name.Declaration_Text_Name (Name);
   begin
      return Self.Find_File (Base);
   end Declaration_Text_Name;

   ------------
   -- Exists --
   ------------

   function Exists (Name : Wide_Wide_String) return Boolean is
      File : constant String := Ada.Characters.Conversions.To_String (Name) &
        Ada.Characters.Latin_1.NUL;
   begin
      return Interfaces.C_Streams.file_exists (File'Address) /= 0;
   end Exists;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (Self      : Directory_Unit_Schema'Class;
      Base_Name : Program.Text) return Program.Text
   is
      Directory : constant Program.Text :=
        Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Self.Path);
   begin
      if (Directory'Length = 0 or else
         Directory (Directory'Last) in '/' | '\')
        and then Exists (Directory & Base_Name)
      then
         return Directory & Base_Name;
      elsif Exists (Directory & '/' & Base_Name) then
         return Directory & '/' & Base_Name;
      elsif Self.Next = null then
         return "";
      else
         return Self.Next.Find_File (Base_Name);
      end if;
   end Find_File;

   ------------------------
   -- Standard_Text_Name --
   ------------------------

   overriding function Standard_Text_Name
     (Self : Directory_Unit_Schema) return Program.Text
   is
      Base : constant Program.Text := Self.Base_Name.Standard_Text_Name;
   begin
      return Self.Find_File (Base);
   end Standard_Text_Name;

   -----------------------
   -- Subunit_Text_Name --
   -----------------------

   overriding function Subunit_Text_Name
     (Self : Directory_Unit_Schema; Name : Program.Text) return Program.Text
   is
      Base : constant Program.Text := Self.Base_Name.Subunit_Text_Name (Name);
   begin
      return Self.Find_File (Base);
   end Subunit_Text_Name;

end Program.Directory_Unit_Schemas;
