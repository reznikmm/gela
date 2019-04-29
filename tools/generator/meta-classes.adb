--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Meta.Classes is

   ----------------
   -- Add_Parent --
   ----------------

   not overriding procedure Add_Parent
     (Self : in out Class;
      Name : League.Strings.Universal_String) is
   begin
      Self.Parents.Append (Name);
   end Add_Parent;

   ------------------
   -- Add_Property --
   ------------------

   not overriding procedure Add_Property
     (Self  : in out Class;
      Value : Property) is
   begin
      Self.Last_Prop := Self.Last_Prop + 1;
      Self.Properties (Self.Last_Prop) := Value;
   end Add_Property;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self        : in out Class;
      Name        : League.Strings.Universal_String;
      Is_Abstract : Boolean := False) is
   begin
      Self.Name := Name;
      Self.Is_Abstract := Is_Abstract;
      Self.Parents.Clear;
      Self.Last_Prop := 0;
   end Initialize;

   -----------------
   -- Is_Abstract --
   -----------------

   not overriding function Is_Abstract (Self : Class) return Boolean is
   begin
      return Self.Is_Abstract;
   end Is_Abstract;

   ----------
   -- Name --
   ----------

   not overriding function Name
     (Self : Class) return League.Strings.Universal_String is
   begin
      return Self.Name;
   end Name;

   -------------
   -- Parents --
   -------------

   not overriding function Parents
     (Self : Class) return League.String_Vectors.Universal_String_Vector is
   begin
      return Self.Parents;
   end Parents;

   ----------------
   -- Properties --
   ----------------

   not overriding function Properties (Self : Class) return Property_Array is
   begin
      return Self.Properties (1 .. Self.Last_Prop);
   end Properties;

end Meta.Classes;
