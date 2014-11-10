with Ada.Text_IO;

package body Gela.Engines is

   ---------
   -- Get --
   ---------

   function Get
     (Self     : access Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      Key : constant Rule_Key :=
        (Asis.Extensions.Flat_Kinds.Flat_Kind (Element), Property);
      Pos : constant Text_Rule_Maps.Cursor :=
        Self.Text_Rules.Find (Key);
      Rule : Text_Rule_Callback;
   begin
      if Text_Rule_Maps.Has_Element (Pos) then
         Rule := Text_Rule_Maps.Element (Pos);

         return Rule.all (Self, Element, Property);
      else
         Ada.Text_IO.Put ("Rule not found for kind ");
         Ada.Text_IO.Put
           (Asis.Extensions.Flat_Kinds.Element_Flat_Kind'Image (Key.Kind));
         Ada.Text_IO.Put (" property ");
         Ada.Text_IO.Put_Line
           (Gela.Properties.Property_Name'Image (Key.Property));
         raise Constraint_Error;
      end if;
   end Get;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Rule_Key) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;

      Last : constant Ada.Containers.Hash_Type :=
        Gela.Properties.Property_Name'Pos
          (Gela.Properties.Property_Name'Last);
      This : constant Ada.Containers.Hash_Type :=
        Gela.Properties.Property_Name'Pos (Item.Property);
   begin
      return (Last + 1) *
        Asis.Extensions.Flat_Kinds.Element_Flat_Kind'Pos (Item.Kind) + This;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Asis.Element) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Asis.Elements.Hash (Item));
   end Hash;

   ---------
   -- Map --
   ---------

   function Map
     (Self     : access Engine;
      Element  : Asis.Element) return Mapped_Element
   is
      Pos : constant Element_Maps.Cursor := Self.Element_Map.Find (Element);
      Result : Mapped_Element;
   begin
      if Element_Maps.Has_Element (Pos) then
         Result := Element_Maps.Element (Pos);
      else
         Result := Self.Next_Mapped;
         Self.Element_Map.Insert (Element, Result);
         Self.Next_Mapped := Self.Next_Mapped + 1;
      end if;

      return Result;
   end Map;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (Self     : in out Engine;
      Kind     : Asis.Extensions.Flat_Kinds.Element_Flat_Kind;
      Property : Gela.Properties.Property_Name;
      Action   : Text_Rule_Callback)
   is
      Key : constant Rule_Key := (Kind, Property);
   begin
      Self.Text_Rules.Insert (Key, Action);
   end Register_Rule;

   --------------------
   -- Text_Container --
   --------------------

   function Text_Container
     (Self : access Engine)
      return Gela.Properties.Text.Text_Container_Access is
   begin
      return Self.Text_Container'Access;
   end Text_Container;

end Gela.Engines;
