--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Hash;
with Ada.Wide_Wide_Characters.Handling;

package body Program.Plain_Contexts is

   -----------------------------
   -- Compilation_Unit_Bodies --
   -----------------------------

   overriding function Compilation_Unit_Bodies (Self : Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access is
   begin
      return Self.Bodies'Unchecked_Access;
   end Compilation_Unit_Bodies;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self  : Unit_Vector;
      Index : Positive)
        return not null Program.Compilation_Units.Compilation_Unit_Access is
   begin
      return Self.List.Element (Index);
   end Element;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Symbol_Reference) return Boolean is
      Left_Text  : constant Text := Ada.Wide_Wide_Characters.Handling.To_Lower
        (Left.Buffer.Text (Left.Span));

      Right_Text : constant Text := Ada.Wide_Wide_Characters.Handling.To_Lower
        (Right.Buffer.Text (Right.Span));
   begin
      return Left_Text = Right_Text;
   end Equal;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Symbol_List_Item) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Ada.Containers.Hash_Type'Mod (Value.Prefix)
        + 100003 * Ada.Containers.Hash_Type'Mod (Value.Symbol);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Symbol_Reference) return Ada.Containers.Hash_Type is
      Value_Text  : constant Text := Ada.Wide_Wide_Characters.Handling.To_Lower
        (Value.Buffer.Text (Value.Span));
   begin
      return Ada.Strings.Wide_Wide_Hash (Value_Text);
   end Hash;

   ---------------
   -- Find_Unit --
   ---------------

   overriding function Find_Unit
     (Self : Unit_Vector;
      Name : Text) return Program.Compilation_Units.Compilation_Unit_Access
   is
      function Find (Value : Text) return Program.Symbols.Symbol;
      --  Find Value in the Symbol_Set

      ----------
      -- Find --
      ----------

      function Find (Value : Text) return Program.Symbols.Symbol is

         type Dummy_Source_Buffer is new Program.Source_Buffers.Source_Buffer
            with null record;

         overriding function Text
           (Self   : Dummy_Source_Buffer;
            Unused : Program.Source_Buffers.Span)
              return Program.Text is (Value);

         overriding procedure Read
           (Self : in out Dummy_Source_Buffer;
            Data : out Program.Source_Buffers.Character_Info_Array;
            Last : out Natural) is null;

         overriding procedure Rewind
           (Self : in out Dummy_Source_Buffer) is null;

         Dummy : aliased Dummy_Source_Buffer;
         Ref   : constant Symbol_Reference := (Dummy'Unchecked_Access, (1, 0));
      begin
         if Self.Context.Symbols.Contains (Ref) then
            return Self.Context.Symbols.Element (Ref);
         else
            return Program.Symbols.No_Symbol;
         end if;
      end Find;

      Cursor : Symbol_List_Maps.Cursor;
      Item   : Symbol_List_Item := (0, 0);
      Result : Symbol_List_Index;
      Prev   : Positive;
      Dot    : Natural := Name'First - 1;
   begin
      loop
         Prev := Dot + 1;
         Dot := Ada.Strings.Wide_Wide_Fixed.Index (Name, ".", Prev);
         exit when Dot not in Name'Range;
         Item.Symbol := Find (Name (Prev .. Dot - 1));
         Cursor := Self.Context.Symbol_Lists.Find (Item);

         if Symbol_List_Maps.Has_Element (Cursor) then
            Item.Prefix := Symbol_List_Maps.Element (Cursor);
         else
            return null;
         end if;
      end loop;

      Item.Symbol := Find (Name (Dot + 1 .. Name'Last));
      Cursor := Self.Context.Symbol_Lists.Find (Item);

      if Symbol_List_Maps.Has_Element (Cursor) then
         Result := Symbol_List_Maps.Element (Cursor);
      else
         return null;
      end if;

      if Self.Map.Contains (Result) then
         return Self.Map.Element (Result);
      else
         return null;
      end if;
   end Find_Unit;

   ----------------
   -- Get_Length --
   ----------------

   overriding function Get_Length (Self : Unit_Vector) return Positive is
   begin
      return Self.List.Last_Index;
   end Get_Length;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   overriding function Library_Unit_Declarations (Self : Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access is
   begin
      return Self.Declarations'Unchecked_Access;
   end Library_Unit_Declarations;

end Program.Plain_Contexts;
