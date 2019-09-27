--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Strings.Wide_Wide_Fixed;

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

   ---------------------------
   -- Find_Or_Create_Symbol --
   ---------------------------

   procedure Find_Or_Create_Symbol
     (Self : in out Context'Class;
      Buffer : not null Program.Source_Buffers.Source_Buffer_Access;
      Span   : Program.Source_Buffers.Span;
      Result : out Program.Symbols.Symbol) is
   begin
      Self.Symbols.Find_Or_Create (Buffer, Span, Result);
   end Find_Or_Create_Symbol;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Symbol_List_Item) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Ada.Containers.Hash_Type'Mod (Value.Prefix)
        + 100003 * Ada.Containers.Hash_Type'Mod (Value.Symbol);
   end Hash;

   ---------------
   -- Find_Unit --
   ---------------

   overriding function Find_Unit
     (Self : Unit_Vector;
      Name : Text) return Program.Compilation_Units.Compilation_Unit_Access
   is
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
         Item.Symbol := Self.Context.Symbols.Find (Name (Prev .. Dot - 1));
         Cursor := Self.Context.Symbol_Lists.Find (Item);

         if Symbol_List_Maps.Has_Element (Cursor) then
            Item.Prefix := Symbol_List_Maps.Element (Cursor);
         else
            return null;
         end if;
      end loop;

      Item.Symbol := Self.Context.Symbols.Find (Name (Dot + 1 .. Name'Last));
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Context'Class) is
   begin
      Self.Symbols.Initialize;
   end Initialize;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   overriding function Library_Unit_Declarations (Self : Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access is
   begin
      return Self.Declarations'Unchecked_Access;
   end Library_Unit_Declarations;

end Program.Plain_Contexts;
