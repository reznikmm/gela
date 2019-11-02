--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Strings.Wide_Wide_Fixed;
with System.Storage_Pools.Subpools;

with Program.Parsers;
with Program.Plain_Compilations;
with Program.Storage_Pools;
with Program.Resolve_Standard;

package body Program.Plain_Contexts is

   type Plain_Compilation_Access is
     access all Program.Plain_Compilations.Compilation;

   -----------------------------
   -- Compilation_Unit_Bodies --
   -----------------------------

   overriding function Compilation_Unit_Bodies (Self : Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access is
   begin
      if Self.Bodies.List.Is_Empty then
         return null;
      else
         return Self.Bodies'Unchecked_Access;
      end if;
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

   ----------
   -- Find --
   ----------

   function Find
     (Self  : Context'Class;
      Value : Program.Text) return Program.Symbols.Symbol is
   begin
      return Self.Symbols.Find (Value);
   end Find;

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

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File
     (Self      : aliased in out Context'Class;
      Text_Name : Text;
      Env       : aliased in out Program.Visibility.Context)
   is
      Units   : Program.Parsers.Unit_Vectors.Vector;
      Pragmas : Program.Parsers.Element_Vectors.Vector;

      Pool : Program.Storage_Pools.Storage_Pool renames
        Program.Storage_Pools.Pool;

      Subpool : constant not null System.Storage_Pools.Subpools.Subpool_Handle
        := Pool.Create_Subpool;

      Compilation : constant Plain_Compilation_Access :=
        new Program.Plain_Compilations.Compilation (Subpool);
      --  Plain_Compilation is a controlled type, so don't allocate it in
      --  the (Subpool)

   begin
      Compilation.Initialize (Self'Unchecked_Access);

      Compilation.Parse_File (Text_Name, Units, Pragmas);

      Env.Create_Empty_Context;

      Program.Resolve_Standard (Unit => Units (1), Env => Env);

      Self.Compilations.Append
        (Program.Compilations.Compilation_Access (Compilation));
      Self.Symbol_Lists.Insert ((0, Program.Symbols.Standard), 1);
      Self.Declarations.Map.Insert (1, Units (1));
      Self.Declarations.List.Append (Units (1));
   end Parse_File;

end Program.Plain_Contexts;
