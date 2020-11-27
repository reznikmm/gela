--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Source_Buffers;

private with Ada.Containers.Hashed_Maps;

package Program.Symbols.Tables is
   pragma Preelaborate;

   type Symbol_Table is tagged limited private;

   procedure Initialize (Self : in out Symbol_Table);
   --  Initialize given symbol table with predefinced symbols

   function Find
     (Self  : Symbol_Table'Class;
      Value : Program.Text) return Symbol;
   --  Return symbol for given Text or No_Symbol if no such value in the table

   procedure Find_Or_Create
     (Self   : in out Symbol_Table'Class;
      Buffer : not null Program.Source_Buffers.Source_Buffer_Access;
      Span   : Program.Source_Buffers.Span;
      Result : out Symbol);

   function Symbol_Text
     (Self   : Symbol_Table'Class;
      Symbol : Program.Symbols.Symbol) return Program.Text;
   --  Return text of the Symbol

private
   type Symbol_Reference is record
      Buffer : not null Program.Source_Buffers.Source_Buffer_Access;
      Span   : Program.Source_Buffers.Span;
   end record;

   function Equal (Left, Right : Symbol_Reference) return S.Boolean;

   function Hash (Value : Symbol_Reference) return Ada.Containers.Hash_Type;

   package Symbol_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Reference,
      Element_Type    => Program.Symbols.Symbol,
      Hash            => Hash,
      Equivalent_Keys => Equal,
      "="             => Program.Symbols."=");

   package Reference_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Program.Symbols.Symbol,
      Element_Type    => Symbol_Reference,
      Hash            => Program.Symbols.Hash,
      Equivalent_Keys => Program.Symbols."=",
      "="             => Equal);

   function Hash (Value : Symbol) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type'Mod (Value));

   type Predefined_Source_Buffer is new Program.Source_Buffers.Source_Buffer
     with null record;

   overriding function Text
     (Self : Predefined_Source_Buffer;
      Span : Program.Source_Buffers.Span)
         return Program.Text;

   overriding procedure Read
     (Self : in out Predefined_Source_Buffer;
      Data : out Program.Source_Buffers.Character_Info_Array;
      Last : out Natural) is null;

   overriding procedure Rewind
     (Self : in out Predefined_Source_Buffer) is null;

   type Symbol_Table is tagged limited record
      Map         : Symbol_Maps.Map;
      References  : Reference_Maps.Map;
      Buffer      : aliased Predefined_Source_Buffer;
      Last_Symbol : Program.Symbols.Symbol := Program.Symbols.X_Symbol'Last;
   end record;

   type Symbol_List is null record;
   type Symbol_List_Table is tagged limited null record;

end Program.Symbols.Tables;
