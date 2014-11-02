with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

with League.Strings;
with League.Strings.Hash;

with Gela.Lexical_Types;
with Gela.Symbol_Sets;

package Gela.Plain_Symbol_Sets is
   pragma Preelaborate;

   type Symbol_Set is new Gela.Symbol_Sets.Symbol_Set with private;

   type Symbol_Set_Access is access all Symbol_Set'Class;

   not overriding procedure Initialize (Self : in out Symbol_Set);
   --  Register predefined operator symbols

   overriding function Get
     (Self  : Symbol_Set;
      Image : League.Strings.Universal_String)
      return Gela.Lexical_Types.Symbol;
   --  Return No_Symbol if there is no such symbol in the set.

   overriding procedure Fetch
     (Self  : in out Symbol_Set;
      Image : League.Strings.Universal_String;
      Value : out Gela.Lexical_Types.Symbol);
   --  Get symbol if already in the set or create new one otherwise.

   overriding function Image
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return League.Strings.Universal_String;
   --  Original image used in Fetch to create given symbol

   overriding function Folded
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return League.Strings.Universal_String;
   --  Image after applying locale-independent simple case folding

   overriding procedure Join
     (Self  : in out Symbol_Set;
      Left  : Gela.Lexical_Types.Symbol;
      Right : Gela.Lexical_Types.Symbol;
      Value : out Gela.Lexical_Types.Symbol);
   --  Fetch symbol corresponding to Left.Right compound name.

   overriding function Prefix
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return Gela.Lexical_Types.Symbol;
   --  Return prefix if Value is compound name or No_Symbol otherwise.

   overriding function Selector
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return Gela.Lexical_Types.Symbol;
   --  Return selector if Value is compound name or Value otherwise.

   overriding procedure Create_List
     (Self  : in out Symbol_Set;
      Head  : Gela.Lexical_Types.Symbol_List :=
        Gela.Lexical_Types.Empty_Symbol_List;
      Tail  : Gela.Lexical_Types.Symbol;
      Value : out Gela.Lexical_Types.Symbol_List);
   --  Get new symbol list as join of Head and Tail.

   overriding procedure Create_List
     (Self  : in out Symbol_Set;
      Head  : Gela.Lexical_Types.Symbol_List;
      Tail  : Gela.Lexical_Types.Symbol_List;
      Value : out Gela.Lexical_Types.Symbol_List);
   --  Get new symbol list as join of Head and Tail.

   overriding function Head
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol_List)
      return Gela.Lexical_Types.Symbol_List;
   --  Return head of Value.

   overriding function Tail
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol_List)
      return Gela.Lexical_Types.Symbol;
   --  Return tail symbol of Value.

private
   --  Simple symbols are in range 1 .. 7FFF_FFFF
   --  First 16#11_0000# of symbols are reserved for character literals
   --  and operator symbols and never stored in sets.
   --  Compound symbols are in range 8000_0000 .. BFFF_FFFF
   --  Symbols in range C000_000 .. FFFF_FFFF are reserved for lists.
   --  For each compound symbol A.B there is a list (A, B)

   type Symbol_Index is range 16#11_0000# .. 16#7FFF_FFFF#;

   use type Gela.Lexical_Types.Symbol;

   subtype First_Symbols is Gela.Lexical_Types.Symbol range 0 .. 19;

   subtype Operator_Symbol is Gela.Lexical_Types.Symbol range 1 .. 19;

   subtype Simple_Symbol is Gela.Lexical_Types.Symbol
     range 1 .. 16#7FFF_FFFF#;

   subtype Compound_Symbol is Gela.Lexical_Types.Symbol
     range 16#8000_0000# .. 16#BFFF_FFFF#;

   --  Symbol list with code in 1 .. BFFF_FFFF represent single item list
   --  with corresponding code. Such list never sstored in sets.
   subtype List_Symbol is Gela.Lexical_Types.Symbol_List
     range 16#C000_0000# .. 16#FFFF_FFFF#;

   type List_Index is range 1 .. 16#3FFF_FFFF#;

   package String_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => Gela.Lexical_Types.Symbol,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=",
      "="             => Gela.Lexical_Types."=");

   type Symbol_Value is record
      Original : League.Strings.Universal_String;
      Folded   : League.Strings.Universal_String;
   end record;

   package Symbol_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Symbol_Index,
      Element_Type => Symbol_Value);

   type List_Node is record
      Left  : Gela.Lexical_Types.Symbol;
      Right : Gela.Lexical_Types.Symbol;
   end record;

   function Hash (Item : List_Node) return Ada.Containers.Hash_Type;

   package List_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => List_Node,
      Element_Type    => List_Index,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   package List_Vectors is new Ada.Containers.Vectors
     (Index_Type   => List_Index,
      Element_Type => List_Node);

   type String_Array is
     array (Operator_Symbol) of League.Strings.Universal_String;

   type Symbol_Set is new Gela.Symbol_Sets.Symbol_Set with record
      Original : String_Maps.Map;
      Folded   : String_Maps.Map;
      Values   : Symbol_Vectors.Vector;
      List_Map : List_Maps.Map;
      Lists    : List_Vectors.Vector;
      Operator : String_Array;
   end record;

end Gela.Plain_Symbol_Sets;
