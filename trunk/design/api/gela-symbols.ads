--  This package provides Symbol interface and its methods.
with League.Strings;

package Gela.Symbols is
   pragma Preelaborate;

   type Symbol is limited interface;
   --  A symbol encountered in compilation units of some context
   type Symbol_Access is access all Symbol'Class;
   for Symbol_Access'Storage_Size use 0;

   function Assigned (Self : access Symbol'Class) return Boolean
     is (Self /= null);

   not overriding function Image
     (Self  : access Symbol) return League.Strings.Universal_String
       is abstract;
   --  Original image used in Fetch to create given symbol

   not overriding function Folded
     (Self  : access Symbol) return League.Strings.Universal_String
       is abstract;
   --  Image after applying locale-independent simple case folding

   not overriding function Append
     (Left  : access Symbol;
      Right : Gela.Symbols.Symbol_Access)
        return Gela.Symbols.Symbol_Access is abstract;
   --  Return symbol corresponding to Left.Right compound name.

   not overriding function Prefix
     (Self  : access Symbol) return Gela.Symbols.Symbol_Access is abstract;
   --  Return prefix if Self is compound name or null otherwise.

--     function Parent
--       (Self  : Symbol_Set'Class;
--        Value : Gela.Lexical_Types.Symbol)
--        return Gela.Lexical_Types.Symbol;
   --  If Value = Standard then return No_Symbol,
   --  Return Standart if Self.Prefix (Value) = No_Symbol
   --  return Self.Prefix (Value) otherwise

   not overriding function Selector
     (Self  : access Symbol) return Gela.Symbols.Symbol_Access is abstract;
   --  Return selector if Self is compound name or Self otherwise.

   type Symbol_List is limited interface;
   --  A symbol encountered in compilation units of some context
   type Symbol_List_Access is access all Symbol_List'Class;
   for Symbol_List_Access'Storage_Size use 0;

   function Assigned (Self : access Symbol_List'Class) return Boolean
     is (Self /= null);

   not overriding function Create_List
     (Head  : access Symbol;
      Tail  : Gela.Symbols.Symbol_List_Access := null)
        return Gela.Symbols.Symbol_List_Access
        is abstract;
   --  Get new symbol list as join of Head and Tail.

   not overriding function Append
     (Head  : access Symbol_List;
      Tail  : Gela.Symbols.Symbol_List_Access)
        return Gela.Symbols.Symbol_List_Access is abstract;
   --  Get new symbol list as join of Head and Tail.

   not overriding function Head
     (Self  : access Symbol_List) return Gela.Symbols.Symbol_Access
        is abstract;
   --  Return head of Self.

   not overriding function Tail
     (Self  : access Symbol_List) return Gela.Symbols.Symbol_List_Access
        is abstract;
   --  Return tail symbol of Self or null.

end Gela.Symbols;
