with League.Strings;

with Gela.Lexical_Types;

package Gela.Symbol_Sets is
   pragma Preelaborate;

   type Symbol_Set is limited interface;
   type Symbol_Set_Access is access all Symbol_Set'Class;
   for Symbol_Set_Access'Storage_Size use 0;

   not overriding function Get
     (Self  : Symbol_Set;
      Image : League.Strings.Universal_String)
      return Gela.Lexical_Types.Symbol is abstract;
   --  Return No_Symbol if there is no such symbol in the set.

   not overriding procedure Fetch
     (Self  : in out Symbol_Set;
      Image : League.Strings.Universal_String;
      Value : out Gela.Lexical_Types.Symbol) is abstract;
   --  Get symbol if already in the set or create new one otherwise.

   not overriding function Image
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return League.Strings.Universal_String is abstract;
   --  Original image used in Fetch to create given symbol

   not overriding function Folded
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return League.Strings.Universal_String is abstract;
   --  Image after applying locale-independent simple case folding

   not overriding procedure Join
     (Self  : in out Symbol_Set;
      Left  : Gela.Lexical_Types.Symbol;
      Right : Gela.Lexical_Types.Symbol;
      Value : out Gela.Lexical_Types.Symbol) is abstract;
   --  Fetch symbol corresponding to Left.Right compound name.

   not overriding function Prefix
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return Gela.Lexical_Types.Symbol is abstract;
   --  Return prefix if Value is compound name or No_Symbol otherwise.

   not overriding function Selector
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol)
      return Gela.Lexical_Types.Symbol is abstract;
   --  Return selector if Value is compound name or Value otherwise.

   not overriding procedure Create_List
     (Self  : in out Symbol_Set;
      Head  : Gela.Lexical_Types.Symbol_List :=
        Gela.Lexical_Types.Empty_Symbol_List;
      Tail  : Gela.Lexical_Types.Symbol;
      Value : out Gela.Lexical_Types.Symbol_List) is abstract;
   --  Get new symbol list as join of Head and Tail.

   not overriding procedure Create_List
     (Self  : in out Symbol_Set;
      Head  : Gela.Lexical_Types.Symbol_List;
      Tail  : Gela.Lexical_Types.Symbol_List;
      Value : out Gela.Lexical_Types.Symbol_List) is abstract;
   --  Get new symbol list as join of Head and Tail.

   not overriding function Head
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol_List)
      return Gela.Lexical_Types.Symbol_List is abstract;
   --  Return head of Value.

   not overriding function Tail
     (Self  : Symbol_Set;
      Value : Gela.Lexical_Types.Symbol_List)
      return Gela.Lexical_Types.Symbol is abstract;
   --  Return tail symbol of Value.

end Gela.Symbol_Sets;
