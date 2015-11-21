with League.Strings;

with Gela.Symbols;

limited with Gela.A4G.Contexts;
private with System;
private with Ada.Containers.Ordered_Maps;

package Gela.A4G.Symbols is

   type Symbol is new Gela.Symbols.Symbol with private;
   type Symbol_Access is access all Symbol'Class;

   function Create
     (Context : not null access Gela.A4G.Contexts.Context'Class;
      Image   : League.Strings.Universal_String;
      Folded  : League.Strings.Universal_String)
       return Gela.Symbols.Symbol_Access;

private
   package List_Maps is new Ada.Containers.Ordered_Maps
     (System.Address,
      Gela.Symbols.Symbol_List_Access,
      System."<",
      Gela.Symbols."=");

   type Symbol is new Gela.Symbols.Symbol with record
      Context : not null access Gela.A4G.Contexts.Context'Class;
      Image   : League.Strings.Universal_String;
      Folded  : League.Strings.Universal_String;
      Lists   : List_Maps.Map;
   end record;

   overriding function Image
     (Self  : access Symbol) return League.Strings.Universal_String;

   overriding function Folded
     (Self  : access Symbol) return League.Strings.Universal_String;

   overriding function Append
     (Left  : access Symbol;
      Right : Gela.Symbols.Symbol_Access) return Gela.Symbols.Symbol_Access;

   overriding function Prefix
     (Self  : access Symbol) return Gela.Symbols.Symbol_Access;

   overriding function Selector
     (Self  : access Symbol) return Gela.Symbols.Symbol_Access;

   overriding function Create_List
     (Head  : access Symbol;
      Tail  : Gela.Symbols.Symbol_List_Access := null)
        return Gela.Symbols.Symbol_List_Access;

   type Compound_Symbol is new Symbol with record
      Prefix   : Gela.Symbols.Symbol_Access;
      Selector : Gela.Symbols.Symbol_Access;
   end record;

   overriding function Prefix
     (Self  : access Compound_Symbol) return Gela.Symbols.Symbol_Access;

   overriding function Selector
     (Self  : access Compound_Symbol) return Gela.Symbols.Symbol_Access;

   type Symbol_List is new Gela.Symbols.Symbol_List with record
      Head : Gela.Symbols.Symbol_Access;
      Tail : Gela.Symbols.Symbol_List_Access;
   end record;

   type Symbol_List_Access is access all Symbol_List'Class;

   overriding function Append
     (Head  : access Symbol_List;
      Tail  : Gela.Symbols.Symbol_List_Access)
        return Gela.Symbols.Symbol_List_Access;

   overriding function Head
     (Self  : access Symbol_List) return Gela.Symbols.Symbol_Access;

   overriding function Tail
     (Self  : access Symbol_List) return Gela.Symbols.Symbol_List_Access;

end Gela.A4G.Symbols;
