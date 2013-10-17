------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Types;
with Gela.Nodes;

package Gela.Defining_Name_Cursors is
   pragma Preelaborate;

   type Abstract_Defining_Name_Cursor is interface;

   type Abstract_Defining_Name_Cursor_Access is
     access all Abstract_Defining_Name_Cursor'Class;

   type Defining_Name_Cursor (This : Abstract_Defining_Name_Cursor_Access) is
   record
      Payload : Gela.Types.Payload;
   end record;

   Empty : constant Defining_Name_Cursor := (null, 0);

   function Element
     (Self    : access Abstract_Defining_Name_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Nodes.Defining_Name is abstract;

   function Next
     (Self    : access Abstract_Defining_Name_Cursor;
      Payload : Gela.Types.Payload) return Defining_Name_Cursor is abstract;

   function Has_Element (Self : Defining_Name_Cursor) return Boolean;

   function Element
     (Self : Defining_Name_Cursor) return Gela.Nodes.Defining_Name;

   function Next (Self : Defining_Name_Cursor) return Defining_Name_Cursor;

   function "+" (Self : Defining_Name_Cursor)
     return Boolean renames Has_Element;
   function "+" (Self : Defining_Name_Cursor)
     return Gela.Nodes.Defining_Name renames Element;
   function "+" (Self : Defining_Name_Cursor)
     return Defining_Name_Cursor renames Next;

   pragma Inline (Has_Element);
   pragma Inline (Element);
   pragma Inline (Next);

end Gela.Defining_Name_Cursors;
