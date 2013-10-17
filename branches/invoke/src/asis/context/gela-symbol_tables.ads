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
with Gela.Defining_Name_Cursors;

package Gela.Symbol_Tables is
   pragma Preelaborate;

   type Abstract_Symbol_Table is limited interface;

   not overriding function Direct_Visible
     (Self    : access Abstract_Symbol_Table;
      Payload : Gela.Types.Payload;
      Symbol  : Gela.Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor is abstract;
   --  Get list of direct visible declaration of given symbol

   not overriding function Visible
     (Self    : access Abstract_Symbol_Table;
      Payload : Gela.Types.Payload;
      Region  : Gela.Nodes.Declarative_Region;
      Symbol  : Gela.Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor is abstract;
   --  Get list of visible declaration of given symbol inside Region

   not overriding function Denote
     (Self      : access Abstract_Symbol_Table;
      Payload   : Gela.Types.Payload;
      Type_Name : Gela.Nodes.Defining_Name)
      return Gela.Nodes.Defining_Name is abstract;
   --  For type with given Type_Name return latest completion declaration

--     not overriding procedure Copy
--       (Self     : in out Abstract_Symbol_Table;
--        Payload  : Gela.Types.Payload;
--        Target   : out Gela.Types.Symbol_Table) is abstract;
--   --  Create copy on write of Self into Target. After this any modification
--     --  of Self or Target will actually create new copy
--
--     not overriding procedure Append
--       (Self    : in out Abstract_Symbol_Table;
--        Payload : in out Gela.Types.Payload;
--        Name    : Gela.Types.Symbol;
--        Value   : Gela.Types.Defining_Name) is abstract;

   type Abstract_Symbol_Table_Access is access all Abstract_Symbol_Table'Class;

   type Symbol_Table (This : Abstract_Symbol_Table_Access) is record
      Payload : Gela.Types.Payload;
   end record;

end Gela.Symbol_Tables;
