------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Types;

package Gela.Compilation_Unit_Lists is

   type Abstract_Compilation_Unit_List is limited interface;

   function Units_Count
     (Self    : access Abstract_Compilation_Unit_List;
      Payload : Gela.Types.Payload)
      return Natural is abstract;

   function First
     (Self    : access Abstract_Compilation_Unit_List;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_Cursor is abstract;

   type Abstract_Compilation_Unit_Cursor is interface;

   function Element
     (Self    : access Abstract_Compilation_Unit_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit is abstract;

   function Next
     (Self    : access Abstract_Compilation_Unit_Cursor;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_Cursor is abstract;

end Gela.Compilation_Unit_Lists;