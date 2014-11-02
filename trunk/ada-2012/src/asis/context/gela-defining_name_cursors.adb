------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body Gela.Defining_Name_Cursors is

   -------------
   -- Element --
   -------------

   function Element (Self : Defining_Name_Cursor)
      return Gela.Nodes.Defining_Name is
   begin
      return Self.This.Element (Self.Payload);
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : Defining_Name_Cursor) return Boolean is
   begin
      return Self.This /= null;
   end Has_Element;

   ----------
   -- Next --
   ----------

   function Next (Self : Defining_Name_Cursor) return Defining_Name_Cursor is
   begin
      return Self.This.Next (Self.Payload);
   end Next;

end Gela.Defining_Name_Cursors;
