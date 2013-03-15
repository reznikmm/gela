------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Types;

package Gela.Mutables.Elements is

   type Element (Compilation : Mutable_Compilation_Access) is
     abstract tagged null record;

   function Size
     (Self    : access Element;
      Payload : Gela.Types.Payload) return Natural is abstract;

   function Tag
     (Self    : access Element;
      Payload : Gela.Types.Payload) return Natural;

   procedure Set_Tag
     (Self    : access Element;
      Payload : Gela.Types.Payload;
      Value   : Natural);

   function Count
     (Self    : access Element;
      Payload : Gela.Types.Payload) return Natural;

   procedure Set_Count
     (Self    : access Element;
      Payload : Gela.Types.Payload;
      Value   : Natural);

   function Free_List_Link
     (Self    : access Element;
      Payload : Gela.Types.Payload) return Gela.Types.Payload;

   procedure Set_Free_List_Link
     (Self    : access Element;
      Payload : Gela.Types.Payload;
      Value   : Gela.Types.Payload);

   function Last_Child
     (Self    : access Element;
      Payload : Gela.Types.Payload) return Natural is abstract;

   function Child
     (Self    : access Element;
      Payload : Gela.Types.Payload;
      Index   : Positive) return Gela.Types.Payload is abstract;

   procedure Set_Child
     (Self    : access Element;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Types.Payload) is abstract;

end Gela.Mutables.Elements;
