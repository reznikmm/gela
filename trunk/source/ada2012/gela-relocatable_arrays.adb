------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Gela.Relocatable_Arrays is

   procedure Free is new
     Ada.Unchecked_Deallocation (Element_Array, Element_Array_Access);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Buffer : in out Relocatable_Array;
      Size   : Index) return Index is
   begin
      return Result : constant Index := Buffer.Last do
         Buffer.Last := Buffer.Last + Size;
      end return;
   end Allocate;

   ---------
   -- Get --
   ---------

   function Get
     (Buffer : Relocatable_Array;
      Position : Index) return Element is
   begin
      return Buffer.Data (Position);
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Buffer   : in out Relocatable_Array;
      Position : Index;
      Value    : Element) is
   begin
      if Buffer.Data = null then
         Buffer.Data := new Element_Array'(1 .. 256 => 0);
      elsif Position > Buffer.Data'Last then
         declare
            Saved : Element_Array_Access := Buffer.Data;
         begin
            Buffer.Data := new Element_Array (1 .. 2 * Saved'Last);
            Buffer.Data (Saved'Range) := Saved.all;
            Buffer.Data (Saved'Last + 1 .. Buffer.Data'Last) := (others => 0);
            Free (Saved);
         end;
      end if;

      Buffer.Data (Position) := Value;
   end Set;

end Gela.Relocatable_Arrays;
