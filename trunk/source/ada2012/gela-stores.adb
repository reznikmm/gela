------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Gela.Stores is

   procedure Free is new
     Ada.Unchecked_Deallocation (Element_Array, Element_Array_Access);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Self : in out Store;
      Size : Natural) return Index
   is
      Length : constant Index := Index (Size);
      Result : Index;
   begin
      if Self.Free_List /= null and then
        Length in Self.Free_List'Range and then
        Self.Free_List (Length) /= 0
      then
         Result := Index (Self.Free_List (Length));
         Self.Free_List (Length) := Self.Get (Result);

         --  Cleanup reused space
         for J in Result .. Result + Length - 1 loop
            Self.Set (J, 0);
         end loop;
      else
         Result := Self.Last;
         Self.Last := Self.Last + Length;
      end if;

      return Result;
   end Allocate;

   ----------
   -- Free --
   ----------

   procedure Free
     (Self    : in out Store;
      Element : Index;
      Size    : Natural)
   is
      Length : constant Index := Index (Size);
   begin
      if Self.Free_List = null or else Length not in Self.Free_List'Range then
         declare
            Saved : Element_Array_Access := Self.Free_List;
         begin
            Self.Free_List := new Element_Array (4 .. Length);

            if Saved = null then
               Self.Free_List.all := (others => 0);
            else
               Self.Free_List (Saved'Range) := Saved.all;
               Self.Free_List (Saved'Last + 1 .. Self.Free_List'Last) :=
                 (others => 0);
               Free (Saved);
            end if;
         end;
      end if;

      Self.Set (Element, Self.Free_List (Length));
      Self.Free_List (Length) := Gela.Stores.Element (Element);
   end Free;

   ---------
   -- Get --
   ---------

   function Get
     (Self     : Store;
      Position : Index) return Element is
   begin
      return Self.Data (Position);
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self     : in out Store;
      Position : Index;
      Value    : Element) is
   begin
      if Self.Data = null then
         Self.Data := new Element_Array'(1 .. 256 => 0);
      elsif Position > Self.Data'Last then
         declare
            Saved : Element_Array_Access := Self.Data;
         begin
            Self.Data := new Element_Array (1 .. 2 * Saved'Last);
            Self.Data (Saved'Range) := Saved.all;
            Self.Data (Saved'Last + 1 .. Self.Data'Last) := (others => 0);
            Free (Saved);
         end;
      end if;

      Self.Data (Position) := Value;
   end Set;

end Gela.Stores;
