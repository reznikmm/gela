with Asis.Gela.To;
with Asis.Gela.Elements;
with Asis.Gela.Properties;
with Asis.Gela.Text_Positions;

package body Asis.Gela.Base_Lists is
   package P renames Asis.Gela.Properties;

   function Kind_In_List
     (C    : in     Compilations.Compilation;
      Item : in Element_Index;
      List : Element_Kind_List) return Boolean;

   ---------
   -- Add --
   ---------

   procedure Add
     (C    : in     Compilations.Compilation;
      List : in     Element_Index;
      Item : in     Element_Index)
   is
      use Asis.Gela.Elements;
      use Asis.Gela.Text_Positions;

      Size     : constant ASIS_Natural  := Length (C, List);
      Tail     : constant Element_Index := Get (C, List, P.Tail);
      Item_Pos : constant Text_Position := End_Position (C, Item);
      List_Pos : constant Text_Position := Start_Position (C, List);
   begin
      if Size = 0 then
         Set (C, List, P.Tail, Item);
         Set (C, Item, P.Next_Element, Item);
         Set_Start_Position (C, List, Item);
         Set_End_Position (C, List, Item);
      elsif List_Pos < Item_Pos or else Is_Nil (Item_Pos) Then
         --  Append (List, Item)
         Set (C, Item, P.Next_Element, Get (C, Tail, P.Next_Element));
         Set (C, Tail, P.Next_Element, Item);
         Set (C, List, P.Tail, Item);
         Set_End_Position (C, List, Item);
      else
         --  Prepend (List, Item)
         Set (C, Item, P.Next_Element, Get (C, Tail, P.Next_Element));
         Set (C, Tail, P.Next_Element, Item);
         Set_Start_Position (C, List, Item);
      end if;

      Set (C, List, P.Last_Index, 0);
      Set (C, List, P.Length, Size + 1);
   end Add;

   ---------
   -- Get --
   ---------

   function Get
     (C     : Compilations.Compilation;
      List  : Element_Index;
      Index : List_Index) return Element_Index
   is
      use Asis.Gela.Elements;
      Size       : constant ASIS_Natural  := Length (C, List);
      Last_Index : ASIS_Natural := Get (C, List, P.Last_Index);
      Last_Item  : Element_Index := Get (C, List, P.Last_Item);
   begin
      if Last_Index = 0 then
         Last_Index := Size;
         Last_Item := Get (C, List, P.Tail);
      end if;

      for J in 1 .. Size loop
         if Index = Last_Index then

            if J /= 1 then
               Set (C, List, P.Last_Index, Last_Index);
               Set (C, List, P.Last_Item, Last_Item);
            end if;

            return Last_Item;
         elsif Last_Index = Size then
            Last_Index := 1;
         else
            Last_Index := Last_Index + 1;
         end if;

         Last_Item := Get (C, Last_Item, P.Next_Element);
      end loop;

      raise Constraint_Error;
   end;

   ------------------
   -- Kind_In_List --
   ------------------

   function Kind_In_List
     (C    : in     Compilations.Compilation;
      Item : in Element_Index;
      List : Element_Kind_List) return Boolean
   is
      Kind : constant Element_Kinds :=
        To.Element_Kinds (Elements.Global_Kind (C, Item));
   begin
      for I in List'Range loop
         if Kind = List (I) then
            return True;
         end if;
      end loop;

      return False;
   end Kind_In_List;

   ------------
   -- Length --
   ------------

   function Length
     (C     : Compilations.Compilation;
      List  : Element_Index) return ASIS_Natural is
   begin
      return Elements.Get (C, List, Properties.Length);
   end Length;

   -------------------
   -- Primary_Lists --
   -------------------

   package body Primary_Lists is

      ---------
      -- Add --
      ---------

      procedure Add
        (C         : Compilations.Compilation;
         Container : in     List;
         Item      : in     Element_Index)
      is
      begin
         pragma Assert (Kind_In_List (C, Item, Allowed));
         Base_Lists.Add (C, Container, Item);
      end Add;

   end Primary_Lists;

   ---------------------
   -- To_Element_List --
   ---------------------

   function To_Element_List
     (C                : Compilations.Compilation;
      List             : Element_Index;
      Unit             : Compilation_Unit;
      Include_Pragmas  : Boolean)
     return Asis.Element_List
   is
      use Asis.Gela.Elements;
      use Asis.Gela.Text_Positions;
      use type P.Global_Kinds;

      Ptr    : Element_Index := Get (C, List, P.Tail);
      Last   : ASIS_Natural := 0;
      Result : Asis.Element_List (1 .. Length (C, List));
   begin
      for J in Result'Range loop
         Ptr := Get (C, Ptr, P.Next_Element);

         if Include_Pragmas or else Global_Kind (C, Ptr) /= P.A_Pragma then
            Last := Last + 1;
            Result (Last).Index := Ptr;
            Result (Last).Unit  := Unit;
         end if;
      end loop;

      return Result (1 .. Last);
   end To_Element_List;

   ---------------------
   -- To_Element_List --
   ---------------------

   function To_Element_List
     (C    : Compilations.Compilation;
      List : Element_Index)
     return Element_Index_List
   is
      use Asis.Gela.Elements;

      Ptr    : Element_Index := Get (C, List, P.Tail);
      Result : Element_Index_List (1 .. Length (C, List));
   begin
      for J in Result'Range loop
         Ptr := Get (C, Ptr, P.Next_Element);
         Result (J) := Ptr;
      end loop;

      return Result;
   end To_Element_List;

end Asis.Gela.Base_Lists;
