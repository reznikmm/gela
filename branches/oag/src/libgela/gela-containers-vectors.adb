package body Gela.Containers.Vectors is

   ------------
   -- Arrays --
   ------------

   package body Arrays is
      --  TODO Make some optimization

      procedure Add
        (Object : in out Vector;
         Value  : in     Array_Type) is
      begin
         for J in Value'Range loop
            Add (Object, Value (J));
         end loop;
      end Add;

      procedure Get
        (Object : in     Vector;
         Index  : in     Index_Type;
         Value  :    out Array_Type) is
      begin
         for J in Value'Range loop
            Value (J) := Get (Object, Index + Index_Natural (Value'First - J));
         end loop;
      end Get;
   end Arrays;

   ---------
   -- Add --
   ---------

   procedure Add
     (Object : in out Vector;
      Item   : in     Item_Type)
   is
      Index  : Index_Natural := Object.Length + 1;
      First  : Index_Natural;
      Second : Index_Natural;
      Third  : Index_Natural;
   begin
      if Object.Saved_Part /= null then
         if Index in Object.Saved_Part'Range then
            Object.Saved_Part (Index) := Item;
         else
            Index := Index - Object.Saved_Part'Length - 1;
         end if;
      end if;

      First  := Index mod Level_Size;
      Second := (Index / Level_Size) mod Level_Size;
      Third  := Index / (Level_Size * Level_Size);

      if Object.Third_Level (Third) = null then
         Object.Third_Level (Third) := new Second_Table;
      end if;

      if Object.Third_Level (Third) (Second) = null then
         Object.Third_Level (Third) (Second) := new First_Table;
      end if;

      Object.Third_Level (Third) (Second) (First) := Item;

      Object.Length := Object.Length + 1;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Object : in out Vector;
      Item   : in     Item_Type;
      Index  :    out Index_Type)
   is
   begin
      Index := Object.Length + 1;
      Add (Object, Item);
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear (Object : in out Vector) is
   begin
      Object.Length := 0;
   end Clear;

   ---------
   -- Get --
   ---------

   function Get (Object : Vector; Index : Index_Type) return Item_Type is
      Ind    : Index_Natural := Index;
      First  : Index_Natural;
      Second : Index_Natural;
      Third  : Index_Natural;
   begin
      if Object.Saved_Part /= null then
         if Index in Object.Saved_Part'Range then
            return Object.Saved_Part (Index);
         else
            Ind := Ind - Object.Saved_Part'Length - 1;
         end if;
      end if;

      First  := Ind mod Level_Size;
      Second := (Ind / Level_Size) mod Level_Size;
      Third  := Ind / (Level_Size * Level_Size);

      return Object.Third_Level (Third) (Second) (First);
   end Get;

   ------------
   -- Length --
   ------------

   function Length (Object : Vector) return Index_Type'Base is
   begin
      return Object.Length;
   end Length;

   ---------
   -- Set --
   ---------

   procedure Set
     (Object : in out Vector;
      Index  : in     Index_Type;
      Item   : in     Item_Type)
   is
      Ind    : Index_Natural := Index;
      First  : Index_Natural;
      Second : Index_Natural;
      Third  : Index_Natural;
   begin
      if Object.Saved_Part /= null then
         if Index in Object.Saved_Part'Range then
            Object.Saved_Part (Index) := Item;
         else
            Ind := Ind - Object.Saved_Part'Length - 1;
         end if;
      end if;

      First  := Ind mod Level_Size;
      Second := (Ind / Level_Size) mod Level_Size;
      Third  := Ind / (Level_Size * Level_Size);

      Object.Third_Level (Third) (Second) (First) := Item;
   end Set;

end Gela.Containers.Vectors;
