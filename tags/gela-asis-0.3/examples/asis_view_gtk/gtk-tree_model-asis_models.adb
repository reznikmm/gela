with System.Address_To_Access_Conversions;

with Asis.Iterator;
with Asis.Compilation_Units;
with Ada.Characters.Handling;

with XASIS.Utils;

with Ada.Text_IO;

package body Gtk.Tree_Model.Asis_Models is
   use type Ada.Containers.Count_Type;

   function To_Iter (Item : Asis_Element_Access) return Gtk_Tree_Iter;
   function To_Item (Iter : Gtk_Tree_Iter) return Asis_Element_Access;

   package Model_Address is
      new System.Address_To_Access_Conversions (Asis_Element);

   GTK_Type : GType := GType_Invalid;

   ---------
   -- Add --
   ---------

   procedure Add
     (Model   : access Gtk_Asis_Model_Record;
      Element : in     Asis.Element)
   is

      procedure Add_Element
        (Parent  : in     Asis_Element_Access;
         Element : in     Asis.Element);

      procedure Add_Unit (Unit  : in     Asis.Compilation_Unit);

      function Get_Unit
        (Unit : Asis.Compilation_Unit) return Asis_Element_Access;

      function Get_Element (Element : Asis.Element) return Asis_Element_Access;

      -----------------
      -- Add_Element --
      -----------------

      procedure Add_Element
        (Parent  : in     Asis_Element_Access;
         Element : in     Asis.Element)
      is
         Item : constant Asis_Element_Access := new Asis_Element (False);
         Iter : Gtk_Tree_Iter := To_Iter (Item);
         Path : Gtk_Tree_Path;
      begin
         Item.Level   := Parent.Level + 1;
         Item.Index   := Positive (Parent.Children.Length + 1);
         Item.Parent  := Parent;
         Item.Element := Element;

         Parent.Children.Append (Item);
         Model.Map.Insert (Element, Item);
         Path := Model.Get_Path (Iter);
         Model.Row_Inserted (Path, Iter);
         Path_Free (Path);
      end Add_Element;

      --------------
      -- Add_Unit --
      --------------

      procedure Add_Unit (Unit  : in     Asis.Compilation_Unit) is
         Item : constant Asis_Element_Access := new Asis_Element (True);
         Iter : Gtk_Tree_Iter := To_Iter (Item);
         Path : Gtk_Tree_Path;
      begin
         Item.Level   := 1;
         Item.Index   := Positive (Model.Root.Children.Length + 1);
         Item.Parent  := Model.Root;
         Item.Unit    := Unit;

         Model.Root.Children.Append (Item);
         Path := Model.Get_Path (Iter);
         Model.Row_Inserted (Path, Iter);
         Path_Free (Path);
      end Add_Unit;

      -----------------
      -- Get_Element --
      -----------------

      function Get_Element
        (Element : Asis.Element) return Asis_Element_Access
      is
         Pos : Hashed_Maps.Cursor := Model.Map.Find (Element);
      begin
         if Hashed_Maps.Has_Element (Pos) then
            return Hashed_Maps.Element (Pos);
         else
            return null;
         end if;
      end Get_Element;

      --------------
      -- Get_Unit --
      --------------

      function Get_Unit
        (Unit : Asis.Compilation_Unit) return Asis_Element_Access
      is
         use Vectors;
         use Asis.Compilation_Units;

         Found : Asis_Element_Access;

         procedure Find (Position : Cursor) is
            Item : Asis_Element_Access := Vectors.Element (Position);
         begin
            if Is_Equal (Item.Unit, Unit) then
               Found := Item;
            end if;
         end;
      begin
         Iterate (Model.Root.Children, Find'Access);

         return Found;
      end Get_Unit;

      Parent  : Asis_Element_Access;

      -------------------
      -- Pre_Operation --
      -------------------

      procedure Pre_Operation
        (Current :        Asis.Element;
         Control : in out Asis.Traverse_Control;
         State   : in out Integer)
      is
         use Asis.Elements;
         Aaa  : Asis_Element_Access;
      begin
         if Get_Element (Current) /= null then
            null;  --  already in model
         elsif Is_Equal (Current, Element) then
            Add_Element (Parent, Current);
         else
            Aaa := Get_Element (Enclosing_Element (Current));
            if Aaa = null then
               Ada.Text_IO.Put_Line
                 ("???:" & Ada.Characters.Handling.To_String (XASIS.Utils.Debug_Image (Current)));
            else
               Add_Element (Aaa, Current);
            end if;
         end if;
      end Pre_Operation;

      --------------------
      -- Post_Operation --
      --------------------

      procedure Post_Operation
        (Element :        Asis.Element;
         Control : in out Asis.Traverse_Control;
         State   : in out Integer)
      is
      begin
         null;
      end Post_Operation;

      procedure Add_Tree is new Asis.Iterator.Traverse_Element (Integer);

      Enclose : constant Asis.Element :=
        Asis.Elements.Enclosing_Element (Element);
      Unit    : constant Asis.Compilation_Unit :=
        Asis.Elements.Enclosing_Compilation_Unit (Element);
      Control : Asis.Traverse_Control := Asis.Continue;
      State   : Integer := 0;
   begin
      if Asis.Elements.Is_Nil (Enclose) then
         Parent := Get_Unit (Unit);

         if Parent = null then
            Add_Unit (Unit);
            Parent := Get_Unit (Unit);
         end if;
      else
         Parent := Get_Element (Enclose);

         if Parent = null then
            Add (Model, Enclose);
            Parent := Get_Element (Enclose);
         end if;
      end if;

      Add_Tree (Element, Control, State);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Model   : access Gtk_Asis_Model_Record;
      Unit    : in     Asis.Compilation_Unit)
   is
      Decl : constant Asis.Element := Asis.Elements.Unit_Declaration (Unit);
      Clauses   : constant Asis.Element_List :=
        Asis.Elements.Context_Clause_Elements (Unit, True);
   begin
      for I in Clauses'Range loop
         Add (Model, Clauses (I));
      end loop;

      if not Asis.Elements.Is_Nil (Decl) then
         Add (Model, Decl);
      end if;
   end Add;

   --------------
   -- Children --
   --------------

   function Children
     (Model  : access Gtk_Asis_Model_Record;
      Parent : in     Gtk_Tree_Iter)
      return Gtk_Tree_Iter
   is
      Item : constant Asis_Element_Access := To_Item (Parent);
   begin
      if Item /= null and then Item.Children.Length > 0 then
         return To_Iter (Item.Children.Element (1));
      else
         return Null_Iter;
      end if;
   end Children;

   ---------
   -- Get --
   ---------

   function Get
     (Model : access Gtk_Asis_Model_Record;
      Path  : in     Gtk_Tree_Path)
     return Asis.Element
   is
      Iter : constant Gtk_Tree_Iter := Get_Iter (Model, Path);
      Item : constant Asis_Element_Access := To_Item (Iter);
   begin
      if Item /= null and then not Item.Is_Unit then
         return Item.Element;
      else
         return Asis.Nil_Element;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Model : access Gtk_Asis_Model_Record;
      Iter  : in     Gtk_Tree_Iter)
     return Asis.Element
   is
      Item : constant Asis_Element_Access := To_Item (Iter);
   begin
      if Item /= null and then not Item.Is_Unit then
         return Item.Element;
      else
         return Asis.Nil_Element;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Model   : access Gtk_Asis_Model_Record;
      Element : in     Asis.Element)
     return Gtk_Tree_Iter
   is
      Pos : Hashed_Maps.Cursor := Model.Map.Find (Element);
   begin
      if Hashed_Maps.Has_Element (Pos) then
         return To_Iter (Hashed_Maps.Element (Pos));
      else
         return Null_Iter;
      end if;
   end Get;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   function Get_Column_Type
     (Model : access Gtk_Asis_Model_Record;
      Index : in     Gint)
      return GType
   is
   begin
      return GType_String;
   end Get_Column_Type;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
     (Model : access Gtk_Asis_Model_Record)
      return Tree_Model_Flags
   is
   begin
      return Tree_Model_Iters_Persist;
   end Get_Flags;

   --------------
   -- Get_Iter --
   --------------

   function Get_Iter
     (Model : access Gtk_Asis_Model_Record;
      Path  : in     Gtk_Tree_Path)
      return Gtk_Tree_Iter
   is
      Indices : constant GInt_Array := Get_Indices (Path);
      Result  : Gtk_Tree_Iter := To_Iter (Model.Root);
   begin
      if Indices'Length = 0 then
         return Null_Iter;
      end if;

      for J in Indices'Range loop
         Result := Nth_Child (Model, Result, Indices (J));
      end loop;

      return Result;
   end Get_Iter;

   -------------------
   -- Get_N_Columns --
   -------------------

   function Get_N_Columns
     (Model : access Gtk_Asis_Model_Record)
      return Gint
   is
   begin
      return 1;
   end Get_N_Columns;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
     (Model : access Gtk_Asis_Model_Record;
      Iter  : in     Gtk_Tree_Iter)
      return Gtk_Tree_Path
   is
      Path : Gtk_Tree_Path := null;
      Item : Asis_Element_Access := To_Item (Iter);
   begin
      if Item /= null then
         Path := Gtk_New;
         while Item.Level > 0 loop
            Prepend_Index (Path, Gint (Item.Index - 1));
            Item := Item.Parent;
         end loop;
      end if;

      return Path;
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   procedure Get_Value
     (Model  : access Gtk_Asis_Model_Record;
      Iter   : in     Gtk_Tree_Iter;
      Column : in     Gint;
      Value  :    out Glib.Values.GValue)
   is
      use Glib.Values;
      use Asis.Compilation_Units;
      use Ada.Characters.Handling;

      Item : constant Asis_Element_Access := To_Item (Iter);
   begin
      Init (Value, GType_String);

      if Item = null then
         Set_String (Value, "<null>");
      elsif Item.Is_Unit then
         Set_String (Value, To_String (Unit_Full_Name (Item.Unit)));
      else
         Set_String (Value,
                     Asis.Element_Kinds'Image
                      (Asis.Elements.Element_Kind (Item.Element)));
      end if;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Model : out Gtk_Asis_Model) is
   begin
      if GTK_Type = GType_Invalid then
         GTK_Type := Abstract_Store.Register ("GtkAsisModel");
      end if;

      Model := new Gtk_Asis_Model_Record;
      Model.Root := new Asis_Element (False);
      Model.Root.Level   := 0;
      Model.Root.Index   := 1;
      Model.Root.Parent  := null;
      Model.Root.Element := Asis.Nil_Element;

      Abstract_Store.Initialize (Model, GTK_Type);
   end Gtk_New;

   ---------------
   -- Has_Child --
   ---------------

   function Has_Child
     (Model : access Gtk_Asis_Model_Record;
      Iter  : in     Gtk_Tree_Iter)
      return Boolean
   is
      Item : constant Asis_Element_Access := To_Item (Iter);
   begin
      return Item /= null and then Item.Children.Length > 0;
   end Has_Child;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Asis.Element) return Ada.Containers.Hash_Type is
      use type Asis.ASIS_Integer;
      Hash : Asis.ASIS_Integer := Asis.Elements.Hash (Item);
   begin
      return Ada.Containers.Hash_Type (abs Hash);
   end Hash;

   ----------
   -- Next --
   ----------

   procedure Next
     (Model : access Gtk_Asis_Model_Record;
      Iter  : in out Gtk_Tree_Iter)
   is
      Item : constant Asis_Element_Access := To_Item (Iter);
   begin
      if Item /= null and then
        Item.Parent.Children.Length > Ada.Containers.Count_Type (Item.Index)
      then
         Iter := To_Iter (Item.Parent.Children.Element (Item.Index + 1));
      else
         Iter := Null_Iter;
      end if;
   end Next;

   ---------------
   -- Nth_Child --
   ---------------

   function Nth_Child
     (Model  : access Gtk_Asis_Model_Record;
      Parent : in     Gtk_Tree_Iter;
      N      : in     Gint)
      return Gtk_Tree_Iter
   is
      Item : constant Asis_Element_Access := To_Item (Parent);
   begin
      if Item /= null and then
        Item.Children.Length >= Ada.Containers.Count_Type (N + 1)
      then
         return To_Iter (Item.Children.Element (Positive (N + 1)));
      else
         return Null_Iter;
      end if;
   end Nth_Child;

   ----------------
   -- N_Children --
   ----------------

   function N_Children
     (Model : access Gtk_Asis_Model_Record;
      Iter  : in     Gtk_Tree_Iter := Null_Iter)
      return Gint
   is
      Item : constant Asis_Element_Access := To_Item (Iter);
   begin
      if Item /= null then
         return Gint (Item.Children.Length);
      else
         return 0;
      end if;
   end N_Children;

   ------------
   -- Parent --
   ------------

   function Parent
     (Model : access Gtk_Asis_Model_Record;
      Child : in     Gtk_Tree_Iter)
      return Gtk_Tree_Iter
   is
      Item : constant Asis_Element_Access := To_Item (Child);
   begin
      if Item /= null and then Item.Level > 1 then
         return To_Iter (Item.Parent);
      else
         return Null_Iter;
      end if;
   end Parent;

   -------------
   -- To_Item --
   -------------

   function To_Item (Iter : Gtk_Tree_Iter) return Asis_Element_Access is
      use Model_Address;
   begin
      return Asis_Element_Access (To_Pointer (Iter.User_Data));
   end To_Item;

   -------------
   -- To_Iter --
   -------------

   function To_Iter (Item : Asis_Element_Access) return Gtk_Tree_Iter is
      use Model_Address;
   begin
      return (Stamp => 1,
              User_Data => To_Address (Object_Pointer (Item)),
              others => <>);
   end To_Iter;

end Gtk.Tree_Model.Asis_Models;
