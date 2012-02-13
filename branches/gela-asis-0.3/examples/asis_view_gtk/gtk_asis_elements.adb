with Asis.Elements;
with Asis.Text;
with Asis.Compilation_Units;
with Ada.Containers.Vectors;
with Ada.Characters.Handling;
with Glib.Object;
with Gtk.Button;
with Gtk.GEntry;
with Gtk.Combo_Box;
with Gtk.Handlers;

with Asis_View_GTK;
With XASIS.Utils;

package body GTK_Asis_Elements is

   function To_String (Element : Asis.Element) return String;
   function To_String (X : Asis.Text.Span) return String;
   function To_String (Unit : Asis.Compilation_Unit) return String;

   package Entry_Callback is
      new Gtk.Handlers.User_Callback (Gtk.Button.Gtk_Button_Record,
                                      Gtk.GEntry.Gtk_GEntry);

   package Combo_Callback is
      new Gtk.Handlers.User_Callback (Gtk.Button.Gtk_Button_Record,
                                      Gtk.Combo_Box.Gtk_Combo_Box);
   package Tips_Callback is
      new Gtk.Handlers.User_Callback (Gtk.Combo_Box.Gtk_Combo_Box_Record,
                                      Gtk.Tooltips.Gtk_Tooltips);

   procedure Go_To_Element
     (Button : access Gtk.Button.Gtk_Button_Record'Class;
      Widget : in     Gtk.GEntry.Gtk_GEntry);

   procedure Go_To_List_Element
     (Button : access Gtk.Button.Gtk_Button_Record'Class;
      Widget : in     Gtk.Combo_Box.Gtk_Combo_Box);

   procedure Combo_Changed
     (Widget : access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
      Tips   : in     Gtk.Tooltips.Gtk_Tooltips);

   package Asis_Data is new Glib.Object.User_Data (Asis.Element);

   package Vectors is new Ada.Containers.Vectors
     (Asis.List_Index, Asis.Element, Asis.Elements.Is_Equal);

   package Vector_Data is new Glib.Object.User_Data (Vectors.Vector);

   procedure Clear (Widget : Gtk.Combo_Box.Gtk_Combo_Box);

   procedure Create_Frames (Root   : in  GTK_Asis_Element) is separate;

   function Get_Element
     (Widget : Gtk.Combo_Box.Gtk_Combo_Box)
     return Asis.Element;

   -----------
   -- Clear --
   -----------

   procedure Clear (Widget : Gtk.Combo_Box.Gtk_Combo_Box) is
      Vector : constant Vectors.Vector := Vector_Data.Get (Widget);
      Length : constant Ada.Containers.Count_Type := Vector.Length;
   begin
      for J in 1 .. Length loop
         Gtk.Combo_Box.Remove_Text (Widget, 0);
      end loop;
   end Clear;

   procedure Show
     (Frame   : out GTK_Asis_Element;
      Element : in  Asis.Element)
     is separate;

   -------------------
   -- Combo_Changed --
   -------------------

   procedure Combo_Changed
     (Widget : access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
      Tips   : in     Gtk.Tooltips.Gtk_Tooltips)
   is
      use Ada.Characters.Handling;
      Combo : Gtk.Combo_Box.Gtk_Combo_Box :=
        Gtk.Combo_Box.Gtk_Combo_Box (Widget);
      Item  : constant Asis.Element := Get_Element (Combo);
   begin
      Gtk.Tooltips.Set_Tip (Tips,
                            Gtk.Combo_Box.Get_Parent (Widget),
                            To_String (XASIS.Utils.Debug_Image (Item)));
   end Combo_Changed;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element
     (Widget : Gtk.Combo_Box.Gtk_Combo_Box)
     return Asis.Element
   is
      use type Glib.Gint;

      Vector  : constant Vectors.Vector := Vector_Data.Get (Widget);
      Active  : constant Glib.Gint := Gtk.Combo_Box.Get_Active (Widget);
   begin
      if Active >= 0 then
         return Vector.Element (Asis.List_Index (Active + 1));
      else
         return Asis.Nil_Element;
      end if;
   end Get_Element;

   procedure Gtk_New
     (Frame : out GTK_Asis_Element;
      Label : in  GLib.UTF8_String := "")
   is
      use Gtk.Frame;
   begin
      Frame := new GTK_Asis_Element_Record;
      Initialize (Frame, Label);
      Gtk.Size_Group.Gtk_New (Frame.Size);
      Gtk.Size_Group.Gtk_New (Frame.Size_2);
      Gtk.Tooltips.Gtk_New (Frame.Tips);
      Create_Frames (Frame);
   end Gtk_New;

   procedure Go_To_Element
     (Button : access Gtk.Button.Gtk_Button_Record'Class;
      Widget : in     Gtk.GEntry.Gtk_GEntry)
   is
      Element : constant Asis.Element := Asis_Data.Get (Widget);
   begin
      Asis_View_GTK.Go_To_Element (Element);
   end Go_To_Element;

   procedure Go_To_List_Element
     (Button : access Gtk.Button.Gtk_Button_Record'Class;
      Widget : in     Gtk.Combo_Box.Gtk_Combo_Box)
   is
   begin
      Asis_View_GTK.Go_To_Element (Get_Element (Widget));
   end Go_To_List_Element;

   function To_String (X : Asis.Text.Span) return String is
      use Asis.Text;
      function Img (X : Asis.ASIS_Natural) return String is
         Image   : constant String := Asis.ASIS_Natural'Image (X);
      begin
         return Image (2 .. Image'Last);
      end Img;

   begin
      return " [" & Img (X.First_Line)
        & ":" & Img (X.First_Column)
        & ".." & Img (X.Last_Line)
        & ":" & Img (X.Last_Column)
        & "]";
   end To_String;

   function To_String (Element : Asis.Element) return String is
   begin
      return '[' &
        Asis.Element_Kinds'Image (Asis.Elements.Element_Kind (Element)) &
        ']';
   end To_String;

   function To_String (Unit : Asis.Compilation_Unit) return String is
      use Asis.Compilation_Units;
      use Ada.Characters.Handling;
   begin
      return To_String (Unit_Full_Name (Unit));
   end To_String;

end GTK_Asis_Elements;
