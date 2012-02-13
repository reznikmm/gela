with GLib;
with Gtk.Frame;
with Gtk.Size_Group;
with Gtk.Tooltips;
with GTK_Asis_Element_Frames;

with Asis;

package GTK_Asis_Elements is

   type GTK_Asis_Element_Record is new Gtk.Frame.Gtk_Frame_Record with private;

   type GTK_Asis_Element is access all GTK_Asis_Element_Record'Class;

   procedure Gtk_New
     (Frame : out GTK_Asis_Element;
      Label : in  GLib.UTF8_String := "");

   procedure Show
     (Frame   : out GTK_Asis_Element;
      Element : in  Asis.Element);

private

   type GTK_Asis_Element_Record is
     new Gtk.Frame.Gtk_Frame_Record with record
        Frames : GTK_Asis_Element_Frames.GTK_Asis_Element_Frame;
        Size   : Gtk.Size_Group.Gtk_Size_Group;
        Size_2 : Gtk.Size_Group.Gtk_Size_Group;
        Tips   : Gtk.Tooltips.Gtk_Tooltips;
     end record;

end GTK_Asis_Elements;
