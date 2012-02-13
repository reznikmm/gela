with Glib;
with Glib.Values;
with Glib.Properties;
with Gtk.Box;
with Gdk.Main;
with Gtk.Main;
with Gtk.Label;
with Gtk.Enums;
with Gtk.Paned;
with Gtk.Widget;
with Gtk.Window;
with Gtk.Toolbar;
with Gtk.Handlers;
with Gtk.Text_View;
with Gtk.Tree_View;
with Gtk.Tree_Store;
with Gtk.Status_Bar;
with Gtk.Text_Buffer;
with Gtk.Tool_Button;
with Gtk.Tree_Selection;
with Gtk.Scrolled_Window;
with Gtk.Text_Iter;
with Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model.Asis_Models;
with Pango.Font;

with GTK_Asis_Elements;
with Gtkada.Handlers;

with Asis;
with Asis.Text;
--with Asis.Errors;
with Asis.Elements;
with Asis.Exceptions;
with Asis.Implementation;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Ada.Characters.Handling;  use Ada.Characters.Handling;

with XASIS.Utils;
with Ada.Exceptions;
with Ada.Wide_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Text_IO;

package body Asis_View_GTK is
   use type Ada.Containers.Count_Type;

   package Widget_Cb is new Gtk.Handlers.Callback
     (Gtk.Tree_Selection.Gtk_Tree_Selection_Record);

   procedure Handler
     (Widget : access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class);

   procedure Pop_Element (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Destroy (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Add_Unit (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   package Element_List is new Ada.Containers.Doubly_Linked_Lists
     (Asis.Element, Asis.Elements.Is_Equal);

   function Read_File (Name : String) return String;

   package Dialogs is
      function Ask_Context_Parameter return String;

      procedure Show_Error
        (Exception_Name : String;
         Diagnosis      : String);

      function Ask_Add_Unit
        (My_Context : Asis.Context) return Asis.Compilation_Unit;

   end Dialogs;

   package body Dialogs is separate;

   Window : Gtk.Window.Gtk_Window;
   Box    : Gtk.Box.Gtk_Box;
   Box2   : Gtk.Box.Gtk_Box;
   Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   Add    : Gtk.Tool_Button.Gtk_Tool_Button;
   Back   : Gtk.Tool_Button.Gtk_Tool_Button;
   Quit   : Gtk.Tool_Button.Gtk_Tool_Button;
   Store  : Gtk.Tree_Model.Asis_Models.Gtk_Asis_Model;
   View   : Gtk.Tree_View.Gtk_Tree_View;
   Elem   : GTK_Asis_Elements.GTK_Asis_Element;
   HPaned : Gtk.Paned.Gtk_Paned;
   Status : Gtk.Status_Bar.Gtk_Status_Bar;
   Context : Gtk.Status_Bar.Context_Id;
   Toolbar : Gtk.Toolbar.Gtk_Toolbar;
   Scroll2 : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   Text    : Gtk.Text_View.Gtk_Text_View;
   Buffer  : Gtk.Text_Buffer.Gtk_Text_Buffer;
   File_Name : Gtk.Label.Gtk_Label;
   A1, A2 : Gtk.Text_Iter.Gtk_Text_Iter;

   Selection : Gtk.Tree_Selection.Gtk_Tree_Selection;

   Parent : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   Render : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
   Stack  : Element_List.List;

   My_Context : Asis.Context;

   procedure Start is

      use type Glib.Gint;
      My_Context_Name       : constant Wide_String :=
        Asis.Ada_Environments.Default_Name;
      Initialization_Parameters : constant Wide_String := "";
      Finalization_Parameters   : constant Wide_String := "";

      Ignore : Glib.Gint;
      Msg    : Gtk.Status_Bar.Message_Id;
   begin
      Gtk.Main.Init;
      Gtk.Window.Gtk_New (Window);
      Gtk.Box.Gtk_New_Vbox (Box);
      Gtk.Toolbar.Gtk_New (Toolbar);
      Gtk.Tool_Button.Gtk_New_From_Stock (Add, "gtk-add");
      Gtk.Tool_Button.Set_Sensitive (Add, False);
      Gtk.Toolbar.Insert (Toolbar, Add);
      Gtk.Tool_Button.Gtk_New_From_Stock (Back, "gtk-go-back");
      Gtk.Tool_Button.Set_Sensitive (Back, False);
      Gtk.Toolbar.Insert (Toolbar, Back);
      Gtk.Tool_Button.Gtk_New_From_Stock (Quit, "gtk-quit");
      Gtk.Toolbar.Insert (Toolbar, Quit);
      Gtk.Box.Pack_Start (Box, Toolbar, False, False);
      Gtk.Paned.Gtk_New_Hpaned (HPaned);
      Gtk.Box.Pack_Start (Box, HPaned);

      Gtk.Scrolled_Window.Gtk_New (Scroll);
      Gtk.Scrolled_Window.Set_Border_Width (Scroll, 5);
      Gtk.Scrolled_Window.Set_Policy
        (Scroll,
         Gtk.Enums.Policy_Automatic,
         Gtk.Enums.Policy_Automatic);

      Gtk.Paned.Pack1 (HPaned, Scroll, True);

      Gtk.Box.Gtk_New_Vbox (Box2);
      GTK_Asis_Elements.Gtk_New (Elem, "Element");
      Gtk.Box.Pack_Start (Box2, Elem, False, False);

      Gtk.Label.Gtk_New (File_Name);
      Gtk.Box.Pack_Start (Box2, File_Name, False, False);

      Gtk.Text_View.Gtk_New (Text);
      Gtk.Text_View.Set_Editable (Text, False);
      Buffer := Gtk.Text_View.Get_Buffer (Text);
      Gtk.Text_View.Modify_Font (Text, Pango.Font.From_String ("Courier"));

      Gtk.Scrolled_Window.Gtk_New (Scroll2);
      Gtk.Scrolled_Window.Set_Border_Width (Scroll2, 5);
      Gtk.Scrolled_Window.Set_Policy
        (Scroll2,
         Gtk.Enums.Policy_Automatic,
         Gtk.Enums.Policy_Automatic);

      Gtk.Scrolled_Window.Add (Scroll2, Text);

      Gtk.Box.Pack_Start (Box2, Scroll2, True, True);

      Gtk.Paned.Pack2 (HPaned, Box2, True);

      Gtk.Tree_Model.Asis_Models.Gtk_New (Store);

      declare
         My_Context_Parameters : constant Wide_String :=
           To_Wide_String (Dialogs.Ask_Context_Parameter);
      begin
         Asis.Implementation.Initialize (Initialization_Parameters);

         Asis.Ada_Environments.Associate
           (The_Context => My_Context,
            Name        => My_Context_Name,
            Parameters  => My_Context_Parameters);

         Asis.Ada_Environments.Open         (My_Context);
      exception
         when Ex : Asis.Exceptions.ASIS_Inappropriate_Context  |
           Asis.Exceptions.ASIS_Inappropriate_Container        |
           Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
           Asis.Exceptions.ASIS_Inappropriate_Element          |
           Asis.Exceptions.ASIS_Inappropriate_Line             |
           Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
           Asis.Exceptions.ASIS_Failed                         =>

            Dialogs.Show_Error
              (Ada.Exceptions.Exception_Name (Ex),
               To_String (Asis.Implementation.Diagnosis));

            Asis.Implementation.Set_Status;

            return;
      end;

      Gtk.Tool_Button.Set_Sensitive (Add, True);

      Add_Unit (Add);

      Gtk.Tree_View.Gtk_New (View, Store);
      Selection := Gtk.Tree_View.Get_Selection (View);
      Gtk.Cell_Renderer_Text.Gtk_New (Render);

      for I in 1 .. Gtk.Tree_Model.Asis_Models.Get_N_Columns (Store) loop
         Gtk.Tree_View_Column.Gtk_New (Column);
         Gtk.Tree_View_Column.Pack_Start (Column, Render, True);
         Gtk.Tree_View_Column.Add_Attribute (Column, Render, "text", I - 1);
         Ignore := Gtk.Tree_View.Append_Column (View, Column);
      end loop;

      Gtk.Scrolled_Window.Add (Scroll, View);

      Gtk.Window.Add (Window, Box);

      Gtk.Scrolled_Window.Show_All (Scroll);
      Gtk.Toolbar.Show_All (Toolbar);
      Gtk.Paned.Show (HPaned);
      Gtk.Box.Show (Box);
      Gtk.Box.Show (Box2);
      GTK_Asis_Elements.Show (Elem);
      Gtk.Label.Show (File_Name);
      Gtk.Scrolled_Window.Show_All (Scroll2);
      Gtk.Window.Show (Window);

      Widget_Cb.Connect (Selection,
                         "changed", --Gtk.Tree_Selection.Signal_Changed,
                         Handler'Access);

      Gtk.Status_Bar.Gtk_New (Status);
      Gtk.Status_Bar.Show (Status);
      Gtk.Status_Bar.Set_Has_Resize_Grip (Status, True);
      Gtk.Box.Pack_Start (Box, Status, False, False);
      Context := Gtk.Status_Bar.Get_Context_Id (Status, "");
      Msg := Gtk.Status_Bar.Push (Status, Context, "");

      Gtkada.Handlers.Widget_Callback.Connect
        (Add, "clicked", Add_Unit'Access);

      Gtkada.Handlers.Widget_Callback.Connect
        (Back, "clicked", Pop_Element'Access);

      Gtkada.Handlers.Widget_Callback.Connect
        (Quit, "clicked", Destroy'Access);

      Gtkada.Handlers.Widget_Callback.Connect
        (Window, "destroy", Destroy'Access);

      Gtk.Main.Main;
   end;

   procedure Handler
     (Widget : access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class)
   is
      use Gtk.Tree_Model;
      Model   : Gtk_Tree_Model;
      Iter    : Gtk_Tree_Iter;
      Element : Asis.Element;
      Unit    : Asis.Compilation_Unit;
      Msg     : Gtk.Status_Bar.Message_Id;
   begin
      Gtk.Tree_Selection.Get_Selected (Widget, Model, Iter);
      Element := Gtk.Tree_Model.Asis_Models.Get (Store, Iter);
      GTK_Asis_Elements.Show (Elem, Element);
      Gtk.Status_Bar.Pop (Status, Context);
      Msg := Gtk.Status_Bar.Push
        (Status, Context, To_String (XASIS.Utils.Debug_Image (Element)));
      Stack.Append (Element);

      Gtk.Tool_Button.Set_Sensitive (Back, Stack.Length > 2);

      if not Asis.Elements.Is_Nil (Element) then
         Unit := Asis.Elements.Enclosing_Compilation_Unit (Element);

         declare
            Name : constant String :=
              To_String (Asis.Compilation_Units.Text_Name (Unit));
         begin
            if Name /= Gtk.Label.Get_Label (File_Name) then
               Gtk.Label.Set_Label (File_Name, Name);
               Gtk.Text_Buffer.Set_Text (Buffer, Read_File (Name));
            end if;
         end;

         declare
            use Asis.Text, Glib;
            Pos : constant Span := Element_Span (Element);
            Ignore : Boolean;
         begin
            if not Is_Nil (Pos) then
               Gtk.Text_Buffer.Get_Iter_At_Line_Offset
                 (Buffer, A1,
                  Gint (Pos.First_Line) - 1,
                  Gint (Pos.First_Column) - 1);

               Gtk.Text_Buffer.Get_Iter_At_Line_Offset
                 (Buffer, A2,
                  Gint (Pos.Last_Line) - 1,
                  Gint (Pos.Last_Column));

               Gtk.Text_Buffer.Select_Range (Buffer, A1, A2);
               Ignore := Gtk.Text_View.Scroll_To_Iter
                 (Text, A1, 0.0, False, 0.05, 0.0);
            end if;
         end;
      end if;
   end;

   procedure Pop_Element (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Element : Asis.Element;
   begin
      if Stack.Length > 2 then
         Stack.Delete_Last;
         Element := Stack.Last_Element;
         Stack.Delete_Last;
         Go_To_Element (Element);
      end if;
   end Pop_Element;

   procedure Destroy (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
   end Destroy;

   procedure Go_To_Element (Element : in Asis.Element) is
      use Gtk.Tree_Model;
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path;
   begin
      if Asis.Elements.Is_Nil (Element) then
         Gdk.Main.Beep;
      else
         Iter := Store.Get (Element);

         if Iter = Null_Iter then
            Store.Add (Element);
            Iter := Store.Get (Element);
         end if;

         Path := Store.Get_Path (Iter);
         View.Expand_To_Path (Path);
         View.Scroll_To_Cell (Path, null, False, 0.5, 0.0);
         Selection.Select_Iter (Iter);
         Path_Free (Path);
         GTK_Asis_Elements.Show (Elem, Element);
      end if;
   end Go_To_Element;

   function Read_File (Name : String) return String is
      use Ada.Wide_Text_IO;
      use Ada.Exceptions;
      use Ada.Strings.Unbounded;
      Input : File_Type;
      Line  : Wide_String (1 .. 80);
      Last  : Natural;
      Text  : Unbounded_String;
   begin
      if Name = "" then
         return "";
      end if;

      Open (Input, In_File, Name);

      while not End_Of_File (Input) loop
         Get_Line (Input, Line, Last);

         Text := Text & To_String (Line (1 .. Last));

         if Last /= Line'Last or End_Of_Line (Input) then
            Text := Text & ASCII.LF;
         end if;
      end loop;

      Close (Input);

      return To_String (Text);
   exception
      when E : Name_Error =>
         Dialogs.Show_Error
           (Ada.Exceptions.Exception_Name (E),
            "Read_File:'" & Name & "' " & Exception_Information (E));

         return "";
   end Read_File;

   procedure Add_Unit (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      use Asis.Compilation_Units;
      Unit : constant Asis.Compilation_Unit :=
        Dialogs.Ask_Add_Unit (My_Context);
   begin
      if not Is_Nil (Unit) then
         Store.Add (Unit);
      end if;
   end Add_Unit;

end Asis_View_GTK;
