with Gtk.GEntry;
with Gtk.Table;
with Gtk.Dialog;
with Gtkada.Dialogs;
with Gtk.Combo_Box;
with Gtk.Radio_Button;
with Gtk.Combo_Box_Entry;

separate (Asis_View_Gtk)
package body Dialogs is

   Ask_Dialog : Gtk.Dialog.Gtk_Dialog;
   Input      : Gtk.GEntry.Gtk_Entry;
   Ask_Unit   : Gtk.Dialog.Gtk_Dialog;
   Radio1     : Gtk.Radio_Button.Gtk_Radio_Button;
   Radio2     : Gtk.Radio_Button.Gtk_Radio_Button;
   Combo1     : Gtk.Combo_Box.Gtk_Combo_Box;
   Combo2     : Gtk.Combo_Box.Gtk_Combo_Box;

   procedure Combo_Changed
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   function Ask_Context_Parameter return String is
      use Gtkada.Dialogs;
      use type Gtk.Dialog.Gtk_Dialog;

      Msg    : constant String :=
        "We are about to open and associate ASIS context. " &
        ASCII.LF &
        "Please enter parameters for Asis.Ada_Environments.Associate:";

      Box    : Gtk.Box.Gtk_Box;
      Widget : Gtk.Widget.Gtk_Widget;
      Result : Gtk.Dialog.Gtk_Response_Type;
   begin
      if Ask_Dialog = null then
         Ask_Dialog := Create_Gtk_Dialog (Msg, Confirmation, "Open context");

         Widget := Gtk.Dialog.Add_Button
           (Ask_Dialog, "gtk-ok", Gtk.Dialog.Gtk_Response_OK);

         Gtk.Dialog.Set_Default_Response
           (Ask_Dialog, Gtk.Dialog.Gtk_Response_OK);

         Widget := Gtk.Dialog.Add_Button
           (Ask_Dialog, "gtk-cancel", Gtk.Dialog.Gtk_Response_Cancel);

         Box := Gtk.Dialog.Get_Vbox (Ask_Dialog);
         Gtk.GEntry.Gtk_New (Input);
         Gtk.Box.Pack_Start (Box, Input);

         Gtk.Dialog.Show_All (Ask_Dialog);
      else
         Gtk.Dialog.Show (Ask_Dialog);
      end if;

      Result := Gtk.Dialog.Run (Ask_Dialog);

      Gtk.Dialog.Hide (Ask_Dialog);

      return Gtk.GEntry.Get_Text (Input);
   end Ask_Context_Parameter;

   procedure Show_Error
     (Exception_Name : String;
      Diagnosis      : String)
   is
      use Gtkada.Dialogs;
      Result : Message_Dialog_Buttons;
   begin
      Result := Message_Dialog
        (Diagnosis, Error, Button_OK, Title => Exception_Name);
   end Show_Error;

   function Ask_Add_Unit
     (My_Context : Asis.Context) return Asis.Compilation_Unit
   is
      use Gtkada.Dialogs;
      use type Gtk.Dialog.Gtk_Dialog;
      use type Gtk.Dialog.Gtk_Response_Type;

      Msg    : constant String :=
        "Choise Compilation Unit to view:";
      Box    : Gtk.Box.Gtk_Box;
      Widget : Gtk.Widget.Gtk_Widget;
      Result : Gtk.Dialog.Gtk_Response_Type;
      Table  : Gtk.Table.Gtk_Table;
   begin
      if Ask_Unit = null then
         Ask_Unit := Create_Gtk_Dialog (Msg, Custom, "Add unit");

         Widget := Gtk.Dialog.Add_Button
           (Ask_Unit, "gtk-ok", Gtk.Dialog.Gtk_Response_OK);

         Gtk.Dialog.Set_Default_Response
           (Ask_Unit, Gtk.Dialog.Gtk_Response_OK);

         Widget := Gtk.Dialog.Add_Button
           (Ask_Unit, "gtk-cancel", Gtk.Dialog.Gtk_Response_Cancel);

         Box := Gtk.Dialog.Get_Vbox (Ask_Unit);

         Gtk.Table.Gtk_New (Table, 2, 2, True);
         Gtk.Box.Pack_Start (Box, Table);
         Gtk.Radio_Button.Gtk_New (Radio1,
                                   Label => "Library Unit Declaration:");

         Gtk.Table.Attach (Table, Radio1, 0, 1, 0, 1);
         Gtk.Radio_Button.Gtk_New (Radio2, Radio1,
                                   Label => "Compilation Unit Body:");

         Gtk.Table.Attach (Table, Radio2, 0, 1, 1, 2);
         Gtk.Combo_Box.Gtk_New_Text (Combo1);
         Gtk.Combo_Box.Gtk_New_Text (Combo2);

         begin
            declare
               use Asis.Compilation_Units;
               Spec : constant Asis.Compilation_Unit_List :=
                 Library_Unit_Declarations (My_Context);
               Impl : constant Asis.Compilation_Unit_List :=
                 Compilation_Unit_Bodies (My_Context);
            begin
               for J in Spec'Range loop
                  Gtk.Combo_Box.Append_Text
                    (Combo1, To_String (Unit_Full_Name (Spec (J))));
               end loop;

               for J in Impl'Range loop
                  Gtk.Combo_Box.Append_Text
                    (Combo2, To_String (Unit_Full_Name (Impl (J))));
               end loop;
            end;

         exception
            when Ex : Asis.Exceptions.ASIS_Inappropriate_Context  |
              Asis.Exceptions.ASIS_Inappropriate_Container        |
              Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
              Asis.Exceptions.ASIS_Inappropriate_Element          |
              Asis.Exceptions.ASIS_Inappropriate_Line             |
              Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
              Asis.Exceptions.ASIS_Failed                         =>

               Show_Error
                 (Ada.Exceptions.Exception_Name (Ex),
                  To_String (Asis.Implementation.Diagnosis));

               Asis.Implementation.Set_Status;

               Gtk.Combo_Box_Entry.Gtk_New_Text
                 (Gtk.Combo_Box_Entry.Gtk_Combo_Box_Entry (Combo1));

               Gtk.Combo_Box_Entry.Gtk_New_Text
                 (Gtk.Combo_Box_Entry.Gtk_Combo_Box_Entry (Combo2));
         end;

         Gtk.Table.Attach (Table, Combo1, 1, 2, 0, 1);
         Gtk.Table.Attach (Table, Combo2, 1, 2, 1, 2);
         Gtk.Dialog.Show_All (Ask_Unit);

         Gtkada.Handlers.Widget_Callback.Connect
           (Combo1, "changed", Combo_Changed'Access);

         Gtkada.Handlers.Widget_Callback.Connect
           (Combo2, "changed", Combo_Changed'Access);
      else
         Gtk.Dialog.Show (Ask_Unit);
      end if;

      Result := Gtk.Dialog.Run (Ask_Unit);
      Gtk.Dialog.Hide (Ask_Unit);

      if Result = Gtk.Dialog.Gtk_Response_OK then
         if Gtk.Radio_Button.Get_Active (Radio1) then
            return Asis.Compilation_Units.Library_Unit_Declaration
              (To_Wide_String (Gtk.Combo_Box.Get_Active_Text (Combo1)),
               My_Context);
         else
            return Asis.Compilation_Units.Compilation_Unit_Body
              (To_Wide_String (Gtk.Combo_Box.Get_Active_Text (Combo2)),
               My_Context);
         end if;
      else
         return Asis.Nil_Compilation_Unit;
      end if;
   end;

   procedure Combo_Changed
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      if Widget = Combo1 then
         Gtk.Radio_Button.Set_Active (Radio1, True);
      else
         Gtk.Radio_Button.Set_Active (Radio2, True);
      end if;
   end Combo_Changed;

end;
