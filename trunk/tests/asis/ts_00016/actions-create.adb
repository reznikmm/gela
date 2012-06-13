with Actions.Print;
with Actions.Representation_Image;

package body Actions.Create is
   
   package Enum is
      type Action_Kind is (Print, Representation_Image);
      
      package Action_Kind_IO is
         new Ada.Wide_Text_IO.Enumeration_IO (Action_Kind);
   end Enum;
   
   function Action
     (Input : Ada.Wide_Text_IO.File_Type)
     return Action_Access
   is
      Kind : Enum.Action_Kind;
   begin
      Enum.Action_Kind_IO.Get (Input, Kind);
      
      case Kind is
         when Enum.Print =>
            return new Actions.Print.Print_Action;
         when Enum.Representation_Image =>
            return new Actions.Representation_Image.Action;
      end case;
   end Action;

end Actions.Create;
