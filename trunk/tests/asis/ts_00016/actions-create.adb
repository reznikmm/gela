with Actions.Print;

package body Actions.Create is
   
   function Action
     (Input : Ada.Wide_Text_IO.File_Type)
     return Action_Access
   is
      pragma Unreferenced (Input);
   begin
      return new Actions.Print.Print_Action;
   end Action;

end Actions.Create;
