with Asis.Gela.Read_Managers;

package Asis.Gela.Simple_Read_Managers is

   type Read_Manager is new Read_Managers.Read_Manager with null record;

   procedure Read
     (Manager : in out Read_Manager;
      Context : in out Asis.Context;
      Units   : in     Read_Managers.Source_List);

end Asis.Gela.Simple_Read_Managers;
