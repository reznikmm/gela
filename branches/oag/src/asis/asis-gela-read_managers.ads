with Ada.Strings.Wide_Unbounded;


package Asis.Gela.Read_Managers is
   package W renames Ada.Strings.Wide_Unbounded;

   --  List of file names
   type Source_List is array (Positive range <>) of W.Unbounded_Wide_String;

   ------------------
   -- Read_Manager --
   ------------------

   type Read_Manager is abstract tagged null record;

   procedure Read
     (Manager : in out Read_Manager;
      Context : in out Asis.Context;
      Units   : in     Source_List) is abstract;
   ------------------------------------------------------------------
   --  Read manager processes list of source files and fills Context
   --  There are multithread and sequencial read managers.

   type Read_Manager_Access is access Read_Manager'Class;

end Asis.Gela.Read_Managers;

