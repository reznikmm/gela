with Asis;

package Actions is
   type Action is abstract tagged null record;
   
   procedure Execute
     (Self    : in out Action;
      Element : Asis.Element) is abstract;

   procedure Leave
     (Self    : in out Action;
      Element : Asis.Element) is abstract;
   
   type Action_Access is access all Action'Class;

end Actions;
