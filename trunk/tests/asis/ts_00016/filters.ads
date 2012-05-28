with Asis;

package Filters is
   type Filter is abstract tagged null record;
   
   procedure Match
     (Self    : in out Filter;
      Element : Asis.Element;
      Success : out Boolean) is abstract;

   type Filter_Access is access all Filter'Class;
   
end Filters;
