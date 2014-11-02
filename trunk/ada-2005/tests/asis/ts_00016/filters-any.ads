package Filters.Any is
   type Any_Filter is new Filter with null record;
   
   procedure Match
     (Self    : in out Any_Filter;
      Element : Asis.Element;
      Success : out Boolean);

end Filters.Any;
