package body Filters.Any is
   
   procedure Match
     (Self    : in out Any_Filter;
      Element : Asis.Element;
      Success : out Boolean)
   is
      pragma Unreferenced (Element);
      pragma Unreferenced (Self);
   begin
      Success := True;
   end Match;

end Filters.Any;
