package Actions.Representation_Image is
   
   type Action is new Actions.Action with private;
   
   procedure Execute
     (Self    : in out Action;
      Element : Asis.Element);

   procedure Leave
     (Self    : in out Action;
      Element : Asis.Element) is null;
   
private
   
   type Action is new Actions.Action with null record;

end Actions.Representation_Image;
