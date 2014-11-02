package Actions.Print is
   
   type Print_Action is new Action with private;
   
   procedure Execute
     (Self    : in out Print_Action;
      Element : Asis.Element);

   procedure Leave
     (Self    : in out Print_Action;
      Element : Asis.Element);
   
private
   
   type Print_Action is new Action with record
      Indent : Natural := 0;
   end record;

end Actions.Print;
