with Asis.Gela.Compilations;

package Asis.Gela.Text_Positions is

   type Text_Position is record
      Line   : ASIS_Natural;
      Column : ASIS_Natural;
   end record;

   Nil : constant Text_Position := (0, 0);

   function Is_Nil (Item : Text_Position) return Boolean;

   function "<" (Left, Right : Text_Position) return Boolean;

   function To_Wide_String (Item : Text_Position) return Wide_String;

   function Start_Position
     (C       : Compilations.Compilation;
      Element : Element_Index) return Text_Position;

   function End_Position
     (C       : Compilations.Compilation;
      Element : Element_Index) return Text_Position;

end Asis.Gela.Text_Positions;
