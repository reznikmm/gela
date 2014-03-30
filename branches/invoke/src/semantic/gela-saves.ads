with Gela.Elements.Defining_Names;

package Gela.Saves is
   pragma Preelaborate;

   type Save is tagged;
   type Save_Access is access all Save'Class;

   type Save is abstract tagged limited null record;

   type Defining_Name_Save is new Save with record
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Parent : Save_Access;
   end record;

end Gela.Saves;
