with Gela.Grammars;
with Ada.Containers.Hashed_Sets;
with League.Strings.Hash;

package AG_Tools.Check_Ordered is

   type NT_Map is array
     (Gela.Grammars.Non_Terminal_Index range <>,
      Gela.Grammars.Non_Terminal_Index range <>) of Boolean;

   type NT_List is array
     (Gela.Grammars.Non_Terminal_Index range <>) of Boolean;

   type Option_List is tagged private;

   function Is_Option
     (Self : Option_List;
      G    : Gela.Grammars.Grammar;
      Part : Gela.Grammars.Part) return Boolean;

   function Pre_Process
     (G : Gela.Grammars.Grammar;
      Implement : NT_Map;
      Is_Concrete : NT_List;
      Is_Option : in out Option_List) return Gela.Grammars.Grammar_Access;

   procedure Check
     (G : Gela.Grammars.Grammar;
      Implement : NT_Map;
      Is_Concrete : NT_List;
      Is_Option : Option_List);

private

   package String_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => League.Strings.Universal_String,
      Hash                => League.Strings.Hash,
      Equivalent_Elements => League.Strings."=",
      "="                 => League.Strings."=");

   type Option_List is tagged record
      Set : String_Sets.Set;
   end record;

   procedure Add_Option
     (Self : in out Option_List;
      G    : Gela.Grammars.Grammar;
      Prod : Gela.Grammars.Production;
      Part : Gela.Grammars.Part);

end AG_Tools.Check_Ordered;
