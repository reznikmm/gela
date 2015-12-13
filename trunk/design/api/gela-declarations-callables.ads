package Gela.Declarations.Callables is
   pragma Preelaborate;

   type Callable is limited interface
     and Gela.Declarations.Declaration;
   --  Common ancestor of all callable declarations
   type Callable_Access is
     access constant Callable'Class;
   for Callable_Access'Storage_Size use 0;

   not overriding function Parameter_Prolile
     (Self : aliased Callable)
      return Gela.Element_Sequences.Element_Sequence_Access is abstract;
   --
   not overriding function Result_Subtype
     (Self : aliased Callable)
      return Gela.Elements.Element_Access is abstract;

   --  TODO: Overriding_Indicator?

end Gela.Declarations.Callables;
