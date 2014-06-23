with Gela.Int.Visiters;

package body Gela.Int.Attr_Functions is

   ------------
   -- Create --
   ------------

   function Create
     (Children : Natural;
      Kind     : Gela.Lexical_Types.Predefined_Symbols.Attribute)
      return Attr_Function is
   begin
      return (Length => Children,
              Kind   => Kind,
              Down   => (others => 0));
   end Create;

   ----------
   -- Kind --
   ----------

   function Kind
     (Self : Attr_Function)
      return Gela.Lexical_Types.Predefined_Symbols.Attribute is
   begin
      return Self.Kind;
   end Kind;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Attr_Function;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is
   begin
      Visiter.Attr_Function (Self);
   end Visit;

end Gela.Int.Attr_Functions;
