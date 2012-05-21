package Asis.Extensions.Syntax_Highlight is

   type Token_Kinds is
     (Comment, Identifier, Literal, Delimiter, Keyword, Error);

   generic
      with procedure Set_Token (From, Count : Positive; Kind : Token_Kinds);
   procedure Parse
     (Text  : in     Wide_String;
      State : in out Natural);

   Default_State : constant := 0;

end Asis.Extensions.Syntax_Highlight;
