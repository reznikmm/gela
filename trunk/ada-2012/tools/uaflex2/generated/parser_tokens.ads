with  Uaflex.Nodes;
package Parser_Tokens is


   subtype YYSType is UAFLEX.Nodes.Node;

    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, Name, Name_List_End,
         Start, Excl_Start, Section_End,
         Regexp, Action );

    subtype Token_Kind is Token;

    Syntax_Error : exception;

end Parser_Tokens;