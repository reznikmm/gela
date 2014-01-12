with Gela.Scanners;
with Gela.Lexical_Handler;
with Gela.Lexical_Types;

package body Gela.Plain_Lexers is

   ----------
   -- Scan --
   ----------

   overriding procedure Scan
     (Self   : Lexer;
      Input  : League.Strings.Universal_String;
      Output : not null access Gela.Lexers.Lexer_Destination'Class)
   is
      pragma Unreferenced (Self);
      Scanner : aliased Gela.Scanners.Scanner;
      Handler : aliased Gela.Lexical_Handler.Handler (Output);
      Token   : Gela.Lexical_Types.Token_Kind := Gela.Lexical_Types.Null_Token;
   begin
      Scanner.Set_Source (Input);
      Scanner.Set_Handler (Handler'Unchecked_Access);
      while Token not in
        Gela.Lexical_Types.End_Of_Input | Gela.Lexical_Types.Error
      loop
         Scanner.Get_Token (Token);
      end loop;
   end Scan;

end Gela.Plain_Lexers;
