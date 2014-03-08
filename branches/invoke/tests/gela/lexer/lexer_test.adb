with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.Strings;
with League.String_Vectors;

with Gela.Contexts;
with Gela.Context_Factories;
with Gela.Lexers;
with Gela.Lexical_Types;
with Gela.Lexical_Handler;

procedure Lexer_Test is
   package Output is
      type Lexer_Destination is new Gela.Lexers.Lexer_Destination with record
         Line   : Positive := 1;
         Start  : Gela.Lexical_Types.Text_Index := 0;
         Result : League.Strings.Universal_String;
      end record;

      overriding procedure New_Token
        (Self  : in out Lexer_Destination;
         Token : Gela.Lexical_Types.Token);

      overriding procedure New_Line
        (Self  : in out Lexer_Destination;
         Token : Gela.Lexical_Types.Line_Span);
   end Output;

   package body Output is

      LF : constant Wide_Wide_Character := Wide_Wide_Character'Val (10);

      ---------------
      -- New_Token --
      ---------------

      overriding procedure New_Token
        (Self  : in out Lexer_Destination;
         Token : Gela.Lexical_Types.Token)
      is
         use Gela.Lexical_Types;
      begin
         case Token.Kind is
            when End_Of_Input =>
               Self.Result.Append ("EOF");
            when Error =>
               Self.Result.Append ("ERROR");
            when Comment_Token =>
               Self.Result.Append ("COMM");
            when Less_Token | Equal_Token | Greater_Token | Hyphen_Token |
                 Slash_Token | Star_Token | Ampersand_Token | Plus_Token |
                 Less_Or_Equal_Token | Greater_Or_Equal_Token |
                 Inequality_Token | Double_Star_Token | Right_Label_Token |
                 Box_Token | Left_Label_Token |
                 Assignment_Token | Arrow_Token |
                 Double_Dot_Token |
                 Apostrophe_Token | Left_Parenthesis_Token |
                 Right_Parenthesis_Token |
                 Comma_Token | Dot_Token |
                 Colon_Token | Semicolon_Token |
                 Vertical_Line_Token
               =>
               Self.Result.Append ("DELIMITER");
            when Or_Token | And_Token | Xor_Token | Mod_Token | Rem_Token |
                 Abs_Token | Not_Token |
                 Abort_Token |
                 Abstract_Token | Accept_Token |
                 Access_Token | Aliased_Token | All_Token |
                 Array_Token | At_Token |
                 Begin_Token | Body_Token | Case_Token |
                 Constant_Token | Declare_Token | Delay_Token |
                 Delta_Token | Digits_Token | Do_Token |
                 Else_Token | Elsif_Token | End_Token |
                 Entry_Token | Exception_Token | Exit_Token |
                 For_Token | Function_Token | Generic_Token |
                 Goto_Token | If_Token | In_Token |
                 Interface_Token | Is_Token | Limited_Token |
                 Loop_Token | New_Token |
                 Null_Token | Of_Token |
                 Others_Token | Out_Token |
                 Overriding_Token | Package_Token | Pragma_Token |
                 Private_Token | Procedure_Token | Protected_Token |
                 Raise_Token | Range_Token | Record_Token |
                 Renames_Token | Requeue_Token |
                 Return_Token | Reverse_Token | Select_Token |
                 Separate_Token | Some_Token | Subtype_Token |
                 Synchronized_Token |
                 Tagged_Token | Task_Token | Terminate_Token |
                 Then_Token | Type_Token | Until_Token |
                 Use_Token | When_Token | While_Token |
                 With_Token
               =>
               Self.Result.Append ("KEYWORD");
            when Identifier_Token =>
               Self.Result.Append ("IDENTIFIER");
            when Numeric_Literal_Token |
                 Character_Literal_Token | String_Literal_Token =>
               Self.Result.Append ("LITERAL");
         end case;

         Self.Result.Append
           (Line_Index'Wide_Wide_Image (Token.Line));
         Self.Result.Append
           (Text_Index'Wide_Wide_Image (Token.First - Self.Start));
         Self.Result.Append
           (Text_Index'Wide_Wide_Image (Token.Last - Self.Start));
         Self.Result.Append (LF);
      end New_Token;

      --------------
      -- New_Line --
      --------------

      overriding procedure New_Line
        (Self  : in out Lexer_Destination;
         Token : Gela.Lexical_Types.Line_Span)
      is
         use Gela.Lexical_Types;
      begin
         if Token.Comment < Token.Last then
            Self.Result.Append ("COMMENT");
            Self.Result.Append
              (Positive'Wide_Wide_Image (Self.Line));
            Self.Result.Append
              (Text_Index'Wide_Wide_Image (Token.Comment - Token.First + 1));
            Self.Result.Append
              (Text_Index'Wide_Wide_Image (Token.Last - Token.First + 1));
            Self.Result.Append (LF);
         end if;

         Self.Line := Self.Line + 1;
         Self.Start := Token.Last + 1;
      end New_Line;

   end Output;

   use type League.Hash_Type;

   Hash    : League.Hash_Type;
   Path    : League.Strings.Universal_String;
   File    : Ada.Wide_Wide_Text_IO.File_Type;
   Input   : League.Strings.Universal_String;
   Context : constant Gela.Contexts.Context_Access :=
     Gela.Context_Factories.Create_Context
       (League.String_Vectors.Empty_Universal_String_Vector,
        League.Strings.Empty_Universal_String);
   Lexer : constant Gela.Lexers.Lexer_Access := Context.Lexer;
   Dest : aliased Output.Lexer_Destination;
begin
   --  Command line: "-IDIR1" "-IDIR2" "FILE" "HASH"
   Path := League.Application.Arguments.Element (2);
   Path.Slice (3, Path.Length);
   Path.Append ("/");
   Path.Append (League.Application.Arguments.Element (3));
   Hash := League.Hash_Type'Wide_Wide_Value
     (League.Application.Arguments.Element (4).To_Wide_Wide_String);
   Gela.Lexical_Handler.Initialize;

   Ada.Wide_Wide_Text_IO.Open
     (File,
      Ada.Wide_Wide_Text_IO.In_File,
      Path.To_UTF_8_String,
      "WCEM=8");

   while not Ada.Wide_Wide_Text_IO.End_Of_File (File) loop
      Input.Append (Ada.Wide_Wide_Text_IO.Get_Line (File));
      Input.Append (Wide_Wide_Character'Val (10));
   end loop;

   Lexer.Scan (Input, Dest'Access);

   if Dest.Result.Hash /= Hash then
      Ada.Wide_Wide_Text_IO.Put_Line (Dest.Result.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put_Line
        (League.Hash_Type'Wide_Wide_Image (Dest.Result.Hash));

      raise Constraint_Error;
   end if;
end Lexer_Test;
