with Ada.Characters.Handling;

with Gela.Decoders.Create;
with Gela.Source_Buffers.Current;

with Asis.Gela.Library;
with Asis.Gela.Current_State;

package body Asis.Gela.Compilations is

   type Buffer_Access is access all Source_Buffers.Current.Source_Buffer;

   function New_Buffer (File : in Wide_String) return Source_Buffer_Access;

   procedure Initialize
     (Object    : in out Compilation_Node;
      File_Name : in     Wide_String;
      Encoding  : in     Encodings.Encoding);

   ------------------------
   -- Create_Compilation --
   ------------------------

   procedure Create_Compilation
     (Comp     :    out Compilation;
      File     : in     Wide_String;
      Encoding : in     Encodings.Encoding)
   is
      use Asis.Gela.Pools;
      Pool : constant Pool_State := New_Pool;
   begin
      --  1) Create new storage pool state, save it in current_state
      Current_State.Set_Pool (Pool);
      --  2) Create new Compilation_Node
      Comp := new Compilation_Node;
      Comp.Pool := Pool;
      Initialize (Comp.all, File, Encoding);
      --  3) Save compilation in current_state
      Current_State.Set_Compilation (Comp);
   end Create_Compilation;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Object    : in out Compilation_Node;
      File_Name : in     Wide_String;
      Encoding  : in     Encodings.Encoding)
   is
      Ignore : Positive;
   begin
      Strings.Wide_Character_Vectors.Clear (Object.Text_Buffer);
      Storage_Vectors.Clear (Object.Storage);
      Line_Vectors.Clear (Object.Line_List);

      if Library.Is_Predefined_Unit (File_Name) then
         Object.Encoding := Encodings.UTF_8;
      else
         Object.Encoding := Encoding;
      end if;

      Strings.Add (Object.Text_Buffer, File_Name, Ignore);
      Object.Buffer := New_Buffer (File_Name);
      Object.Decoder := Decoders.Create (Object.Encoding);
      Object.Oper_Images := (others => 0);
   end Initialize;

   ----------------
   -- New_Buffer --
   ----------------

   function New_Buffer (File : in Wide_String) return Source_Buffer_Access is
      Result : constant Buffer_Access :=
        new Source_Buffers.Current.Source_Buffer;
      Name   : constant String := Ada.Characters.Handling.To_String (File);
   begin
      Source_Buffers.Current.Open (Result.all, Name);

      return Source_Buffer_Access (Result);
   end New_Buffer;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Object : Compilation_Node) return Wide_String is
   begin
      return Strings.Get (Object.Text_Buffer, 1);
   end File_Name;

   ------------------------
   -- Get_Operator_Image --
   ------------------------

   function Get_Operator_Image
     (Comp : Compilation;
      Oper : Operator_Kinds) return ASIS_Positive is
   begin
      if Comp.Oper_Images (Oper) = 0 then
         case Oper is
            when  An_And_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """and""",
                            Comp.Oper_Images (Oper));
            when An_Or_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """or""",
                            Comp.Oper_Images (Oper));
            when An_Xor_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """xor""",
                            Comp.Oper_Images (Oper));
            when An_Equal_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """=""",
                            Comp.Oper_Images (Oper));
            when A_Not_Equal_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """/=""",
                            Comp.Oper_Images (Oper));
            when A_Less_Than_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """<""",
                            Comp.Oper_Images (Oper));
            when A_Less_Than_Or_Equal_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """<=""",
                            Comp.Oper_Images (Oper));
            when A_Greater_Than_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """>""",
                            Comp.Oper_Images (Oper));
            when A_Greater_Than_Or_Equal_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """>=""",
                            Comp.Oper_Images (Oper));
            when A_Plus_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """+""",
                            Comp.Oper_Images (Oper));
            when A_Minus_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """-""",
                            Comp.Oper_Images (Oper));
            when A_Concatenate_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """&""",
                            Comp.Oper_Images (Oper));
            when A_Unary_Plus_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """+""",
                            Comp.Oper_Images (Oper));
            when A_Unary_Minus_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """-""",
                            Comp.Oper_Images (Oper));
            when A_Multiply_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """*""",
                            Comp.Oper_Images (Oper));
            when A_Divide_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """/""",
                            Comp.Oper_Images (Oper));
            when A_Mod_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """mod""",
                            Comp.Oper_Images (Oper));
            when A_Rem_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """rem""",
                            Comp.Oper_Images (Oper));
            when An_Exponentiate_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """**""",
                            Comp.Oper_Images (Oper));
            when An_Abs_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """abs""",
                            Comp.Oper_Images (Oper));
            when A_Not_Operator =>
               Strings.Add (Comp.Text_Buffer,
                            """not""",
                            Comp.Oper_Images (Oper));
            when Not_An_Operator =>
               raise Internal_Error;
         end case;
      end if;

      return ASIS_Positive (Comp.Oper_Images (Oper));
   end Get_Operator_Image;

end Asis.Gela.Compilations;
