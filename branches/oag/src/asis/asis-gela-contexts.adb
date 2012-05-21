with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;
with Asis.Compilation_Units;

--with Asis.Gela.Debug;
with Asis.Gela.Current_State;
with Asis.Gela.Text_Positions;
with Asis.Gela.Simple_Read_Managers;

with Ada.Wide_Text_IO;
with Ada.Strings.Wide_Maps;


package body Asis.Gela.Contexts is

   procedure Set_Encoding
     (Node     : in out Context_Node;
      Encoding : in     Wide_String);

   ---------------------
   -- Get_Compilation --
   ---------------------

   function Get_Compilation
     (Unit : Asis.Compilation_Unit) return Compilations.Compilation
   is
      C     : constant Asis.Context_Index := Get_Context (Unit).Index;
      N     : Context_Node renames Current_State.Contexts (C);
      Index : constant Unit_Index :=
        (Unit.Index / Context_Index'Last) mod 2 ** 20;
   begin
      if Assigned (Unit) then
         return Unit_Header_Vectors.Get (N.Unit_Headers, Index).Compilation;
      else
         return null;
      end if;
   end Get_Compilation;

   -------------------
   -- Get_Unit_Data --
   -------------------

   function Get_Unit_Data (Unit : Compilation_Unit) return Element_Index is
      C     : constant Asis.Context_Index := Get_Context (Unit).Index;
      N     : Context_Node renames Current_State.Contexts (C);
      Index : constant Unit_Index :=
        (Unit.Index / Context_Index'Last) mod 2 ** 20;
   begin
      return Unit_Header_Vectors.Get (N.Unit_Headers, Index).Data;
   end Get_Unit_Data;

   ---------------
   -- Get_Units --
   ---------------

   function Get_Units
     (Context : Asis.Context) return Asis.Compilation_Unit_List
   is
      use Unit_Header_Vectors;
      N      : Context_Node renames Current_State.Contexts (Context.Index);
      Result : Compilation_Unit_List (1 .. Length (N.Unit_Headers));
      Last   : ASIS_Natural := 0;
      Unit   : Unit_Index;
   begin
      for J in Result'Range loop
         Unit := Get (N.Unit_Headers, J).Unit;

         if Unit /= 0 then
            Last := Last + 1;
            Result (Last).Index := Unit;
         end if;
      end loop;

      return Result (1 .. Last);
   end Get_Units;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context (Unit : Asis.Compilation_Unit) return Context is
      C : constant Asis.Context_Index := Unit.Index mod Context_Index'Last + 1;
   begin
      return (Index => C);
   end Get_Context;

   ----------------------
   -- Parse_Parameters --
   ----------------------

   procedure Parse_Parameters
     (Node       : in out Context_Node;
      Context    : in     Asis.Context;
      Name       : in     Wide_String;
      Parameters : in     Wide_String)
   is
      use Asis.Gela.Library;
      use Ada.Strings.Wide_Maps;
      use type W.Unbounded_Wide_String;
      use type Read_Managers.Read_Manager_Access;

      Space  : constant Wide_Character_Set := To_Set (' ');
      Params : W.Unbounded_Wide_String := Node.Parameters;
      From   : Positive;
      To     : Natural;
   begin
      Clear_Search_Path (Node.Search_Path);
      W.Find_Token (Params, Space, Ada.Strings.Outside, From, To);

      while To > 0 loop
         declare
            Slice : constant Wide_String := W.Slice (Params, From, To);
            Word  : constant Wide_String (1 .. Slice'Length) := Slice;
         begin
            W.Delete (Params, 1, To);
            W.Find_Token (Params, Space, Ada.Strings.Outside, From, To);

            if Word'Length >= 2 and then Word (1) = '-' then
               case Word (2) is
                  when 'I' =>
                     Add_To_Search_Path
                       (Node.Search_Path, Word (3 .. Word'Last));

                  when 'A' =>
--                     pragma Assert (Debug.Set (Word (3 .. Word'Last)));
                     null;

                  when 'E' =>
                     Set_Encoding (Node, Word (3 .. Word'Last));

                  when others =>
                     null;
               end case;
            else
               Node.Input_File := W.To_Unbounded_Wide_String (Word);
            end if;
         end;
      end loop;

      if Node.Input_File = "" then
         Implementation.Set_Status
           (Status    => Asis.Errors.Parameter_Error,
            Diagnosis => "No input file");

         raise Exceptions.ASIS_Failed;
      elsif not File_Exists (W.To_Wide_String (Node.Input_File)) then
         Implementation.Set_Status
           (Status    => Asis.Errors.Parameter_Error,
            Diagnosis => "No such file: " &
                         W.To_Wide_String (Node.Input_File));

         raise Exceptions.ASIS_Failed;
      end if;

      if Node.Read_Manager = null then
         Node.Read_Manager := new Simple_Read_Managers.Read_Manager;
      end if;

      Node.Name          := W.To_Unbounded_Wide_String (Name);
      Node.Parameters    := W.To_Unbounded_Wide_String (Parameters);
      Node.Context       := Context;
      Node.Is_Associated := True;
   end Parse_Parameters;

   ----------
   -- Open --
   ----------

   procedure Open (Node : in out Context_Node) is
   begin
      Read_Managers.Read
        (Node.Read_Manager.all, Node.Context, (1 => Node.Input_File));

      Node.Is_Open := True;
   end Open;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error
     (Node        : in out Context_Node;
      Kind        : in     Extensions.Errors.Error_Kind;
      The_Unit    : in     Compilation_Unit := Asis.Nil_Compilation_Unit;
      Line        : in     Text.Line_Number        := 0;
      Column      : in     Text.Character_Position := 0;
      Text        : in     Wide_String             := "")
   is
      use type Extensions.Errors.Error_Handler_Access;

      function Get_File_Name return Wide_String is
         use Asis.Compilation_Units;
      begin
         if Is_Nil (The_Unit) then
            return W.To_Wide_String (Node.Input_File);
         else
            return Text_Name (The_Unit);
         end if;
      end Get_File_Name;

      function Where_Image return Wide_String is
      begin
         if Line /= 0 and Column /= 0 then
            return Asis.Gela.Text_Positions.To_Wide_String ((Line, Column));
         else
            return "";
         end if;
      end Where_Image;

      File_Name : constant Wide_String := Get_File_Name;

      Levels : constant array (Extensions.Errors.Error_Kind) of Error_Level :=
        (others => Error);

      Level  : constant Error_Level := Levels (Kind);

   begin
      if Node.Error_Handler /= null then
         Extensions.Errors.Report_Error
           (Object    => Node.Error_Handler.all,
            Kind      => Kind,
            The_Unit  => The_Unit,
            File_Name => File_Name,
            Line      => Line,
            Column    => Column,
            Text      => Text);
      else
         Ada.Wide_Text_IO.Put_Line
           (File_Name & ":" & Where_Image & ":" &
            Extensions.Errors.Error_Kind'Wide_Image (Kind) & ": " &
            Text);
      end if;

      if Node.Error < Level then
         Node.Error := Level;

         if Level = Fatal then
            Implementation.Set_Status
              (Asis.Errors.Environment_Error, "Fatal error");

            raise Asis.Exceptions.ASIS_Failed;
         end if;
      end if;
   end Report_Error;

   ------------------
   -- Set_Encoding --
   ------------------

   procedure Set_Encoding
     (Node     : in out Context_Node;
      Encoding : in     Wide_String) is
   begin
      Node.User_Encoding := Encodings.Encoding'Wide_Value (Encoding);
   exception when Constraint_Error =>
      Implementation.Set_Status
        (Status    => Asis.Errors.Parameter_Error,
         Diagnosis => "Unknown encoding: " & Encoding);

      raise Exceptions.ASIS_Failed;
   end Set_Encoding;

   -------------------
   -- Is_Associated --
   -------------------

   function Is_Associated (Node : Context_Node) return Boolean is
   begin
      return Node.Is_Associated;
   end Is_Associated;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Node : Context_Node) return Boolean is
   begin
      return Node.Is_Open;
   end Is_Open;

   ----------
   -- Name --
   ----------

   function Name (Node : Context_Node) return Wide_String is
   begin
      return W.To_Wide_String (Node.Name);
   end Name;

   ----------
   -- Name --
   ----------

   function Parameters (Node : Context_Node) return Wide_String is
   begin
      return W.To_Wide_String (Node.Parameters);
   end Parameters;

   -----------
   -- Close --
   -----------

   procedure Close (Node : in out Context_Node) is
   begin
      Node.Is_Open := False;
   end Close;

   ----------------
   -- Dissociate --
   ----------------

   procedure Dissociate (Node : in out Context_Node) is
   begin
      Node.Is_Associated := False;
   end Dissociate;

   function Has_File (Node : Context_Node; File : Wide_String) return Boolean
   is
      use Unit_Header_Vectors;
      use Asis.Gela.Compilations;

      Unit : Unit_Header;
      Comp : Compilation;
   begin
      for J in 1 .. Length (Node.Unit_Headers) loop
         Unit := Get (Node.Unit_Headers, J);

         if Unit.Compilation /= Comp then
            Comp := Unit.Compilation;

            if File_Name (Comp.all) = File then
               return True;
            end if;
         end if;
      end loop;

      return False;
   end Has_File;

   --------------
   -- Encoding --
   --------------

   function Encoding (Node : Context_Node) return Encodings.Encoding is
   begin
      return Node.User_Encoding;
   end Encoding;

   -----------------------
   -- Set_Error_Handler --
   -----------------------

   procedure Set_Error_Handler
     (Node    : in out Context_Node;
      Handler : in     Extensions.Errors.Error_Handler_Access) is
   begin
      Node.Error_Handler := Handler;
   end Set_Error_Handler;

end Asis.Gela.Contexts;
