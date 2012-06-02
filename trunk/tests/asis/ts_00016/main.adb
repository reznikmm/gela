with Ada.Wide_Text_IO;
with Ada.Command_Line;

with Asis;
with Asis.Implementation;
with Asis.Ada_Environments;

with Actions.Create;
with Filters.Create;
with Process_Context;
with Traversing_Actions;

procedure Main is
   
   package Enum is
      type Command_Kinds is
        (''',
         Associate,
         Process_Context,
         Action);
      
      type Unit_Kinds is (Compilation_Unit_Body, Library_Unit_Declaration);
   end Enum;
   
   package Command_IO is
     new Ada.Wide_Text_IO.Enumeration_IO (Enum.Command_Kinds);
   
   package Unit_Kind_IO is
     new Ada.Wide_Text_IO.Enumeration_IO (Enum.Unit_Kinds);

   Context : Asis.Context;
   State   : Traversing_Actions.Traversal_State;
   Input   : Ada.Wide_Text_IO.File_Type;
   Command : Enum.Command_Kinds;
begin
   Ada.Wide_Text_IO.Open
     (Input,
      Ada.Wide_Text_IO.In_File, 
      Ada.Command_Line.Argument (1));
   
   while not Ada.Wide_Text_IO.End_Of_File (Input) loop
      Command_IO.Get (Input, Command);
      
      case Command is
         when Enum.''' =>
            declare
               Line : Wide_String (1 .. 80);
               Last : Natural;
            begin
               Ada.Wide_Text_IO.Get_Line (Input, Line, Last);
            end;
         when Enum.Associate =>
            declare
               Line : Wide_String (1 .. 280);
               Last : Natural;
            begin
               Ada.Wide_Text_IO.Get_Line (Input, Line, Last);
               
               Asis.Implementation.Initialize ("");

               Asis.Ada_Environments.Associate
                 (The_Context => Context,
                  Name        => Asis.Ada_Environments.Default_Name,
                  Parameters  => Line (1 .. Last));

               Asis.Ada_Environments.Open (Context);
            end;
   
         when Enum.Process_Context =>
            declare
               use type Enum.Unit_Kinds;
               Kind : Enum.Unit_Kinds;
               Line : Wide_String (1 .. 80);
               Last : Natural;
            begin
               Unit_Kind_IO.Get (Input, Kind);
               Ada.Wide_Text_IO.Get_Line (Input, Line, Last);
               Process_Context
                 (Context,
                  Kind = Enum.Library_Unit_Declaration,
                  Line (1 .. Last), State);
            end;
            
         when Enum.Action =>
            declare
               Action : constant Actions.Action_Access :=
                 Actions.Create.Action (Input);
               Filter : constant Filters.Filter_Access :=
                 Filters.Create.Filter (Input);
            begin
               State.Add (Action, Filter);
            end;
      end case;
   end loop;
   
   Asis.Ada_Environments.Close        (Context);
   Asis.Ada_Environments.Dissociate   (Context);
   Asis.Implementation.Finalize       ("");
end Main;
