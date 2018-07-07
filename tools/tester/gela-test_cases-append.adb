package body Gela.Test_Cases.Append is

   ---------
   -- "+" --
   ---------

   function "+"
     (Left, Right : access Test_Cases.Test_Case'Class)
      return Gela.Test_Cases.Test_Case_Access is
   begin
      return new Test_Case'(List => (Test_Case_Access (Left),
                                     Test_Case_Access (Right)),
                            Last => 0,
                            Duration => <>);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+"
     (Left : access Test_Cases.Test_Case'Class;
      Right : access Gela.Test_Cases.Execute.Test_Case'Class)
      return Gela.Test_Cases.Execute.Test_Case_Access is
   begin
      return new Run_Test_Case'
        (Left => Test_Case_Access (Left),
         Right => Gela.Test_Cases.Execute.Test_Case_Access (Right),
         On_Left => False,
         Duration => <>);
   end "+";

   ---------------
   -- Arguments --
   ---------------

   function Arguments
     (Self : Run_Test_Case)
      return League.String_Vectors.Universal_String_Vector
   is
   begin
      return Self.Right.Arguments;
   end Arguments;

   -----------
   -- Build --
   -----------

   function Build
     (Self : Run_Test_Case)
      return League.Strings.Universal_String is
   begin
      return Self.Right.Build;
   end Build;

   -------------
   -- Command --
   -------------

   function Command
     (Self : Run_Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.Right.Command;
   end Command;

   --------------
   -- Duration --
   --------------

   function Duration (Self : Test_Case) return League.Calendars.Time is
   begin
      return Self.Duration;
   end Duration;

   --------------
   -- Duration --
   --------------

   function Duration (Self : Run_Test_Case) return League.Calendars.Time is
   begin
      return Self.Duration;
   end Duration;

   ----------
   -- File --
   ----------

   function File (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.List (Self.List'Last).File;
   end File;

   ----------
   -- File --
   ----------

   function File
     (Self : Run_Test_Case)
      return League.Strings.Universal_String is
   begin
      return Self.Right.File;
   end File;

   -------------
   -- Fixture --
   -------------

   function Fixture
     (Self : Test_Case)
      return League.Strings.Universal_String is
   begin
      return Self.List (Self.List'Last).Fixture;
   end Fixture;

   -------------
   -- Fixture --
   -------------

   function Fixture
     (Self : Run_Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.Right.Fixture;
   end Fixture;

   ----------
   -- Name --
   ----------

   function Name (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.List (Self.List'Last).Name;
   end Name;

   ----------
   -- Name --
   ----------

   function Name
     (Self : Run_Test_Case)
      return League.Strings.Universal_String is
   begin
      return Self.Right.Name;
   end Name;

   ------------
   -- Output --
   ------------

   function Output
     (Self : Test_Case)
      return League.Strings.Universal_String is
   begin
      return Self.List (Self.Last).Output;
   end Output;

   ------------
   -- Output --
   ------------

   function Output
     (Self : Run_Test_Case)
      return League.Strings.Universal_String is
   begin
      if Self.On_Left then
         return Self.Left.Output;
      else
         return Self.Right.Output;
      end if;
   end Output;

   ----------
   -- Path --
   ----------

   function Path
     (Self : Run_Test_Case)
      return League.Strings.Universal_String is
   begin
      return Self.Right.Path;
   end Path;

   ---------
   -- Run --
   ---------

   procedure Run (Self : in out Test_Case) is
      use type League.Calendars.Time;
   begin
      for J in Self.List'Range loop
         Self.List (J).Run;
         Self.Last := J;

         if J = 1 then
            Self.Duration := Self.List (J).Duration;
         else
            Self.Duration := Self.Duration + Self.List (J).Duration;
         end if;

         exit when Self.List (J).Status /= Success;
      end loop;
   end Run;

   ---------
   -- Run --
   ---------

   procedure Run (Self : in out Run_Test_Case) is
      use type League.Calendars.Time;
   begin
      Self.Left.Run;
      Self.Duration := Self.Left.Duration;

      if Self.Left.Status = Success then
         Self.On_Left := False;
         Self.Right.Run;
         Self.Duration := Self.Duration + Self.Right.Duration;
      else
         Self.On_Left := True;
      end if;
   end Run;

   -----------------
   -- Set_Command --
   -----------------

   procedure Set_Command
     (Self      : in out Run_Test_Case;
      Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector)
   is
   begin
      Self.Right.Set_Command (Command, Arguments);
   end Set_Command;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Self  : in out Run_Test_Case;
      Value : League.Strings.Universal_String)
   is
   begin
      Self.Right.Set_Name (Value);
   end Set_Name;

   ------------
   -- Status --
   ------------

   function Status (Self : Test_Case) return Status_Kind is
   begin
      return Self.List (Self.Last).Status;
   end Status;

   ------------
   -- Status --
   ------------

   function Status (Self : Run_Test_Case) return Status_Kind is
   begin
      if Self.On_Left then
         return Self.Left.Status;
      else
         return Self.Right.Status;
      end if;
   end Status;

   ---------------
   -- Traceback --
   ---------------

   function Traceback
     (Self : Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.List (Self.Last).Traceback;
   end Traceback;

   ---------------
   -- Traceback --
   ---------------

   function Traceback
     (Self : Run_Test_Case)
      return League.Strings.Universal_String
   is
   begin
      if Self.On_Left then
         return Self.Left.Traceback;
      else
         return Self.Right.Traceback;
      end if;
   end Traceback;

end Gela.Test_Cases.Append;
