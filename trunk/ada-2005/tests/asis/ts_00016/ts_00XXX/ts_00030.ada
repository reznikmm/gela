
PACKAGE V46a is
   TYPE V73 IS RANGE 0 .. 150;

   TYPE V98f IS (V5e2, V5f3, V606);

   TYPE V55f (V5f2 : V98f := V5e2) IS
     RECORD
        CASE V5f2 IS
           WHEN V5f3 =>
              V5f7  : V73;
           when others =>
              null;
        END CASE;
     END RECORD;

   SUBTYPE V665 IS V55f(V5f3);

   TYPE V817 IS (V7b1,V7b2,V7b3, V7b4, V791);
   TYPE V7dd(V7b0 : V817 := V791) IS
      RECORD
         CASE V7b0 IS
            WHEN V791 =>
               NULL;
            WHEN OTHERS =>
               V531 : V73;
         END CASE;
      END RECORD;

   PROCEDURE V7f9(ACTION : IN V7dd);
END V46a;

PACKAGE BODY V46a IS
   PROCEDURE V7f9(ACTION : IN V7dd) is
   begin
      null;
   end V7f9;

   PROCEDURE V667 (V5f3 : IN V665) IS
   BEGIN
      V7f9 (V7dd'(V7b3,V5f3.V5f7));
   END V667;

END V46a;

