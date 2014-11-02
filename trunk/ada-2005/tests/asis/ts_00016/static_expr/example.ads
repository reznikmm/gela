package Example is

   Named_Number    : constant := 2;
   Static_Constant : constant Integer := 3;

   type Modular_Type is mod 8;
   type Fixed_Point_Type is delta 0.01 range -100_000.00 .. 100_000.00;
   type Decimal_Point_Type is delta 0.01 digits 8;

   Modular_Constant       : constant Modular_Type := 3;
   Float_Constant         : constant Float := 3.0;
   Fixed_Point_Constant   : constant Fixed_Point_Type := 3.0;
   Decimal_Point_Constant : constant Decimal_Point_Type := 3.0;

   type Enum is
     (P00, P01, P02, P03, P04, P05, P06, P07, P08, P09,
      P10, P11, P12, P13, P14, P15, P16, P17, P18, P19,
      P20, P21, P22, P23, P24, P25, P26, P27, P28, P29,
      P30, P31, P32, P33, P34, P35, P36, P37, P38, P39,
      P40, P41, P42, P43, P44, P45, P46, P47, P48, P49,
      P50, P51, P52, P53, P54, P55, P56, P57, P58, P59,
      P60, P61, P62, P63, P64, P65, P66, P67, P68, P69,
      P70, P71, P72, P73, P74, P75, P76, P77, P78, P79,
      P80, P81, P82, P83, P84, P85, P86, P87, P88, P89,
      P90, P91, P92, P93, P94, P95, P96, P97, P98, P99,
      P100, P101, P102, P103, P104, P105, P106, P107, P108, P109,
      P110, P111, P112, P113, P114, P115, P116, P117, P118, P119,
      P120, P121, P122, P123, P124, P125, P126, P127, P128, P129,
      P130, P131, P132, P133, P134, --P135, P136, P137, P138,
      P139,
      P140, P141, P142, --P143, P144, P145, P146,
      P147, P148, P149,
      P150, P151, P152, P153, P154, P155, P156, P157, P158, P159,
      P160, P161, P162, P163, P164, P165, P166, P167--, P168, P169
      );

   for Enum use
     (P00 => 0,                --  =0
      P01 => 0 + 1,            --  =1
      P02 => Named_Number,
      P03 => Static_Constant,
      --  "and" for Boolean --
      P04 => 4 + Boolean'Pos (False and False),
      P05 => 5 + Boolean'Pos (False and True),
      P06 => 6 + Boolean'Pos (True  and False),
      P07 => 6 + Boolean'Pos (True  and True),
      --  "and" for Modular --
      P08 => 6 + Modular_Type'Pos (3 and 6),
      --  "or" for Boolean --
      P09 => 9  + Boolean'Pos (False or False),
      P10 => 9  + Boolean'Pos (False or True),
      P11 => 10 + Boolean'Pos (True  or False),
      P12 => 11 + Boolean'Pos (True  or True),
      --  "or" for Modular --
      P13 => 6 + Modular_Type'Pos (3 or 6),
      --  "xor" for Boolean --
      P14 => 14 + Boolean'Pos (False xor False),
      P15 => 14 + Boolean'Pos (False xor True),
      P16 => 15 + Boolean'Pos (True  xor False),
      P17 => 17 + Boolean'Pos (True  xor True),
      --  "xor" for Modular --
      P18 => 13 + Modular_Type'Pos (3 xor 6),
      --  "=" --
      P19 => 18 + Boolean'Pos (False = False),
      P20 => 20 + Boolean'Pos (False = True),
      P21 => 21 + Boolean'Pos (True  = False),
      P22 => 21 + Boolean'Pos (True  = True),
      P23 => 23 + Boolean'Pos (Modular_Constant = 6),
      P24 => 23 + Boolean'Pos (Modular_Constant = 3),
      P25 => 25 + Boolean'Pos (Static_Constant = 6),
      P26 => 25 + Boolean'Pos (Static_Constant = 3),
      P27 => 27 + Boolean'Pos (Float_Constant = 6.0),
      P28 => 27 + Boolean'Pos (Float_Constant = 3.0),
      P29 => 29 + Boolean'Pos (Fixed_Point_Constant = 6.0),
      P30 => 29 + Boolean'Pos (Fixed_Point_Constant = 3.0),
      P31 => 31 + Boolean'Pos (Decimal_Point_Constant = 6.0),
      P32 => 31 + Boolean'Pos (Decimal_Point_Constant = 3.0),

      --  "<" --

      P33 => 33 + Boolean'Pos (False < False),
      P34 => 33 + Boolean'Pos (False < True),
      P35 => 35 + Boolean'Pos (True  < False),
      P36 => 36 + Boolean'Pos (True  < True),
      P37 => 36 + Boolean'Pos (Modular_Constant < 6),
      P38 => 38 + Boolean'Pos (Modular_Constant < 3),
      P39 => 39 + Boolean'Pos (6 < Modular_Constant),
      P40 => 39 + Boolean'Pos (Static_Constant < 6),
      P41 => 41 + Boolean'Pos (Static_Constant < 3),
      P42 => 42 + Boolean'Pos (6 < Static_Constant),
      P43 => 42 + Boolean'Pos (Float_Constant < 6.0),
      P44 => 44 + Boolean'Pos (Float_Constant < 3.0),
      P45 => 45 + Boolean'Pos (6.0 < Float_Constant),
      P46 => 45 + Boolean'Pos (Fixed_Point_Constant < 6.0),
      P47 => 47 + Boolean'Pos (Fixed_Point_Constant < 3.0),
      P48 => 48 + Boolean'Pos (6.0 < Fixed_Point_Constant),
      P49 => 48 + Boolean'Pos (Decimal_Point_Constant < 6.0),
      P50 => 50 + Boolean'Pos (Decimal_Point_Constant < 3.0),
      P51 => 51 + Boolean'Pos (6.0 < Decimal_Point_Constant),

      --  "<=" --

      P52 => 51 + Boolean'Pos (False <= False),
      P53 => 52 + Boolean'Pos (False <= True),
      P54 => 54 + Boolean'Pos (True  <= False),
      P55 => 54 + Boolean'Pos (True  <= True),
      P56 => 55 + Boolean'Pos (Modular_Constant <= 6),
      P57 => 56 + Boolean'Pos (Modular_Constant <= 3),
      P58 => 58 + Boolean'Pos (6 <= Modular_Constant),
      P59 => 58 + Boolean'Pos (Static_Constant <= 6),
      P60 => 59 + Boolean'Pos (Static_Constant <= 3),
      P61 => 61 + Boolean'Pos (6 <= Static_Constant),
      P62 => 61 + Boolean'Pos (Float_Constant <= 6.0),
      P63 => 62 + Boolean'Pos (Float_Constant <= 3.0),
      P64 => 64 + Boolean'Pos (6.0 <= Float_Constant),
      P65 => 64 + Boolean'Pos (Fixed_Point_Constant <= 6.0),
      P66 => 65 + Boolean'Pos (Fixed_Point_Constant <= 3.0),
      P67 => 67 + Boolean'Pos (6.0 <= Fixed_Point_Constant),
      P68 => 67 + Boolean'Pos (Decimal_Point_Constant <= 6.0),
      P69 => 68 + Boolean'Pos (Decimal_Point_Constant <= 3.0),
      P70 => 70 + Boolean'Pos (6.0 <= Decimal_Point_Constant),

      --  ">" --

      P71 => 71 + Boolean'Pos (False > False),
      P72 => 72 + Boolean'Pos (False > True),
      P73 => 72 + Boolean'Pos (True  > False),
      P74 => 74 + Boolean'Pos (True  > True),
      P75 => 75 + Boolean'Pos (Modular_Constant > 6),
      P76 => 76 + Boolean'Pos (Modular_Constant > 3),
      P77 => 76 + Boolean'Pos (6 > Modular_Constant),
      P78 => 78 + Boolean'Pos (Static_Constant > 6),
      P79 => 79 + Boolean'Pos (Static_Constant > 3),
      P80 => 79 + Boolean'Pos (6 > Static_Constant),
      P81 => 81 + Boolean'Pos (Float_Constant > 6.0),
      P82 => 82 + Boolean'Pos (Float_Constant > 3.0),
      P83 => 82 + Boolean'Pos (6.0 > Float_Constant),
      P84 => 84 + Boolean'Pos (Fixed_Point_Constant > 6.0),
      P85 => 85 + Boolean'Pos (Fixed_Point_Constant > 3.0),
      P86 => 85 + Boolean'Pos (6.0 > Fixed_Point_Constant),
      P87 => 87 + Boolean'Pos (Decimal_Point_Constant > 6.0),
      P88 => 88 + Boolean'Pos (Decimal_Point_Constant > 3.0),
      P89 => 88 + Boolean'Pos (6.0 > Decimal_Point_Constant),

      --  ">=" --

      P90  =>  89 + Boolean'Pos (False >= False),
      P91  =>  91 + Boolean'Pos (False >= True),
      P92  =>  91 + Boolean'Pos (True  >= False),
      P93  =>  92 + Boolean'Pos (True  >= True),
      P94  =>  94 + Boolean'Pos (Modular_Constant >= 6),
      P95  =>  94 + Boolean'Pos (Modular_Constant >= 3),
      P96  =>  95 + Boolean'Pos (6 >= Modular_Constant),
      P97  =>  97 + Boolean'Pos (Static_Constant >= 6),
      P98  =>  97 + Boolean'Pos (Static_Constant >= 3),
      P99  =>  98 + Boolean'Pos (6 >= Static_Constant),
      P100 => 100 + Boolean'Pos (Float_Constant >= 6.0),
      P101 => 100 + Boolean'Pos (Float_Constant >= 3.0),
      P102 => 101 + Boolean'Pos (6.0 >= Float_Constant),
      P103 => 103 + Boolean'Pos (Fixed_Point_Constant >= 6.0),
      P104 => 103 + Boolean'Pos (Fixed_Point_Constant >= 3.0),
      P105 => 104 + Boolean'Pos (6.0 >= Fixed_Point_Constant),
      P106 => 106 + Boolean'Pos (Decimal_Point_Constant >= 6.0),
      P107 => 106 + Boolean'Pos (Decimal_Point_Constant >= 3.0),
      P108 => 107 + Boolean'Pos (6.0 >= Decimal_Point_Constant),
      --  "+" --
      P109 => 108 + Boolean'Pos (+Modular_Constant = 3),
      P110 => 107 + (+Static_Constant),
      P111 => 110 + Boolean'Pos (+Float_Constant = 3.0),
      P112 => 111 + Boolean'Pos (+Fixed_Point_Constant = 3.0),
      P113 => 112 + Boolean'Pos (+Decimal_Point_Constant = 3.0),
      --  "-" --
      P114 => 113 + Boolean'Pos (-Modular_Constant = 5),
      P115 => 118 + (-Static_Constant),
      P116 => 115 + Boolean'Pos (-Float_Constant = 0.0 - 3.0),
      P117 => 116 + Boolean'Pos (-Fixed_Point_Constant = 0.0 - 3.0),
      P118 => 117 + Boolean'Pos (-Decimal_Point_Constant = 0.0 - 3.0),
      --  "+" --
      P119 => 118 + Boolean'Pos (Modular_Constant + 3 = 6),
      P120 => 119 + Boolean'Pos (Modular_Constant + 6 = 1),
      P121 => 118 + Static_Constant,
      P122 => 121 + Boolean'Pos (Float_Constant + 2.0 = 5.0),
      P123 => 122 + Boolean'Pos (Fixed_Point_Constant + 2.0 = 5.0),
      P124 => 123 + Boolean'Pos (Decimal_Point_Constant + 2.0 = 5.0),
      --  "-" --
      P125 => 124 + Boolean'Pos (Modular_Constant - 2 = 1),
      P126 => 125 + Boolean'Pos (Modular_Constant - 6 = 5),
      P127 => 130 - Static_Constant,
      P128 => 127 + Boolean'Pos (Float_Constant - 2.0 = 1.0),
      P129 => 128 + Boolean'Pos (Fixed_Point_Constant - 2.0 = 1.0),
      P130 => 129 + Boolean'Pos (Decimal_Point_Constant - 2.0 = 1.0),
      --  "*" --
      P131 => 130 + Boolean'Pos (Modular_Constant * 2 = 6),
      P132 => 131 + Boolean'Pos (Modular_Constant * 3 = 1),
      P133 => 124 + Static_Constant * 3,
      P134 => 133 + Boolean'Pos (Float_Constant * 2.0 = 6.0),
--      P135 => 134 + Boolean'Pos (Fixed_Point_Constant * 2 = 6.0),
--      P136 => 135 + Boolean'Pos (Decimal_Point_Constant * 2 = 6.0),
--      P137 => 136 + Boolean'Pos (Fixed_Point_Constant = 1.5 * 2.0),
--      P138 => 137 + Boolean'Pos (Decimal_Point_Constant = 1.5 * 2.0),
      --  "/" --
      P139 => 138 + Boolean'Pos (Modular_Constant / 2 = 1),
      P140 => 139 + Boolean'Pos (Modular_Constant / 4 = 0),
      P141 => 140 + Static_Constant / 2,
      P142 => 141 + Boolean'Pos (Float_Constant / 2.0 = 1.5),
--      P143 => 142 + Boolean'Pos (Fixed_Point_Constant / 2 = 1.5),
--      P144 => 143 + Boolean'Pos (Decimal_Point_Constant / 2 = 1.5),
--      P145 => 144 + Boolean'Pos (Fixed_Point_Constant = 6.0 / 2.0),
--      P146 => 145 + Boolean'Pos (Decimal_Point_Constant = 6.0 / 2.0),
      --  "mod" --
      P147 => 146 + Boolean'Pos (Modular_Constant mod 2 = 1),
      P148 => 147 + Boolean'Pos (Static_Constant mod 2 = 1),
      P149 => 148 + Boolean'Pos ((-Static_Constant) mod 2 = 1),
      P150 => 149 + Boolean'Pos (Static_Constant mod (-2) = -1),
      P151 => 150 + Boolean'Pos ((-Static_Constant) mod (-2) = -1),
      --  "rem" --
      P152 => 151 + Boolean'Pos (Modular_Constant rem 2 = 1),
      P153 => 152 + Boolean'Pos (Static_Constant rem 2 = 1),
      P154 => 153 + Boolean'Pos ((-Static_Constant) rem 2 = -1),
      P155 => 154 + Boolean'Pos (Static_Constant rem (-2) = 1),
      P156 => 155 + Boolean'Pos ((-Static_Constant) rem (-2) = -1),
      --  "abs" --
      P157 => 156 + Boolean'Pos (abs (-Modular_Constant) = 5),
      P158 => 157 + Boolean'Pos (abs (-Static_Constant) = 3),
      P159 => 158 + Boolean'Pos (abs (-Float_Constant) = 3.0),
      P160 => 159 + Boolean'Pos (abs (-Fixed_Point_Constant) = 3.0),
      P161 => 160 + Boolean'Pos (abs (-Decimal_Point_Constant) = 3.0),
      --  "not" --
      P162 => 161 + Boolean'Pos (not False),
      P163 => 163 + Boolean'Pos (not True),
      P164 => 163 + Boolean'Pos ((not Modular_Constant) = 4),
      --  "**" --
      P165 => 164 + Boolean'Pos (Modular_Constant ** 3 = 3),
      P166 => 165 + Boolean'Pos (Static_Constant ** 3 = 27),
      P167 => 166 + Boolean'Pos (Float_Constant ** 3 = 27.0)
     );

   type ARM_4_9p7 is
     (A000, A001, A002, A003, A004, A005, A006, A007, A008--, A009,
--      A010, A011, A012, A013, A014, A015, A016, A017, A018, A019,
--      A020, A021, A022, A023, A024, A025, A026, A027, A028, A029,
--      A030, A031, A032, A033, A034, A035, A036, A037, A038, A039,
--      A040, A041, A042, A043, A044, A045, A046, A047, A048, A049,
--      A050, A051, A052, A053, A054, A055, A056, A057, A058, A059,
--      A060, A061, A062, A063, A064, A065, A066, A067, A068, A069,
--      A070, A071, A072, A073, A074, A075, A076, A077, A078, A079,
--      A080, A081, A082, A083, A084, A085, A086, A087, A088, A089,
--      A090, A091, A092, A093, A094, A095, A096, A097, A098, A099
     );

   subtype One_To_Five is Integer range 1 .. 5;

   type Real is digits 8;

   for ARM_4_9p7 use
     (A000 => Natural'First,
      A001 => One_To_Five'Last - 4,
      A002 => Modular_Type'Modulus - 6,
      A003 => Real'Digits - 5,  --  ARM 3.5.8 (2)
      A004 => 4,  --  ARM 3.5.10 (2)  FIXME
--      A004 => 3 + Boolean'Pos (Fixed_Point_Type'Small = 1.0 / 128.0),
      A005 => 4 + Boolean'Pos (Fixed_Point_Type'Delta = 0.01),--  3.5.10 (3)
      A006 => Fixed_Point_Type'Aft + 4,  --  3.5.10 (5)
      A007 => Decimal_Point_Type'Digits - 1,  --  3.5.10 (7)
      A008 => Decimal_Point_Type'Scale + 6  --  3.5.10 (11)
      );

   type ARM_4_9p9 is (C000, C001, C002, C003, C004, C005, C006);

   Float_3_5         : constant Float := 3.5;
   Fixed_Point_3_5   : constant Fixed_Point_Type := 3.5;
   Decimal_Point_3_5 : constant Decimal_Point_Type := 3.5;

   for ARM_4_9p9 use
     (C000 => Integer (Modular_Constant) - 3,
      C001 => 1, --Integer (Float_Constant) - 2,
      C002 => 2, --Integer (Float_3_5) - 2,
      C003 => 3, --Integer (Fixed_Point_3_5) - 1,
      C004 => 4, -- Integer (Decimal_Point_3_5)
      C005 => Boolean'Pos (Float (Fixed_Point_3_5) = 3.5) + 4,
      C006 => Boolean'Pos (Float (Decimal_Point_3_5) = 3.5) + 5
     );

   type ARM_4_9p22 is
     (D000, D001, D002, D003, D004, D005, D006, D007, D008, D009,
      D010, D011, D012, D013, D014, D015, D016, D017, D018, D019,
      D020, D021, D022, D023, D024--, D025, D026, D027, D028, D029,
--      D030, D031, D032, D033, D034, D035, D036, D037, D038, D039,
--      D040, D041, D042, D043, D044, D045, D046, D047, D048, D049,
--      D050, D051, D052, D053, D054, D055, D056, D057, D058, D059,
--      D060, D061, D062, D063, D064, D065, D066, D067, D068, D069,
--      D070, D071, D072, D073, D074, D075, D076, D077, D078, D079,
--      D080, D081, D082, D083, D084, D085, D086, D087, D088, D089,
--      D090, D091, D092, D093, D094, D095, D096, D097, D098, D099
     );

   for ARM_4_9p22 use
     (D000 => Boolean'Pos (Boolean'Min (True, False)),
      D001 => Boolean'Pos (Boolean'Max (True, False)),
      D002 => 1 + Boolean'Pos (Boolean'Succ (False)),
      D003 => 3 + Boolean'Pos (Boolean'Pred (True)),
      D004 => Integer'Min (4, 5),
      D005 => Integer'Max (0, 5),
      D006 => Integer'Succ (5),
      D007 => Integer'Pred (8),
      D008 => 7 + Boolean'Pos (Modular_Type'Min (4, 5) = 4),
      D009 => 8 + Boolean'Pos (Modular_Type'Max (4, 5) = 5),
      D010 => 9 + Boolean'Pos (Modular_Type'Succ (5) = 6),
      D011 => 10 + Boolean'Pos (Modular_Type'Pred (7) = 6),
      D012 => 11 + Boolean'Pos
        (Float'Min (Float_Constant, Float_3_5) = Float_Constant),
      D013 => 12 + Boolean'Pos
        (Float'Max (Float_Constant, Float_3_5) = Float_3_5),
      D014 => 14, -- 13 + Boolean'Pos (Float'Succ (Float_3_5) > Float_3_5),
      D015 => 15, --14 + Boolean'Pos (Float'Pred (Float_3_5) < Float_3_5),
      D016 => 16, -- 15 + Boolean'Pos
--        (Fixed_Point_Type'Min (Fixed_Point_Constant, Fixed_Point_3_5)
--           = Fixed_Point_Constant),
      D017 => 17, -- 16 + Boolean'Pos
--        (Fixed_Point_Type'Max (Fixed_Point_Constant, Fixed_Point_3_5)
--           = Fixed_Point_3_5),
      D018 => 18, -- 13 + Boolean'Pos (Float'Succ (Float_3_5) > Float_3_5),
      D019 => 19, --14 + Boolean'Pos (Float'Pred (Float_3_5) < Float_3_5)
      D020 => 20, -- 19 + Boolean'Pos (Modular_Type'Mod (11) = Modular_Constant)
      D021 => 20 + Boolean'Pos (Boolean'Val (0) = False),
      D022 => 21 + Boolean'Pos (Boolean'Val (1) = True),
      D023 => Integer'Val (23),
      D024 => 24
     );

end Example;
