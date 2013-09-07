package Ag.Shift_Reduce is

    type Small_Integer is range -32_000 .. 32_000;

    type Shift_Reduce_Entry is record
        T   : Small_Integer;
        Act : Small_Integer;
    end record;
    pragma Pack(Shift_Reduce_Entry);

    subtype Row is Integer range -1 .. Integer'Last;

  --pragma suppress(index_check);

    type Shift_Reduce_Array is array (Row  range <>) of Shift_Reduce_Entry;

    Shift_Reduce_Matrix : constant Shift_Reduce_Array :=
        ( (-1,-1) -- Dummy Entry

-- state  0
,( 3, 13),( 4, 14),( 6, 16),( 7, 15)
,( 8, 17),( 10, 10),( 15, 11),(-1,-3000)

-- state  1
,( 3, 13),( 4, 14),( 6, 16),( 7, 15)
,( 8, 17),( 10, 10),( 15, 11),(-1,-1)

-- state  2
,(-1,-2)
-- state  3
,(-1,-4)
-- state  4
,(-1,-5)
-- state  5
,(-1,-6)

-- state  6
,(-1,-7)
-- state  7
,(-1,-8)
-- state  8
,(-1,-9)
-- state  9
,(-1,-10)

-- state  10
,( 8, 17),(-1,-3000)
-- state  11
,( 8, 17),(-1,-3000)

-- state  12
,( 2, 22),(-1,-3000)
-- state  13
,( 5, 23),(-1,-3000)

-- state  14
,( 5, 24),(-1,-3000)
-- state  15
,( 8, 17),(-1,-3000)

-- state  16
,( 8, 26),(-1,-3000)
-- state  17
,(-1,-36)
-- state  18
,( 0,-3001)
,(-1,-3000)
-- state  19
,(-1,-3)
-- state  20
,( 7, 29),( 17, 28)
,(-1,-3000)
-- state  21
,( 17, 30),(-1,-3000)
-- state  22
,( 8, 17)
,( 19, 33),( 23, 38),( 25, 39),(-1,-3000)

-- state  23
,( 8, 17),(-1,-3000)
-- state  24
,( 8, 17),(-1,-3000)

-- state  25
,( 9, 45),(-1,-3000)
-- state  26
,( 8, 17),(-1,-3000)

-- state  27
,(-1,-3000)
-- state  28
,(-1,-11)
-- state  29
,( 9, 45),(-1,-3000)

-- state  30
,(-1,-13)
-- state  31
,(-1,-15)
-- state  32
,( 17, 49),( 18, 50)
,(-1,-3000)
-- state  33
,( 8, 17),(-1,-3000)
-- state  34
,( 8, 17)
,( 23, 38),( 25, 39),(-1,-18)
-- state  35
,(-1,-19)

-- state  36
,( 21, 53),(-1,-21)
-- state  37
,(-1,-23)
-- state  38
,( 8, 17)
,( 19, 33),( 23, 38),( 25, 39),(-1,-3000)

-- state  39
,( 8, 17),( 19, 33),( 23, 38),( 25, 39)
,(-1,-3000)
-- state  40
,( 27, 56),( 28, 57),(-1,-3000)

-- state  41
,(-1,-34)
-- state  42
,(-1,-26)
-- state  43
,( 27, 58),( 28, 57)
,(-1,-3000)
-- state  44
,(-1,-27)
-- state  45
,(-1,-37)
-- state  46
,( 8, 17)
,(-1,-3000)
-- state  47
,( 27, 60),( 28, 57),(-1,-3000)

-- state  48
,( 8, 17),(-1,-3000)
-- state  49
,(-1,-14)
-- state  50
,( 8, 17)
,( 19, 33),( 23, 38),( 25, 39),(-1,-3000)

-- state  51
,( 20, 63),(-1,-3000)
-- state  52
,(-1,-20)
-- state  53
,( 8, 17)
,(-1,-3000)
-- state  54
,( 18, 50),( 24, 65),(-1,-3000)

-- state  55
,( 18, 50),( 26, 66),(-1,-3000)
-- state  56
,( 8, 17)
,(-1,-3000)
-- state  57
,( 8, 17),(-1,-3000)
-- state  58
,( 8, 17)
,(-1,-3000)
-- state  59
,( 17, 71),(-1,-3000)
-- state  60
,( 11, 72)
,(-1,-3000)
-- state  61
,( 17, 73),(-1,-3000)
-- state  62
,(-1,-16)

-- state  63
,( 8, 17),( 23, 38),( 25, 39),(-1,-3000)

-- state  64
,( 22, 75),(-1,-3000)
-- state  65
,(-1,-24)
-- state  66
,(-1,-25)

-- state  67
,(-1,-30)
-- state  68
,( 27, 76),(-1,-3000)
-- state  69
,(-1,-35)

-- state  70
,( 27, 77),(-1,-3000)
-- state  71
,(-1,-33)
-- state  72
,( 12, 78)
,(-1,-3000)
-- state  73
,(-1,-12)
-- state  74
,( 8, 17),( 23, 38)
,( 25, 39),(-1,-17)
-- state  75
,(-1,-22)
-- state  76
,( 8, 17)
,(-1,-3000)
-- state  77
,( 8, 17),(-1,-3000)
-- state  78
,(-1,-32)

-- state  79
,( 13, 82),(-1,-3000)
-- state  80
,( 17, 83),( 28, 57)
,(-1,-3000)
-- state  81
,( 17, 84),( 28, 57),(-1,-3000)

-- state  82
,(-1,-31)
-- state  83
,(-1,-28)
-- state  84
,(-1,-29)
);
--  The offset vector
SHIFT_REDUCE_OFFSET : constant array (0.. 84) of Integer :=
( 0,
 8, 16, 17, 18, 19, 20, 21, 22, 23, 24,
 26, 28, 30, 32, 34, 36, 38, 39, 41, 42,
 45, 47, 52, 54, 56, 58, 60, 61, 62, 64,
 65, 66, 69, 71, 75, 76, 78, 79, 84, 89,
 92, 93, 94, 97, 98, 99, 101, 104, 106, 107,
 112, 114, 115, 117, 120, 123, 125, 127, 129, 131,
 133, 135, 136, 140, 142, 143, 144, 145, 147, 148,
 150, 151, 153, 154, 158, 159, 161, 163, 164, 166,
 169, 172, 173, 174);
end Ag.Shift_Reduce;