; ModuleID = 'a22006b.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct._ada_string = type { i8*, i32, i32 }

@s1 = internal global %struct._ada_string { i8* getelementptr inbounds ([8 x i8]* @.str1, i32 0, i32 0), i32 1, i32 7 }, align 8
@s2 = internal global %struct._ada_string { i8* getelementptr inbounds ([39 x i8]* @.str, i32 0, i32 0), i32 1, i32 38 }, align 8
@.str = private unnamed_addr constant [39 x i8] c"CHECK USE OF HT IN AND OUT OF COMMENTS\00", align 1
@.str1 = private unnamed_addr constant [8 x i8] c"A22006B\00", align 1

; Function Attrs: nounwind uwtable
define void @A22006B() #0 {
  call void @report__test(%struct._ada_string* @s1, %struct._ada_string* @s2)
  call void (...)* @report__result()
  ret void
}

declare void @report__test(%struct._ada_string*, %struct._ada_string*) #1

declare void @report__result(...) #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
