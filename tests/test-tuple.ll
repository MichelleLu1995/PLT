; ModuleID = 'JSTEM'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%c\0A\00"

declare i32 @printf(i8*, ...)

declare i8* @puts(i8*, ...)

declare i8* @fopen(i8*, i8*)

declare i32 @fclose(i8*)

declare i32 @fread(i8*, i32, i32, i8*)

declare i8* @fgets(i8*, i32, i8*)

declare i32 @fwrite(i8*, i32, i32, i8*)

declare i32 @strlen(i8*)

define void @main() {
entry:
  %t = alloca [8 x i32]
  %j = alloca i32
  store [8 x i32] [i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7, i32 8], [8 x i32]* %t
  store i32 0, i32* %j
  br label %while

while:                                            ; preds = %while_body, %entry
  %j5 = load i32* %j
  %tmp6 = icmp slt i32 %j5, 8
  br i1 %tmp6, label %while_body, label %merge

while_body:                                       ; preds = %while
  %j1 = load i32* %j
  %t2 = getelementptr [8 x i32]* %t, i32 0, i32 %j1
  %t3 = load i32* %t2
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), i32 %t3)
  %j4 = load i32* %j
  %tmp = add i32 %j4, 1
  store i32 %tmp, i32* %j
  br label %while

merge:                                            ; preds = %while
  ret void
}
