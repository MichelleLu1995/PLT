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
  %m = alloca [3 x [3 x i32]]
  %i = alloca i32
  %j = alloca i32
  store [3 x [3 x i32]] [[3 x i32] [i32 1, i32 2, i32 3], [3 x i32] [i32 4, i32 5, i32 6], [3 x i32] [i32 7, i32 8, i32 9]], [3 x [3 x i32]]* %m
  store i32 0, i32* %i
  br label %while

while:                                            ; preds = %merge, %entry
  %i12 = load i32* %i
  %tmp13 = icmp slt i32 %i12, 3
  br i1 %tmp13, label %while_body, label %merge14

while_body:                                       ; preds = %while
  store i32 0, i32* %j
  br label %while1

while1:                                           ; preds = %while_body2, %while_body
  %j8 = load i32* %j
  %tmp9 = icmp slt i32 %j8, 3
  br i1 %tmp9, label %while_body2, label %merge

while_body2:                                      ; preds = %while1
  %i3 = load i32* %i
  %j4 = load i32* %j
  %m5 = getelementptr [3 x [3 x i32]]* %m, i32 0, i32 %i3, i32 %j4
  %m6 = load i32* %m5
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), i32 %m6)
  %j7 = load i32* %j
  %tmp = add i32 %j7, 1
  store i32 %tmp, i32* %j
  br label %while1

merge:                                            ; preds = %while1
  %i10 = load i32* %i
  %tmp11 = add i32 %i10, 1
  store i32 %tmp11, i32* %i
  br label %while

merge14:                                          ; preds = %while
  ret void
}
