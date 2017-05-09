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
  %r = alloca [5 x i32]
  %i = alloca i32
  store [5 x i32] [i32 0, i32 1, i32 2, i32 3, i32 4], [5 x i32]* %r
  store i32 0, i32* %i
  br label %while

while:                                            ; preds = %while_body, %entry
  %i5 = load i32* %i
  %tmp6 = icmp slt i32 %i5, 5
  br i1 %tmp6, label %while_body, label %merge

while_body:                                       ; preds = %while
  %i1 = load i32* %i
  %r2 = getelementptr [5 x i32]* %r, i32 0, i32 %i1
  %r3 = load i32* %r2
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), i32 %r3)
  %i4 = load i32* %i
  %tmp = add i32 %i4, 1
  store i32 %tmp, i32* %i
  br label %while

merge:                                            ; preds = %while
  ret void
}
