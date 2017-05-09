; ModuleID = 'JSTEM'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%c\0A\00"
@string = private unnamed_addr constant [3 x i8] c"hi\00"

declare i32 @printf(i8*, ...)

declare i8* @puts(i8*, ...)

declare i8* @fopen(i8*, i8*)

declare i32 @fclose(i8*)

declare i32 @fread(i8*, i32, i32, i8*)

declare i8* @fgets(i8*, i32, i8*)

declare i32 @fwrite(i8*, i32, i32, i8*)

declare i32 @strlen(i8*)

define i32 @main() {
entry:
  %a = alloca i8*
  store i8* getelementptr inbounds ([3 x i8]* @string, i32 0, i32 0), i8** %a
  %a1 = load i8** %a
  %puts = call i8* (i8*, ...)* @puts(i8* %a1)
  ret i32 0
}
