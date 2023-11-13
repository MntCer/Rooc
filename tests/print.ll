; ModuleID = 'Rooc'
source_filename = "Rooc"

@fmt = private unnamed_addr constant [12 x i8] c"Hello world\00", align 1

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @fmt, i32 0, i32 0))
  ret i32 0
}
