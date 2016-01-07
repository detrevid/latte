; ModuleID = '<stdin>'

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }

@stdin = external global %struct._IO_FILE*
@dnl = internal constant [4 x i8] c"%d\0A\00"
@fnl = internal constant [6 x i8] c"%.1f\0A\00"
@d = internal constant [3 x i8] c"%d\00"
@lf = internal constant [4 x i8] c"%lf\00"
@str.error = internal constant [14 x i8] c"runtime error\00"

declare i32 @printf(i8*, ...)

declare i32 @scanf(i8*, ...)

declare i32 @puts(i8*)

declare void @exit(i32)

declare i64 @getline(i8**, i64*, %struct._IO_FILE*)

declare noalias i8* @malloc(i64)

declare i64 @strlen(i8* nocapture)

declare i8* @strcat(i8*, i8* nocapture readonly)

define void @error() {
entry:
  %0 = tail call i32 @puts(i8* getelementptr inbounds ([14 x i8]* @str.error, i64 0, i64 0))
  tail call void @exit(i32 0)
  unreachable
}

define void @printInt(i32 %x) {
entry:
  %t0 = getelementptr [4 x i8]* @dnl, i32 0, i32 0
  %0 = call i32 (i8*, ...)* @printf(i8* %t0, i32 %x)
  ret void
}

define void @printString(i8* %s) {
entry:
  %0 = call i32 @puts(i8* %s)
  ret void
}

define i32 @readInt() {
entry:
  %res = alloca i32
  %t1 = getelementptr [3 x i8]* @d, i32 0, i32 0
  %0 = call i32 (i8*, ...)* @scanf(i8* %t1, i32* %res)
  %t2 = load i32* %res
  ret i32 %t2
}

define i8* @readString() {
  %line = alloca i8*, align 8
  %size = alloca i64, align 8
  store i8* null, i8** %line, align 8
  br label %1

; <label>:1                                       ; preds = %17, %0
  %2 = load i8** %line, align 8
  %3 = icmp ne i8* %2, null
  br i1 %3, label %4, label %10

; <label>:4                                       ; preds = %1
  %5 = load i8** %line, align 8
  %6 = getelementptr inbounds i8* %5, i64 0
  %7 = load i8* %6, align 1
  %8 = sext i8 %7 to i32
  %9 = icmp eq i32 %8, 10
  br label %10

; <label>:10                                      ; preds = %4, %1
  %11 = phi i1 [ true, %1 ], [ %9, %4 ]
  br i1 %11, label %12, label %18

; <label>:12                                      ; preds = %10
  %13 = load %struct._IO_FILE** @stdin, align 8
  %14 = call i64 @getline(i8** %line, i64* %size, %struct._IO_FILE* %13)
  %15 = icmp eq i64 %14, -1
  br i1 %15, label %16, label %17

; <label>:16                                      ; preds = %12
  call void @error()
  br label %17

; <label>:17                                      ; preds = %16, %12
  br label %1

; <label>:18                                      ; preds = %10
  %19 = load i8** %line, align 8
  %20 = call i64 @strlen(i8* %19) #6
  %21 = sub i64 %20, 1
  %22 = load i8** %line, align 8
  %23 = getelementptr inbounds i8* %22, i64 %21
  store i8 0, i8* %23, align 1
  %24 = load i8** %line, align 8
  ret i8* %24
}

define i8* @concatString(i8* %str1, i8* %str2) {
  %1 = alloca i8*, align 8
  %2 = alloca i8*, align 8
  %new_str = alloca i8*, align 8
  store i8* %str1, i8** %1, align 8
  store i8* %str2, i8** %2, align 8
  %3 = load i8** %1, align 8
  %4 = call i64 @strlen(i8* %3) #6
  %5 = load i8** %2, align 8
  %6 = call i64 @strlen(i8* %5) #6
  %7 = add i64 %4, %6
  %8 = add i64 %7, 1
  %9 = call noalias i8* @malloc(i64 %8) #7
  store i8* %9, i8** %new_str, align 8
  %10 = icmp ne i8* %9, null
  br i1 %10, label %11, label %20

; <label>:11                                      ; preds = %0
  %12 = load i8** %new_str, align 8
  %13 = getelementptr inbounds i8* %12, i64 0
  store i8 0, i8* %13, align 1
  %14 = load i8** %new_str, align 8
  %15 = load i8** %1, align 8
  %16 = call i8* @strcat(i8* %14, i8* %15) #7
  %17 = load i8** %new_str, align 8
  %18 = load i8** %2, align 8
  %19 = call i8* @strcat(i8* %17, i8* %18) #7
  br label %21

; <label>:20                                      ; preds = %0
  call void @error()
  br label %21

; <label>:21                                      ; preds = %20, %11
  %22 = load i8** %new_str, align 8
  ret i8* %22
}
