
/* type Request */
#define ReadFile 0
#define WriteFile 1
#define AppendFile 2
#define ReadBinFile 3
#define WriteBinFile 4
#define AppendBinFile 5
#define DeleteFile 6
#define StatusFile 7
#define ReadChan 8
#define AppendChan 9
#define ReadBinChan 10
#define AppendBinChan 11
#define StatusChan 12
#define Echo 13
#define GetArgs 14
#define GetEnv 15
#define SetEnv 16
#define ReadChannels 17
#define ReadBinChannels 18
#define CreateProcess 19
#define CreateDirectory 20
#define OpenFile 21
#define OpenBinFile 22
#define CloseFile 23
#define ReadVal 24
#define ReadBinVal 25
#define WriteVal 26
#define WriteBinVal 27
/* extra */
#define Sleep 28
#define ChangeDirectory 29
#define GetTime 30
#define DeleteDirectory 31
#define System 32
#define ReadDirectory 33
#define XCommand 34
#define GetAsyncInput 35
#define GetCpuTime 36
#define GetProgName 37
#define GetLocalTime 38
#define SigAction 39
#define Exit 40
#define ReadFileScattered 41
#define Select 42
#define SocketRequest 43
#define XRequest 44
#define ReadFileFast 45

#define XMKRESP 0x80000000

/* type Response */
#define RSuccess 0
#define Str 1
#define Bn 2
#define Failure 3
#define Tagg 4
#define BinTag 5
#define StrList 6
#define Fil 7
#define Dbl 8
#define AsyncInput 9
#define SocketResponse 10
#define XResponse 11

/* type IOError */
#define WriteError 0
#define ReadError 1
#define SearchError 2
#define FormatError 3
#define OtherError 4

#define SAIgnore 0
#define SADefault 1
#define SACatch 2
