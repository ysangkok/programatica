/* Should match with SocketRequest in xtypes/Sockets.hs */
typedef enum {
      SRqOpenLSocket
    , SRqOpenSocket
    , SRqWriteSocket
    , SRqCloseSocket
    , SRqCloseLSocket
    , SRqGetStdinSocket
    , SRqCreateTimer
    , SRqDestroyTimer
    , SRqGetLSocketName
    , SRqGetSocketName
    , SRqStartProcess
    , SRqDLOpen
    , SRqDLClose
    , SRqDLSym
    , SRqOpenFileAsSocket
    , SRqWriteSocketPS
    , SRqGetStdoutSocket
} SocketRequestConstr;

/* Should match with SocketResponse in xtypes/Sockets.hs */
typedef  enum {
      SRLSocket
    , SRSocket
    , SRTimer
    , SRProcessSockets
    , SRDLHandle
    , SRDLVal
    , SRWrote
} SocketResponseConstr;

/* Should match with Descriptor in xtypes/Sockets.hs */
typedef enum {
      LSocketDe
    , SocketDe
    , OutputSocketDe
    , TimerDe
    , DisplayDe
      /* Pseudo constructors below, only used in the C code. */
    , InputOutputSocketDe
} DescriptorConstr;

/* Should match with AEvent in xtypes/Sockets.hs */
typedef enum {
      ESocketAccepted
    , ESocketRead
    , ESocketWritable
    , ETimerAlarm
    , EXEvent
} EventConstr;
