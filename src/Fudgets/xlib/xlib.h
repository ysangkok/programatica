#define MyDefaultGC	0

#define GCAllComponents	((1<<(GCLastBit+1))-1)

/* Same order as in Command.hs !! */
typedef enum {
   XRqOpenDisplay
 , XRqCreateSimpleWindow
 , XRqCreateRootWindow
 , XRqCreateGC
 , XRqLoadFont
 , XRqCreateFontCursor
 , XRqGrabPointer 
 , XRqQueryFont
 , XRqAllocNamedColor
 , XRqAllocColor
 , XRqCreatePixmap
 , XRqReadBitmapFile
 , XRqCreateBitmapFromData
 , XRqRmGetStringDatabase
 , XRqRmGetResource
 , XRqTranslateCoordinates
 , XRqInternAtom
 , XRqGetWindowProperty
 , XRqQueryPointer
 , XRqQueryFontList /* !! */
 , XRqLoadQueryFontList
 , XRqQueryColor
 , XRqQueryTree
 , XRqDefaultRootWindow
 , XRqGetGeometry
 , XRqDefaultVisual
 , XRqSync
 , XRqQueryTextExtents16
 , XRqListFonts
 , XRqListFontsWithInfo
 /* , XRqGetResource */
} XRequestConstr;

/* Same order as in Command.hs !! */
typedef enum {
   XCCloseDisplay
 , XCDestroyWindow
 , XCMapRaised
 , XCLowerWindow
 , XCUnmapWindow
 , XCDraw
 , XCDrawMany
 , XCClearArea
 , XCClearWindow
 , XCChangeGC
 , XCFreeGC
 , XCChangeWindowAttributes
 , XCConfigureWindow
 , XCStoreName
 , XCSetNormalHints
 , XCSetWMHints
 , XCUngrabPointer
 , XCGrabButton
 , XCUngrabButton
 , XCFlush
 , XCFreePixmap
 , XCShapeCombineMask
 , XCShapeCombineRectangles
 , XCShapeCombineShape
 , XCRmDestroyDatabase
 , XCRmCombineDatabase
 , XCRmPutLineResource
 , XCSetWMProtocols
 , XCSendEvent
 , XCSetSelectionOwner
 , XCConvertSelection
 , XCChangeProperty
 , XCFreeColors
 , XCReparentWindow
 , XCWarpPointer
 , XCSetRegion
 , XCAddToSaveSet
 , XCRemoveFromSaveSet
 , XCBell
 , XCSetGCWarningHack
} XCommandConstr;

/* Same order as in Command.hs !! */
typedef enum {
  XDCDrawLine,
  XDCDrawImageString,
  XDCDrawString,
  XDCDrawRectangle,
  XDCFillRectangle,
  XDCFillPolygon,
  XDCDrawArc, XDCFillArc,
  XDCCopyArea,
  XDCCopyPlane,
  XDCDrawPoint,
  XDCCreatePutImage,
  XDCDrawImageStringPS,
  XDCDrawStringPS,
  XDCDrawLines,
  XDCDrawImageString16,
  XDCDrawString16
} XDrawCommandConstr;

typedef enum {
  XDrMyWindow,
  XDrPixmap
} XDrawableConstr;

extern int debug;

/* Same order as in Event.hs !! */
typedef enum {
   XEFocusIn
 , XEFocusOut
 , XEKeymapNotify
 , XEGraphicsExpose
 , XEKeyEvent
 , XEButtonEvent
 , XEMotionNotify
 , XEEnterNotify
 , XELeaveNotify
 , XEExpose
 , XENoExpose
 , XEVisibilityNotify
 , XECreateNotify
 , XEDestroyNotify
 , XEUnmapNotify
 , XEMapNotify
 , XEMapRequest
 , XEReparentNotify
 , XEConfigureNotify
 , XEConfigureRequest
 , XEGravityNotify
 , XEResizeRequest
 , XECirculateNotify
 , XECirculateRequest
 , XEPropertyNotify
 , XESelectionClear
 , XESelectionRequest
 , XESelectionNotify
 , XEColormapNotify
 , XEClientMessage
 , XEMappingNotify
} XEventConstr;

#define XEKeyPress XEKeyEvent
#define XEKeyRelease XEKeyEvent
#define XEButtonPress XEButtonEvent
#define XEButtonRelease XEButtonEvent

/* Same order as in Event.hs !! */
typedef enum {
   XRDisplayOpened
 , XRWindowCreated
 /*, XRWindowDestroyed*/
 , XRGCCreated
 , XRCursorCreated
 , XRPointerGrabbed
 , XRFontLoaded
 , XRFontQueried
 , XRColorAllocated
 , XRPixmapCreated
 , XRBitmapRead
 , XRRmDatabaseCreated
 , XRGotResource
 , XRCoordinatesTranslated
 , XRGotAtom
 , XRGotEvent
 , XRGotWindowProperty
 , XRPointerQueried
 , XRFontQueriedList
 , XRColorQueried
 , XRTreeQueried
 , XRGotDefaultRootWindow
 , XRGotGeometry
 , XRGotVisual
 , XRSynced
 , XRTextExtents16Queried
 , XRGotFontList
 , XRGotFontListWithInfo
} XResponseConstr;
