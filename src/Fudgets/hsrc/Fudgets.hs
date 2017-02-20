#include "exists.h"
module Fudgets(Alignment(..), buttonF, border3dF, buttonBorderF, pushButtonF,
 BMevents(..), inputPopupOptF, inputPopupF,
 passwdPopupOptF, passwdPopupF, stringPopupOptF, stringPopupF,
 confirmPopupF, ConfirmMsg(..), oldConfirmPopupF, oldMessagePopupF,
 messagePopupF, intDispF, displayF, labelF, EditStop(..), editF,
 EditEvt(..), EditCmd(..), editorF, editorF', selectall, loadEditor,
 newline, EDirection(..), inputEditorF, inputEditorF', EditorF,
 EditStopFn(..), EditStopChoice(..), IsSelect(..),setEditorCursorPos,
 oldFilePickF, PickListRequest(..),
 textF, textF',-- textF'',
 ListRequest(..),
 replaceAll, replaceAllFrom, deleteItems, insertItems, appendItems,
 changeItems, replaceItems, highlightItems, pickItem, applyListRequest, 
 TextRequest(..), TextF, HasInitText(..), HasSizing(..),Sizing(..),
 smallPickListF, labRightOfF, labLeftOfF, labBelowF, labAboveF,
 tieLabelF, menuF, PopupMenu(..), menuPopupF,
 pickListF, moreShellF,pickListF', moreShellF',
 hyperGraphicsF, hyperGraphicsF',GraphicsF,setAdjustSize,
 moreFileShellF, moreFileF, moreF, moreF',
 popupMenuF, Click(..), quitButtonF,
 quitIdF, quitF, radioGroupF, radioGroupF', intF, passwdF, stringF,
 intInputF, stringInputF, passwdInputF,
 intInputF', stringInputF', passwdInputF',
 terminalF, cmdTerminalF, TerminalCmd(..),
 toggleButtonF, contDynF,
 DynFMsg(..), dynF, dynListF, DynMsg(..), Either(..),
 Fudget(..), F, --F(..),
 FudgetIO(..),
 --Fa(..),Direction, TCommand(..),TEvent(..),

 {- FCommand(..), FEvent(..), K(..), KCommand(..), KEvent(..),
 TCommand(..), TEvent(..), -}

 InF(..), inputDoneSP, inputLeaveDoneSP,
 inputListSP, inputPairSP, inputThroughF, inputPairF, inputListF, inputChange,
 inputListLF, inputPairLF, -- obsolete
 stripInputSP, InputMsg(..), Orientation(..), listF,
 untaggedListF, loopCompF, loopCompSP, loopF, loopLeftF, loopRightF, loopOnlyF,
 loopThroughRightF, loopCompThroughLeftF, loopCompThroughRightF,
 loopThroughBothF,
 Message, delayF,
 
 getF, putF, putsF, startupF, appendStartF, 

 nullF, parF, prodF, absF, bypassF, concatMapF, idF, idLeftF,
 idRightF, mapF, mapstateF, serCompLeftToRightF, serCompRightToLeftF, stubF,
 throughF, toBothF, (>*<), (>+<), (>=^<), (>=^^<),
 (>#+<), (>#==<), (>==<), (>^=<), (>^^=<),
 prepostMapHigh,
 (-+-),(-*-),(-==-),

 alignF, marginHVAlignF, layoutModifierF, noStretchF,
 marginF, sepF,

 autoP, flipP, permuteP, revP, idP,
 aBottom, aCenter,
 aLeft, aRight, aTop,
 -- barP, rightBelowP,
 dynListLF, LayoutDir(..),
 listLF, nullLF, holeF,
 --rbLayoutF,
 untaggedListLF, LayoutRequest,
 Placer, center, center', fixedh, fixedv, flipPoint,
 flipRect, flipReq,

 NameLayout, LName(..),hvAlignNL, marginHVAlignNL, hBoxNL, hBoxNL',
 nullNL, leafNL, spaceNL, placeNL, 
 listNF, modNL, nameF, nameLayoutF, sepNL, marginNL, vBoxNL, vBoxNL',

 hBoxF, matrixF, placerF, spacerF, spacer1F, revHBoxF, revVBoxF, spacerP,
 tableF, vBoxF, horizontalP, horizontalP', matrixP, matrixP',
 verticalP, verticalP',paragraphP,paragraphP',paragraphP'',
 dynPlacerF, dynSpacerF, -- There are pitfalls with using these...

 Distance(..), Spacer, bottomS, centerS, compS, flipS,
 hAlignS, sizeS, maxSizeS, minSizeS, hCenterS, hMarginS, marginHVAlignS,
 hvAlignS, hvMarginS, idS, leftS, marginS, sepS, noStretchS,
 rightS, topS, vAlignS, vCenterS, vMarginS, tableP, tableP', bubbleF,
 bubblePopupF, bubbleRootPopupF, shellF, PotRequest(..), PotState(..),
 containerGroupF, hPotF, vPotF, popupShellF, popupShellF', PopupMsg(..),
 posPopupShellF, hScrollF, scrollF, scrollShellF, vScrollF,
 ESelCmd(..), ESelEvt(..), SelCmd(..), SelEvt(..), eselectionF,
 selectionF, allcacheF,
 {-
 bitmapdatacacheF, bitmapfilecacheF,
 colorcacheF, fontcacheF, fontcursorcacheF, fstructcacheF, gCcacheF,
 -}
 doubleClickF, Time(..), compEitherSP, idLeftSP, idRightSP, postMapSP, preMapSP,
 prepostMapSP, serCompSP, loopLeftSP, loopSP, loopOnlySP, loopThroughRightSP,
 loopThroughBothSP,
 parSP, seqSP, SP, StreamProcIO(..),

 {-SPm(..), bindSPm, getSPm, monadSP, nullSPm, putsSPm, thenSPm, toSPm, unitSPm,-}

 filterJustSP, filterLeftSP, filterRightSP, mapFilterSP, splitSP,
 toBothSP, appendStartSP, chopSP, concSP, concatMapAccumlSP,
 concatMapSP, concatSP, concmapSP, delaySP, feedSP, filterSP, getSP,
 idSP, mapAccumlSP, mapSP, mapstateSP, nullSP, pullSP, putSP, putsSP,
 runSP, splitAtElemSP, startupSP, stepSP, walkSP, zipSP, hIOF,
 hIOSuccF, hIOerrF, haskellIOF, inputLinesSP, linesSP, outputF, stderrF, stdinF,
 stdioF, stdoutF, subProcessF, readDirF, readFileF, writeFileF, openLSocketF,
 openSocketF, receiverF, transceiverF, transmitterF, asyncTransmitterF,
 asyncTransceiverF, Tick(..), timerF,
 cmdContSP, conts, getLeftSP, getRightSP, waitForSP, waitForF, dropSP, contMap,
 --dropF,
 fudlogue, fudlogue', Fudlogue, Host(..), LSocket(..), Peer(..), Port(..), Socket(..),
 Button(..), ColorName(..), FontName(..), KeySym(..), FontStruct, RGB(..),
 WindowAttributes, 
 ModState(..), Modifiers(..), 
 inputButtonKey, inputLeaveKey, inputMsg, mapInp, stripInputMsg,
 inputDone, inputLeaveDone,
 tstInp, argFlag, argKey, argReadKey, argKeyList, args, progName,
 bgColor, buttonFont, defaultFont, defaultSize,
 defaultPosition, defaultSep, edgeWidth, fgColor, labelFont, look3d,
 menuFont, options, paperColor, shadowColor, shineColor,
 filterLeft, filterRight, isLeft, isRight, mapEither,
 fromLeft, fromRight, plookup, splitEitherList, stripEither, stripLeft,
 --  mapfilter, isM, stripMaybe, stripMaybeDef,
     -- use Maybe.mapMaybe,isJust,fromJust,fromMaybe!
 stripRight, swapEither,(=.>),
 Line(..), Point(..), Rect(..), Size(..), confine, diag, freedom,
 growrect, inRect, lL, line2rect, moveline, moverect, origin, pMax,
 pMin, pP, padd, plim, pmax, pmin, posrect, psub, rR, rect2line,
 rectMiddle,
 rmax, rsub, scale, scalePoint, sizerect,
 --xcoord, ycoord, rectpos, rectsize,
 aboth, anth, gmap, -- afst, asnd, dropto, 
 issubset, lhead, loop, lsplit, ltail, mapPair, number, oo, pair,
 pairwith, part, remove, replace, swap, unionmap, module FudVersion,

 Cont(..),
 XCommand, XEvent, Path(..),CoordMode(..),Shape(..),
 DrawCommand(..),fillCircle,drawCircle,
 Graphic(..),
 Drawing(..),atomicD,labelD,up,boxD,hboxD,hboxD',vboxD,vboxD',tableD,tableD',
 hboxcD,hboxcD',vboxlD,vboxlD',matrixD,matrixD',
 attribD,softAttribD,hardAttribD,fontD,fgD,stackD,spacedD,placedD,
 blankD,filledRectD,rectD,
 DPath(..),
#ifdef USE_EXIST_Q
 Gfx,
#endif
 g,
 FixedDrawing(..),FixedColorDrawing(..),gctx2gc,
 FlexibleDrawing(..),flex,flex',
 filler,hFiller,vFiller,frame,frame',ellipse,ellipse',arc,arc',
 filledEllipse,filledEllipse',filledarc,filledarc',
 lpar,rpar,lbrack,rbrack,lbrace,rbrace,
 triangleUp,triangleDown,filledTriangleUp,filledTriangleDown,
 BitmapFile(..),
 ColorGen(..),FontGen(..),FontSpec,ColorSpec,--Name(..),--ColorFallback(..),
#ifdef USE_EXIST_Q
 colorSpec,fontSpec,
#endif
 GCtx,rootGCtx,wCreateGCtx,createGCtx,gcFgA,gcBgA,gcFontA,
 GCAttributes(..),GCFillStyle(..),GCCapStyle(..),GCLineStyle(..),GCFunction(..),
 Width(..),

 Customiser(..), PF(..), standard,

 HasClickToType(..), HasVisible(..), HasFontSpec(..), setFont,
 HasKeys(..), HasWinAttr(..), --HasTitle(..),
 HasBorderWidth(..), HasBgColorSpec(..), HasFgColorSpec(..), HasMargin(..),
 setBgColor,setFgColor,
 HasAlign(..),
 {- HasAllowedChar(..), HasShowString(..), -}
 setAllowedChar, setShowString, setCursorPos,
 HasCache(..),
 setDeleteQuit,setDeleteWindowAction,DeleteWindowAction(..),--HasDeleteQuit(..),
 HasInitSize(..),
 HasInitDisp(..),
 --setInitDisp, getInitDisp,
 setSpacer,
 HasStretchable(..),
 HasLabelInside(..),setPlacer,

 ShellF, shellF', setInitPos,
 unmappedSimpleShellF, unmappedSimpleShellF',
 ButtonF, buttonF', buttonF'', setLabel,
 DisplayF, displayF',-- displayF'',
 labelF', --labelF'',
 StringF, stringF', stringF'', setInitString, setInitStringSize,
 passwdF', passwdF'',
 intF', intF'',
 intDispF', --intDispF'',
 gcWarningF,bellF,
 --D_IOError,

 getTime,getLocalTime,

 spyF,teeF, ctrace,showCommandF) where

import AllFudgets -- hiding (Cont,PCont,Fa)
import FudVersion

-- I hate this file /TH
-- I still hate this file /TH
