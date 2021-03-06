(in-package :cl-sdl2)

(define-foreign-library cl-sdl2-ttf
  (:unix "libSDL2_ttf.so")
  (t (:default "libSDL2_ttf")))

(use-foreign-library cl-sdl2-ttf)

(DEFCFUN "TTF_Linked_Version" :POINTER)
(DEFCFUN "TTF_ByteSwappedUNICODE" :VOID (SWAPPED :INT))
(DEFCFUN "TTF_Init" :INT)
(DEFCFUN "TTF_OpenFont" :POINTER (FILE :STRING) (PTSIZE :INT))
(DEFCFUN "TTF_OpenFontIndex" :POINTER (FILE :STRING) (PTSIZE :INT)
         (INDEX :LONG))
(DEFCFUN "TTF_OpenFontRW" :POINTER (SRC :pointer) (FREESRC :INT)
         (PTSIZE :INT))
(DEFCFUN "TTF_OpenFontIndexRW" :POINTER (SRC :pointer)
         (FREESRC :INT) (PTSIZE :INT) (INDEX :LONG))
(DEFCFUN "TTF_GetFontStyle" :INT (FONT :POINTER))
(DEFCFUN "TTF_SetFontStyle" :VOID (FONT :POINTER) (STYLE :INT))
(DEFCFUN "TTF_GetFontOutline" :INT (FONT :POINTER))
(DEFCFUN "TTF_SetFontOutline" :VOID (FONT :POINTER) (OUTLINE :INT))
(DEFCFUN "TTF_GetFontHinting" :INT (FONT :POINTER))
(DEFCFUN "TTF_SetFontHinting" :VOID (FONT :POINTER) (HINTING :INT))
(DEFCFUN "TTF_FontHeight" :INT (FONT :POINTER))
(DEFCFUN "TTF_FontAscent" :INT (FONT :POINTER))
(DEFCFUN "TTF_FontDescent" :INT (FONT :POINTER))
(DEFCFUN "TTF_FontLineSkip" :INT (FONT :POINTER))
(DEFCFUN "TTF_GetFontKerning" :INT (FONT :POINTER))
(DEFCFUN "TTF_SetFontKerning" :VOID (FONT :POINTER) (ALLOWED :INT))
(DEFCFUN "TTF_FontFaces" :LONG (FONT :POINTER))
(DEFCFUN "TTF_FontFaceIsFixedWidth" :INT (FONT :POINTER))
(DEFCFUN "TTF_FontFaceFamilyName" :STRING (FONT :POINTER))
(DEFCFUN "TTF_FontFaceStyleName" :STRING (FONT :POINTER))
(DEFCFUN "TTF_GlyphIsProvided" :INT (FONT :POINTER) (CH UINT16))
(DEFCFUN "TTF_GlyphMetrics" :INT (FONT :POINTER) (CH UINT16)
         (MINX (:POINTER :INT)) (MAXX (:POINTER :INT)) (MINY (:POINTER :INT))
         (MAXY (:POINTER :INT)) (ADVANCE (:POINTER :INT)))
(DEFCFUN "TTF_SizeText" :INT (FONT :POINTER) (TEXT :STRING)
         (W (:POINTER :INT)) (H (:POINTER :INT)))
(DEFCFUN "TTF_SizeUTF8" :INT (FONT :POINTER) (TEXT :STRING)
         (W (:POINTER :INT)) (H (:POINTER :INT)))
(DEFCFUN "TTF_SizeUNICODE" :INT (FONT :POINTER) (TEXT (:POINTER UINT16))
         (W (:POINTER :INT)) (H (:POINTER :INT)))
(DEFCFUN "TTF_RenderText_Solid" :POINTER (FONT :POINTER)
         (TEXT :STRING) (FG sdl-color))
(DEFCFUN "TTF_RenderUTF8_Solid" :POINTER (FONT :POINTER)
         (TEXT :STRING) (FG sdl-color))
(DEFCFUN "TTF_RenderUNICODE_Solid" :POINTER (FONT :POINTER)
         (TEXT (:POINTER UINT16)) (FG sdl-color))
(DEFCFUN "TTF_RenderGlyph_Solid" :POINTER (FONT :POINTER) (CH UINT16)
         (FG sdl-color))
(DEFCFUN "TTF_RenderText_Shaded" :POINTER (FONT :POINTER)
         (TEXT :STRING) (FG sdl-color) (BG sdl-color))
(DEFCFUN "TTF_RenderUTF8_Shaded" :POINTER (FONT :POINTER)
         (TEXT :STRING) (FG sdl-color) (BG sdl-color))
(DEFCFUN "TTF_RenderUNICODE_Shaded" :POINTER (FONT :POINTER)
         (TEXT (:POINTER UINT16)) (FG sdl-color) (BG sdl-color))
(DEFCFUN "TTF_RenderGlyph_Shaded" :POINTER (FONT :POINTER) (CH UINT16)
         (FG sdl-color) (BG sdl-color))
(DEFCFUN "TTF_RenderText_Blended" :POINTER (FONT :POINTER)
         (TEXT :STRING) (FG sdl-color))
(DEFCFUN "TTF_RenderUTF8_Blended" :POINTER (FONT :POINTER)
         (TEXT :STRING) (FG sdl-color))
(DEFCFUN "TTF_RenderUNICODE_Blended" :POINTER (FONT :POINTER)
         (TEXT (:POINTER UINT16)) (FG sdl-color))
(DEFCFUN "TTF_RenderText_Blended_Wrapped" :POINTER (FONT :POINTER)
         (TEXT :STRING) (FG sdl-color) (WRAPLENGTH UINT32))
(DEFCFUN "TTF_RenderUTF8_Blended_Wrapped" :POINTER (FONT :POINTER)
         (TEXT :STRING) (FG sdl-color) (WRAPLENGTH UINT32))
(DEFCFUN "TTF_RenderUNICODE_Blended_Wrapped" :POINTER (FONT :POINTER)
         (TEXT (:POINTER UINT16)) (FG sdl-color) (WRAPLENGTH UINT32))
(DEFCFUN "TTF_RenderGlyph_Blended" :POINTER (FONT :POINTER) (CH UINT16)
         (FG sdl-color))
(DEFCFUN "TTF_CloseFont" :VOID (FONT :POINTER)) (DEFCFUN "TTF_Quit" :VOID)
(DEFCFUN "TTF_WasInit" :INT)
(DEFCFUN "TTF_GetFontKerningSizeGlyphs" :INT (FONT :POINTER)
         (PREVIOUS_CH UINT16) (CH UINT16))
