(in-package :cl-sdl2-cffi)

(defparameter +NULL+
  (cffi:null-pointer))

;; utilities
(defun join-flags (type &rest flags)
  (apply #'logior
         (mapcar (lambda (f)
                   (foreign-enum-value type f))
                 flags)))

;; SDL.h
(defcfun "SDL_Init" :int
  (flags Uint32))

(defcfun "SDL_InitSubSystem" :int
  (flags Uint32))

(defcfun "SDL_QuitSubSystem" :void
  (flags Uint32))

(defcfun "SDL_WasInit" Uint32
  (flags Uint32))

(defcfun "SDL_Quit" :void)

;; SDL_stdinc.h
(defcfun "SDL_GetMemoryFunctions" :void
  (malloc-func :pointer)
  (calloc-func :pointer)
  (realloc-func :pointer)
  (free-func :pointer))

(defcfun "SDL_SetMemoryFunctions" :void
  (malloc-func :pointer)
  (calloc-func :pointer)
  (realloc-func :pointer)
  (free-func :pointer))

(defcfun "SDL_GetNumAllocations" :int)

(defcfun "SDL_getenv" :string
  (name :string))

(defcfun "SDL_setenv" :int
  (name :string)
  (value :string)
  (overwrite :int))

(defcfun "SDL_qsort" :void
  (base :pointer)
  (nmemb size-t)
  (size size-t)
  (compare-fn :pointer))

(defcfun "SDL_abs" :int
  (x :int))

(defcfun "SDL_isdigit" :int
  (x :int))

(defcfun "SDL_isspace" :int
  (x :int))
(defcfun "SDL_isupper" :int
  (x :int))
(defcfun "SDL_islower" :int
  (x :int))
(defcfun "SDL_toupper" :int
  (x :int))
(defcfun "SDL_tolower" :int
  (x :int))

(defcfun "SDL_crc32" :Uint32
  (crc Uint32)
  (data :pointer)
  (len size-t))

(defcfun "SDL_memset" :pointer
  (dst :pointer)
  (c :int)
  (len size-t))

;; SDL_pixels.h
(defcfun "SDL_GetPixelFormatName" :string
  (format uint32))

(defcfun "SDL_PixelFormatEnumToMasks" sdl-bool
  (format Uint32)
  (bpp :pointer)
  (Rmask :pointer)
  (Gmask :pointer)
  (Bmask :pointer)
  (Amask :pointer))

(defcfun "SDL_MasksToPixelFormatEnum" uint32
  (bpp :int)
  (Rmask Uint32)
  (Gmask Uint32)
  (Bmask Uint32)
  (Amask Uint32))

(defcfun "SDL_AllocFormat" :pointer
  (pixel-format uint32))

(defcfun "SDL_FreeFormat" :void
  (format :pointer))

(defcfun "SDL_AllocPalette" :pointer
  (ncolors :int))

(defcfun "SDL_SetPixelFormatPalette" :int
  (format :pointer)
  (pallet :pointer))

(defcfun "SDL_SetPaletteColors" :int
  (palette :pointer)
  (colors :pointer)
  (firstcolor :int)
  (ncolors :int))

(defcfun "SDL_MapRGB" uint32
  (format :pointer)
  (r uint8)
  (g uint8)
  (b uint8))

(defcfun "SDL_MapRGBA" uint32
  (format :pointer)
  (r uint8)
  (g uint8)
  (b uint8)
  (a uint8))

(defcfun "SDL_GetRGB" :void
  (pixel uint32)
  (format :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer))

(defcfun "SDL_GetRGBA" :void
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (a :pointer))

(defcfun "SDL_CalculateGammaRamp" :void
  (gamma :float)
  (ramp :pointer))

;; SDL_keyboard.h
(defcfun "SDL_GetKeyboardFocus" :pointer)

(defcfun "SDL_GetKeyboardState" :pointer
  (numkeys :pointer))

(defcfun "SDL_GetModState" sdl-keymod)

(defcfun "SDL_SetModState" :void
  (modstate sdl-keymod))

(defcfun "SDL_GetKeyFromScancode" sdl-key-code
  (scancode sdl-scancode))

(defcfun "SDL_GetScancodeFromKey" sdl-scancode
  (key sdl-key-code))

(defcfun "SDL_GetScancodeName" :string
  (scancode sdl-scancode))

(defcfun "SDL_GetScancodeFromName" sdl-scancode
  (name :string))

(defcfun "SDL_GetKeyName" :string
  (key sdl-key-code))

(defcfun "SDL_GetKeyFromName" sdl-key-code
  (name :string))

(defcfun "SDL_StartTextInput" :void)

(defcfun "SDL_IsTextInputActive" sdl-bool)

(defcfun "SDL_StopTextInput" :void)

(defcfun "SDL_SetTextInputRect" :void
  (rect (:pointer (:struct sdl-rect))))

;; SDL_joystick.h
(progn
  (defcfun "SDL_LockJoysticks" :void)
  (defcfun "SDL_UnlockJoysticks" :void)

  (defcfun "SDL_NumJoysticks" :int)

  (defcfun "SDL_JoystickNameForIndex" :string
    (device_index :int))

  (defcfun "SDL_JoystickGetDevicePlayerIndex" :int
    (device_index :int ))

  (defcfun "SDL_JoystickGetDeviceGUID" sdl-joystick-guid
    (device-index :int))

  (defcfun "SDL_JoystickGetDeviceVendor" Uint16
    (device_index :int))

  (DEFCFUN "SDL_JoystickGetDeviceProduct" UINT16 (DEVICE_INDEX :INT))
  (DEFCFUN "SDL_JoystickGetDeviceProductVersion" UINT16 (DEVICE_INDEX :INT))
  (DEFCFUN "SDL_JoystickGetDeviceType" SDL-JOYSTICK-TYPE (DEVICE_INDEX :INT))
  (DEFCFUN "SDL_JoystickGetDeviceInstanceID" SDL-JOYSTICK-ID (DEVICE_INDEX :INT))
  (DEFCFUN "SDL_JoystickOpen" :POINTER (DEVICE_INDEX :INT))
  (DEFCFUN "SDL_JoystickFromInstanceID" :POINTER (INSTANCE_ID SDL-JOYSTICK-ID))
  (DEFCFUN "SDL_JoystickFromPlayerIndex" :POINTER (PLAYER_INDEX :INT))
  (DEFCFUN "SDL_JoystickAttachVirtual" :INT (TYPE SDL-JOYSTICK-TYPE) (NAXES :INT)
           (NBUTTONS :INT) (NHATS :INT))
  (DEFCFUN "SDL_JoystickDetachVirtual" :INT (DEVICE_INDEX :INT))
  (DEFCFUN "SDL_JoystickIsVirtual" SDL-BOOL (DEVICE_INDEX :INT))
  (DEFCFUN "SDL_JoystickSetVirtualAxis" :INT (JOYSTICK :POINTER) (AXIS :INT)
           (VALUE SINT16))
  (DEFCFUN "SDL_JoystickSetVirtualButton" :INT (JOYSTICK :POINTER) (BUTTON :INT)
           (VALUE UINT8))
  (DEFCFUN "SDL_JoystickSetVirtualHat" :INT (JOYSTICK :POINTER) (HAT :INT)
           (VALUE UINT8))
  (DEFCFUN "SDL_JoystickName" :STRING (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickGetPlayerIndex" :INT (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickSetPlayerIndex" :VOID (JOYSTICK :POINTER)
           (PLAYER_INDEX :INT))
  (DEFCFUN "SDL_JoystickGetGUID" SDL-JOYSTICK-GUID (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickGetVendor" UINT16 (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickGetProduct" UINT16 (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickGetProductVersion" UINT16 (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickGetSerial" :STRING (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickGetType" SDL-JOYSTICK-TYPE (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickGetGUIDString" :VOID (GUID SDL-JOYSTICK-GUID)
           (PSZGUID :STRING) (CBGUID :INT))
  (DEFCFUN "SDL_JoystickGetGUIDFromString" SDL-JOYSTICK-GUID (PCHGUID :STRING))
  (DEFCFUN "SDL_JoystickGetAttached" SDL-BOOL (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickInstanceID" SDL-JOYSTICK-ID (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickNumAxes" :INT (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickNumBalls" :INT (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickNumHats" :INT (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickNumButtons" :INT (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickUpdate" :VOID)
  (DEFCFUN "SDL_JoystickEventState" :INT (STATE :INT))
  (DEFCFUN "SDL_JoystickGetAxis" SINT16 (JOYSTICK :POINTER) (AXIS :INT))
  (DEFCFUN "SDL_JoystickGetAxisInitialState" SDL-BOOL (JOYSTICK :POINTER)
           (AXIS :INT) (STATE :POINTER))
  (DEFCFUN "SDL_JoystickGetHat" UINT8 (JOYSTICK :POINTER) (HAT :INT))
  (DEFCFUN "SDL_JoystickGetBall" :INT (JOYSTICK :POINTER) (BALL :INT)
           (DX :POINTER) (DY :POINTER))
  (DEFCFUN "SDL_JoystickGetButton" UINT8 (JOYSTICK :POINTER) (BUTTON :INT))
  (DEFCFUN "SDL_JoystickRumble" :INT (JOYSTICK :POINTER)
           (LOW_FREQUENCY_RUMBLE UINT16) (HIGH_FREQUENCY_RUMBLE UINT16)
           (DURATION_MS UINT32))
  (DEFCFUN "SDL_JoystickRumbleTriggers" :INT (JOYSTICK :POINTER)
           (LEFT_RUMBLE UINT16) (RIGHT_RUMBLE UINT16) (DURATION_MS UINT32))
  (DEFCFUN "SDL_JoystickHasLED" SDL-BOOL (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickSetLED" :INT (JOYSTICK :POINTER) (RED UINT8)
           (GREEN UINT8) (BLUE UINT8))
  (DEFCFUN "SDL_JoystickClose" :VOID (JOYSTICK :POINTER))
  (DEFCFUN "SDL_JoystickCurrentPowerLevel" SDL-JOYSTICK-POWER-LEVEL
    (JOYSTICK :POINTER)))

;; SDL_events.h
(defcfun "SDL_PumpEvents" :void)

(defcfun "SDL_PeepEvents" :int
  (events :pointer)
  (numevents :int)
  (action sdl-event-action)
  (mintype uint32)
  (maxtype uint32))

(defcfun "SDL_HasEvent" sdl-bool
  (type Uint32))

(defcfun "SDL_HasEvents" sdl-bool
  (mintype Uint32)
  (maxtype Uint32))

(defcfun "SDL_FlushEvent" :void
  (type Uint32))

(defcfun "SDL_FlushEvents" :void
  (mintype Uint32)
  (maxtype Uint32))

(defcfun "SDL_PollEvent" :int
  (event :pointer))

(defcfun "SDL_WaitEvent" :int
  (event :pointer))

(defcfun "SDL_WaitEventTimeout" :int
  (event :pointer)
  (timeout :int))

(defcfun "SDL_PushEvent" :int
  (event :pointer))

(defcfun "SDL_SetEventFilter" :void
  (filter-fn :pointer)
  (user-data :pointer))

(defcfun "SDL_GetEventFilter" sdl-bool
  (filter-fn :pointer)
  (user-data :pointer))

(defcfun "SDL_AddEventWatch" :void
  (filter-fn :pointer)
  (user-data :pointer))

(defcfun "SDL_DelEventWatch" :void
  (filter-fn :pointer)
  (user-data :pointer))

(defcfun "SDL_FilterEvents" :void
  (filter-fn :pointer)
  (user-data :pointer))

(defcfun "SDL_EventState" Uint8
  (type Uint32)
  (state :int))

(defcfun "SDL_RegisterEvents" Uint32
  (numevents :int))

;; SDL_rect.h
(defcfun "SDL_HasIntersection" sdl-bool
  (rect-A :pointer)
  (rect-B :pointer))

(defcfun "SDL_IntersectRect" sdl-bool
  (rect-A :pointer)
  (rect-B :pointer)
  (result :pointer))

(defcfun "SDL_UnionRect" :void
  (rect-A :pointer)
  (rect-B :pointer)
  (result :pointer))

(defcfun "SDL_EnclosePoints" sdl-bool
  (points :pointer)
  (count :int)
  (rect-clip :pointer)
  (result :pointer))

(defcfun "SDL_IntersectRectAndLine" sdl-bool
  (rect :pointer)
  (X1 :pointer)
  (Y1 :pointer)
  (X2 :pointer)
  (Y2 :pointer))

;; SDL_blendmode.h
(defcfun "SDL_ComposeCustomBlendMode" sdl-blend-mode
  (src-color-factor sdl-blend-factor)
  (dst-color-factor sdl-blend-factor)
  (color-operation sdl-blend-operation)
  (src-alpha-factor sdl-blend-factor)
  (dst-alpha-factor sdl-blend-factor)
  (alpha-operation sdl-blend-operation))

;; SDL_surface.h
(defcfun "SDL_CreateRGBSurface" :pointer
  (flags Uint32)
  (width :int)
  (height :int)
  (depth :int)
  (Rmask Uint32)
  (Gmask Uint32)
  (Bmask Uint32)
  (Amask Uint32))

(defcfun "SDL_CreateRGBSurfaceWithFormat" :pointer
  (flags Uint32)
  (width :int)
  (height :int)
  (depth :int)
  (format Uint32))

(defcfun "SDL_CreateRGBSurfaceFrom" :pointer
  (pixels :pointer)
  (width :int)
  (height :int)
  (depth :int)
  (pitch :int)
  (Rmask Uint32)
  (Gmask Uint32)
  (Bmask Uint32)
  (Amask Uint32))

(defcfun "SDL_CreateRGBSurfaceWithFormatFrom" :pointer
  (pixels :pointer)
  (width :int)
  (height :int)
  (depth :int)
  (pitch :int)
  (format Uint32))

(defcfun "SDL_FreeSurface" :void
  (surface :pointer))

(defcfun "SDL_SetSurfacePalette" :int
  (surface :pointer)
  (palette :pointer))

(defcfun "SDL_LockSurface" :int
  (surface :pointer))

(defcfun "SDL_UnlockSurface" :void
  (surface :pointer))

(defcfun "SDL_LoadBMP_RW" :pointer
  (src :pointer)
  (freesrc :int))

(defcfun "SDL_SaveBMP_RW" :int
  (surface :pointer)
  (dst :pointer)
  (freedst :int))

(defcfun "SDL_SetSurfaceRLE" :int
  (surface :pointer)
  (flag :int))

(defcfun "SDL_HasSurfaceRLE" sdl-bool
  (surface :pointer))

(defcfun "SDL_SetColorKey" :int
  (surface :pointer)
  (flag :int)
  (key Uint32))

(defcfun "SDL_HasColorKey" sdl-bool
  (surface :pointer))

(defcfun "SDL_GetColorKey" :int
  (surface :pointer)
  (key :pointer))

(defcfun "SDL_SetSurfaceColorMod" :int
  (surface :pointer)
  (r Uint8)
  (g Uint8)
  (b Uint8))

(defcfun "SDL_GetSurfaceColorMod" :int
  (surface :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer))

(defcfun "SDL_SetSurfaceAlphaMod" :int
  (surface :pointer)
  (alpha Uint8))

(defcfun "SDL_GetSurfaceAlphaMod" :int
  (surface :pointer)
  (alpha :pointer))

(defcfun "SDL_SetSurfaceBlendMode" :int
  (surface :pointer)
  (blendMode sdl-blend-mode))

(defcfun "SDL_GetSurfaceBlendMode" :int
  (surface :pointer)
  (blendMode :pointer))

(defcfun "SDL_SetClipRect" sdl-bool
  (surface :pointer)
  (rect :pointer))

(defcfun "SDL_GetClipRect" :void
  (surface :pointer)
  (rect :pointer))

(defcfun "SDL_DuplicateSurface" :pointer
  (surface :pointer))

(defcfun "SDL_ConvertSurface" :pointer
  (src :pointer)
  (fmt :pointer)
  (flags Uint32))

(defcfun "SDL_ConvertSurfaceFormat" :pointer
  (src :pointer)
  (pixel-format Uint32)
  (flags Uint32))

(defcfun "SDL_ConvertPixels" :int
  (width :int)
  (height :int)
  (src_format Uint32)
  (src :pointer)
  (src_pitch :int)
  (dst_format Uint32)
  (dst :pointer)
  (dst_pitch :int))

(defcfun "SDL_FillRect" :int
  (dst :pointer)
  (rect :pointer)
  (color Uint32))

(defcfun "SDL_FillRects" :int
  (dst :pointer)
  (rects :pointer)
  (count :int)
  (color Uint32))

(defcfun "SDL_UpperBlit" :int
  (src-surf :pointer)
  (src-rect :pointer)
  (dst-surf :pointer)
  (dst-rect :pointer))

(defcfun "SDL_LowerBlit" :int
  (src-surf :pointer)
  (src-rect :pointer)
  (dst-surf :pointer)
  (dst-rect :pointer))

(defmacro sdl-blitsurface (src-surf src-rect dst-surf dst-rect)
  "alias for sdl-upperblit"
  `(sdl-upperblit ,src-surf
                  ,src-rect
                  ,dst-surf
                  ,dst-rect))

(defcfun "SDL_SoftStretch" :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))

(defcfun "SDL_UpperBlitScaled" :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))

(defcfun "SDL_LowerBlitScaled" :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))

(defmacro sdl-blitscaled (src srcrect dst dstrect)
  `(sdl-upperblitscaled ,src ,srcrect ,dst ,dstrect))

(defcfun "SDL_SetYUVConversionMode" :void
  (mode sdl-yuv-conversion-mode))

(defcfun "SDL_GetYUVConversionMode"
    sdl-yuv-conversion-mode)

(defcfun "SDL_GetYUVConversionModeForResolution" sdl-yuv-conversion-mode
  (width :int)
  (height :int))

;; SDL_gamecontroller.h

;; at the moment all I care about is finding one, then using joystick API
(defcfun "SDL_IsGameController" sdl-bool
  (joystick-index :int))

;; SDL_timer.h
(defcfun "SDL_GetTicks" Uint32)

(defcfun "SDL_GetPerformanceCounter" Uint64)

(defcfun "SDL_GetPerformanceFrequency" Uint64)

(defcfun "SDL_Delay" :void
  (ms Uint32))

(defcfun "SDL_AddTimer" sdl-timer-id
  (interval Uint32)
  (callback :pointer)
  (param :pointer))

(defcfun "SDL_RemoveTimer" sdl-bool
  (id sdl-timer-id))

;; SDL_video.h
(defcfun "SDL_GetNumVideoDrivers" :int)

(defcfun "SDL_GetVideoDriver" :string
  (index :int))

(defcfun "SDL_VideoInit" :int
  (driver-name :string))

(defcfun "SDL_VideoQuit" :void)

(defcfun "SDL_GetCurrentVideoDriver" :string)

(defcfun "SDL_GetNumVideoDisplays" :int)

(defcfun "SDL_GetDisplayName" :string
  (display-index :int))

(defcfun "SDL_GetDisplayBounds" :int
  (display-index :int)
  (rect (:pointer (:struct sdl-rect))))

(defcfun "SDL_GetDisplayUsableBounds" :int
  (display-index :int)
  (rect (:pointer (:struct sdl-rect))))

(defcfun "SDL_GetDisplayDPI" :int
  (display-index :int)
  (ddpi (:pointer :float))
  (hdpi (:pointer :float))
  (vdpi (:pointer :float)))

(defcfun "SDL_GetDisplayOrientation" sdl-display-orientation
  (display-index :int))

(defcfun "SDL_GetNumDisplayModes" :int
  (display-index :int))

(defcfun "SDL_GetDisplayMode" :int
  (display-index :int)
  (mode-index :int)
  (mode (:pointer (:struct sdl-display-mode))))

(defcfun "SDL_GetDesktopDisplayMOde" :int
  (display-index :int)
  (mode (:pointer (:struct sdl-display-mode))))

(defcfun "SDL_GetCurrentDisplayMode" :int
  (display-index :int)
  (mode (:pointer (:struct sdl-display-mode))))

(defcfun "SDL_GetClosestDisplayMode" (:pointer (:struct sdl-display-mode))
  (display-index :int)
  (mode (:pointer (:struct sdl-display-mode)))
  (closest (:pointer (:struct sdl-display-mode))))

(defcfun "SDL_GetWindowDisplayIndex" :int
  (window :pointer))

(defcfun "SDL_SetWindowDisplayMode" :int
  (window :pointer)
  (mode (:pointer (:struct sdl-display-mode))))

(defcfun "SDL_GetWindowDisplayMode" :int
  (window :pointer)
  (mode (:pointer (:struct sdl-display-mode))))

(defcfun "SDL_GetWindowPixelFormat" Uint32
  (window :pointer))

(defcfun "SDL_CreateWindow" :pointer
  (title :string)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flags Uint32))

(defcfun "SDL_CreateWindowFrom" :pointer
  (data :pointer))

(defcfun "SDL_GetWindowID" Uint32
  (window :pointer))

(defcfun "SDL_GetWindowFromID" :pointer
  (id Uint32))

(defcfun "SDL_GetWindowFlags" Uint32
  (window :pointer))

(defcfun "SDL_SetWindowTitle" :void
  (window :pointer)
  (title :string))

(defcfun "SDL_GetWindowTitle" :string
  (window :pointer))

(defcfun "SDL_SetWindowIcon" :void
  (window :pointer)
  (icon (:pointer (:struct sdl-surface))))

(defcfun "SDL_SetWindowData" :pointer
  (window :pointer)
  (name :string)
  (userdata :pointer))

(defcfun "SDL_GetWindowData" :pointer
  (window :pointer)
  (name :string))

(defcfun "SDL_SetWindowPosition" :void
  (window :pointer)
  (x :int)
  (y :int))

(defcfun "SDL_GetWindowPosition" :void
  (window :pointer)
  (x (:pointer :int))
  (y (:pointer :int)))

(defcfun "SDL_SetWindowSize" :void
  (window :pointer)
  (w :int)
  (h :int))

(defcfun "SDL_GetWindowSize" :void
  (window :pointer)
  (w (:pointer :int))
  (h (:pointer :int)))

(defcfun "SDL_GetWindowBordersSize" :int
  (window :pointer)
  (top (:pointer :int))
  (left (:pointer :int))
  (bottom (:pointer :int))
  (right (:pointer :int)))

(defcfun "SDL_SetWindowMinimumSize" :void
  (window :pointer)
  (min-w :int)
  (min-h :int))

(defcfun "SDL_GetWindowMinimumSize" :void
  (window :pointer)
  (w (:pointer :int))
  (h (:pointer :int)))

(defcfun "SDL_SetWindowMaximumSize" :void
  (window :pointer)
  (max-w :int)
  (max-h :int))

(defcfun "SDL_GetWindowMaximumSize" :void
  (window :pointer)
  (w (:pointer :int))
  (h (:pointer :int)))

(defcfun "SDL_SetWindowBordered" :void
  (window :pointer)
  (bordered sdl-bool))

(defcfun "SDL_SetWindowResizable" :void
  (window :pointer)
  (resizable sdl-bool))

(defcfun "SDL_ShowWindow" :void
  (window :pointer))

(defcfun "SDL_HideWindow" :void
  (window :pointer))

(defcfun "SDL_RaiseWindow" :void
  (window :pointer))

(defcfun "SDL_MaximizeWindow" :void
    (window :pointer))

(defcfun "SDL_MinimizeWindow" :void
  (window :pointer))

(defcfun "SDL_RestoreWindow" :void
  (window :pointer))

(defcfun "SDL_SetWindowFullscreen" :int
  (window :pointer)
  (flags Uint32))

(defcfun "SDL_GetWindowSurface" (:pointer (:struct sdl-surface))
  (window :pointer))

(defcfun "SDL_UpdateWindowSurface" :int
  (window :pointer))

(defcfun "SDL_UpdateWindowSurfaceRects" :int
  (window :pointer)
  (rects (:pointer (:struct sdl-rect)))
  (numrects :int))

(defcfun "SDL_SetWindowGrab" :void
  (window :pointer)
  (grabbed sdl-bool))

(defcfun "SDL_GetWindowGrab" sdl-bool
  (window :pointer))

(defcfun "SDL_GetGrabbedWindow" :pointer)

(defcfun "SDL_SetWindowBrightness" :int
  (window :pointer)
  (brightness :float))

(defcfun "SDL_GetWindowBrightness" :float
  (window :pointer))

(defcfun "SDL_SetWindowOpacity" :int
  (window :pointer)
  (opacity :float))

(defcfun "SDL_GetWindowOpacity" :int
  (window :pointer)
  (out_opacity (:pointer :float)))

(defcfun "SDL_SetWindowModalFor" :int
  (modal_window :pointer)
  (parent_window :pointer))

(defcfun "SDL_SetWindowInputFocus" :int
  (window :pointer))

(defcfun "SDL_SetWindowGammaRamp" :int
  (window :pointer)
  (red (:pointer Uint16))
  (green (:pointer Uint16))
  (blue (:pointer Uint16)))

(defcfun "SDL_GetWindowGammaRamp" :int
  (window :pointer)
  (red (:pointer Uint16))
  (green (:pointer Uint16))
  (blue (:pointer Uint16)))

(defcfun "SDL_SetWindowHitTest" :int
  (window :pointer)
  (callback :pointer)
  (callback_data :pointer))

(defcfun "SDL_DestroyWindow" :void
  (window :pointer))

(defcfun "SDL_IsScreenSaverEnabled" sdl-bool)

(defcfun "SDL_EnableScreenSaver" :void)

(defcfun "SDL_DisableScreenSaver" :void)

(defcfun "SDL_GL_LoadLibrary" :int
  (path :string))

(defcfun "SDL_GL_GetProcAddress" :pointer
  (proc :string))

(defcfun "SDL_GL_UnloadLibrary" :void)

(defcfun "SDL_GL_ExtensionSupported" sdl-bool
  (extension :string))

(defcfun "SDL_GL_ResetAttributes" :void)

(defcfun "SDL_GL_SetAttribute" :int
  (attr sdl-gl-attr)
  (value :int))

(defcfun "SDL_GL_GetAttribute" :int
  (attr sdl-gl-attr)
  (value (:pointer :int)))

(defcfun "SDL_GL_CreateContext" :pointer
  (window :pointer))

(defcfun "SDL_GL_MakeCurrent" :int
  (window :pointer)
  (context :pointer))

(defcfun "SDL_GL_GetCurrentWindow" :pointer)

(defcfun "SDL_GL_GetCurrentContext" :pointer)

(defcfun "SDL_GL_GetDrawableSize" :void
  (window :pointer)
  (w (:pointer :int))
  (h (:pointer :int)))

(defcfun "SDL_GL_SetSwapInterval" :int
  (interval :int))

(defcfun "SDL_GL_GetSwapInterval" :int)

(defcfun "SDL_GL_SwapWindow" :void
  (window :pointer))

(defcfun "SDL_GL_DeleteContext" :void
  (context :pointer))
