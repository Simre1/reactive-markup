{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ReactiveMarkup.Runners.Gtk
  ( GtkElements,
    widgetRunner,
    windowRunner,
    runGtkWidget,
    runGtkWidgetUnblocking,
    GtkM,
    runGtkM,
    GtkState(..),
    defaultGtkState,
    -- BC.Cairo
  )
where

import Control.Monad (forM_, when, join, replicateM_)
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Reader as R
import Data.Colour
import Data.Colour.SRGB
import Data.Functor ((<&>))
import qualified Data.GI.Base.Attributes as Gtk (AttrOpTag (..))
import Data.IORef (atomicWriteIORef, IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified Data.GI.Base.Overloading as GI
import qualified Data.GI.Base.Attributes as GI
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (nullPtr, castPtr)
import ReactiveMarkup
import Data.Word (Word32)
import qualified Data.GI.Base.Signals as GLib
import Data.Proxy
import Control.Concurrent (forkOS)
import Control.Concurrent.MVar (putMVar, takeMVar, newEmptyMVar)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

import System.Mem.StableName
import Data.Coerce (coerce)

type GtkElements =
  [ Label ('[Text, DynamicText] |-> BasicStyling),
    List ('[Orientation] |-> Expandable),
    Button ('[Text, DynamicText, Activate, Click] |-> Expandable |-> BasicStyling),
    ToggleButton '[Toggle],
    DynamicState,
    DynamicStateIO,
    DynamicMarkup,
    HandleEvent,
    HandleEventIO,
    FlowLayout '[Orientation],
    GridLayout [HomogenousRows, HomogenousColumns],
    TextInput ('[Text, DynamicText, TextChange, Activate] |-> BasicStyling),
    HotKey,
    -- DrawingBoard '[DrawDiagram BC.Cairo, DrawDynamicDiagram BC.Cairo, MouseClickWithPosition, AspectRatio],
    Notebook,
    Menu
  ]

type Expandable = '[HorizontalExpand, VerticalExpand]

type BasicStyling = '[FontSize, FontWeight, FontStyle, FontColour, BackgroundColour]

windowRunner :: Runner '[Window '[Text]] (IO ()) (GtkM Gtk.Widget)
windowRunner = fullRun (\(Window (Options options) children) childrenRunner handleEvent -> do
    sequence $ runMarkup optionRunner handleEvent <$> options
    runMarkup childrenRunner handleEvent children
  )
  where
    optionRunner :: Runner '[Text] (IO ()) (GtkM ())
    optionRunner = simpleRun (\(Text t) -> ask gtkWindow >>= \w -> Gtk.set w [#title Gtk.:= t])

-- | Basic `Runner` for GTK 3. Could be improved drastically by utilizing a similar technique to virtual DOM.
-- widgetRunner :: Runner GtkElements IO (GtkM (Ret a))
widgetRunner :: Runner GtkElements (IO ()) (GtkM Gtk.Widget)
widgetRunner =
  emptyRunner
    -- basic elements
    |-> runLabel
    |-> runList
    |-> runButton
    |-> runToggleButton
    |-> runDynamicState
    |-> runDynamicStateIO
    |-> runDynamicMarkup
    |-> runHandleEvent
    |-> runHandleEventIO
    -- additional layouts
    |-> runFlowLayout
    |-> runGridLayout
    -- input elements
    |-> runTextInput
    |-> runHotKey
    |-> runNotebook
    |-> runMenu

runLabel :: Runner '[Label ('[Text, DynamicText] |-> BasicStyling)] (IO ()) (GtkM Gtk.Widget)
runLabel = eventRun $ \label@(Label options) handleEvent -> do
  label <- Gtk.new Gtk.Label []
  gtkWidget <- Gtk.toWidget label
  applyOptions label options handleEvent $ runTextLabel |-> runDynamicTextLabel |-> runBasicStyling
  pure gtkWidget

runList :: Runner '[List ('[Orientation] |-> Expandable)] (IO ()) (GtkM Gtk.Widget)
runList = fullRun $ \(List options children) runner handleEvent -> do
  boxLayout <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]
  applyOptions boxLayout options handleEvent $ runOrientation |-> runExpandable
  traverse (\markup -> runMarkup runner handleEvent markup >>= #add boxLayout) children
  Gtk.toWidget boxLayout

-- runBox :: Runner (Box [MinWidth, MinHeight]) IO (GtkM Gtk.Widget)
-- runBox (Box (Options options) child) runner handleEvent = do
--   gtkBox <- Gtk.new Gtk.Box [#expand Gtk.:= True]
--   childWidget <- runMarkup runner handleEvent child
--   #add gtkBox childWidget
--   widget <- Gtk.toWidget gtkBox
--   addCssToWidget widget $ T.concat $ runMarkup optionsRunner handleEvent <$> options
--   pure widget
--   where 
--     optionsRunner :: Runner [MinWidth, MinHeight] (IO ()) (T.Text)
--     optionsRunner = emptyRunner
--       |-> (\(MinWidth u) _ _ -> "min-width:" <> toCssUnit u <> ";")
--       |-> (\(MinHeight u) _ _ -> "min-height:" <> toCssUnit u <> ";")
--       where 
--         toCssUnit (Pixel p) = T.pack (show p) <> "px"
--         toCssUnit (Percent p) = T.pack (show p) <> "%"

runButton :: Runner '[Button ('[Text, DynamicText, Activate, Click] |-> Expandable |-> BasicStyling)] (IO ()) (GtkM Gtk.Widget)
runButton = eventRun $ \(Button options) handleEvent -> do
  button <- Gtk.new Gtk.Button []
  applyOptions button options handleEvent $ runTextLabel |-> runDynamicTextLabel |-> runActivate |-> runClick |-> runExpandable |-> runBasicStyling 
  Gtk.toWidget button

runToggleButton :: Runner '[ToggleButton '[Toggle]] (IO ()) (GtkM Gtk.Widget)
runToggleButton = eventRun $ \(ToggleButton options) handleEvent -> do
  button <- Gtk.new Gtk.ToggleButton []
  applyOptions button options handleEvent $ runToggle
  Gtk.toWidget button

runDynamicState :: Runner '[DynamicState] (IO ()) (GtkM Gtk.Widget)
runDynamicState = fullRun $ \(DynamicState state mapEvent generateMarkup) childRunner handleOuterEvent -> do
  (dynamicState, updateState) <- liftIO $ newDynamic state
  let handleInnerEvent innerEvent = do
        state <- current $ toBehavior dynamicState
        let (changedState, outerEvent) = mapEvent state innerEvent
        maybe (pure ()) (triggerEvent updateState) changedState
        maybe (pure ()) handleOuterEvent outerEvent
  let child = generateMarkup dynamicState
  runMarkup childRunner handleInnerEvent child

runDynamicStateIO :: Runner '[DynamicStateIO] (IO ()) (GtkM Gtk.Widget)
runDynamicStateIO = fullRun $ \(DynamicStateIO state mapEvent generateMarkup) childRunner handleOuterEvent -> do
  (dynamicState, updateState) <- liftIO $ newDynamic state
  let handleInnerEvent innerEvent = do
        state <- current $ toBehavior dynamicState
        (changedState, outerEvent) <- liftIO $ mapEvent state innerEvent
        maybe (pure ()) (triggerEvent updateState) changedState
        maybe (pure ()) handleOuterEvent outerEvent
  let children = generateMarkup dynamicState
  runMarkup childRunner handleInnerEvent $ children

runDynamicMarkup :: Runner '[DynamicMarkup] (IO ()) (GtkM Gtk.Widget)
runDynamicMarkup = fullRun $ \(DynamicMarkup dynamicState generateMarkup) childRunner handleEvent -> do
  frame <- Gtk.new Gtk.Frame []
  Gtk.frameSetShadowType frame Gtk.ShadowTypeNone
  state <- liftIO $ current $ toBehavior dynamicState
  cleanUpRef <- liftIO $ newIORef (pure ())
  let setWidget widget = liftIO $ do
        cleanUp <- join $ readIORef cleanUpRef
        writeIORef cleanUpRef (#remove frame widget)
        #add frame widget
        #showAll widget
      generateWidget state = do
        runMarkup childRunner handleEvent $ generateMarkup state
  handler <- withinIO $ \newState -> generateWidget newState >>= setWidget
  unregisterWidgetUpdate <-
    liftIO $
      reactimate (toEvent dynamicState) $ simpleEventHandler handler
  generateWidget state >>= setWidget
  widget <- Gtk.toWidget frame
  Gtk.on widget #destroy (liftES unregisterWidgetUpdate)
  pure widget

runHandleEvent :: Runner '[HandleEvent] (IO ()) (GtkM Gtk.Widget)
runHandleEvent = fullRun $ \(HandleEvent innerHandle markup) runner handleEvent -> do
  runMarkup runner (maybe (pure ()) handleEvent . innerHandle) markup

runHandleEventIO :: Runner '[HandleEventIO] (IO ()) (GtkM Gtk.Widget)
runHandleEventIO = fullRun $ \(HandleEventIO innerHandle markup) runner handleEvent -> do
  runMarkup runner (\x -> innerHandle x >>= maybe (pure ()) handleEvent) markup

runFlowLayout :: Runner '[FlowLayout '[Orientation]] (IO ()) (GtkM Gtk.Widget)
runFlowLayout = fullRun $ \(FlowLayout options children) runner handleEvent -> do
  flowLayout <- Gtk.new Gtk.FlowBox []
  applyOptions flowLayout options handleEvent $ runOrientation
  traverse (\markup -> runMarkup runner handleEvent markup >>= #add flowLayout) children
  Gtk.toWidget flowLayout

runGridLayout :: Runner '[GridLayout [HomogenousRows, HomogenousColumns]] (IO ()) (GtkM Gtk.Widget)
runGridLayout = fullRun $ \(GridLayout options children) runner handleEvent -> do
  gridLayout <- Gtk.new Gtk.Grid []
  applyOptions gridLayout options handleEvent $ runHomogenousRows |-> runHomogenousColumns
  traverse (runMarkupWithTwoExact (gridChildRunner gridLayout) runner handleEvent) children
  Gtk.toWidget gridLayout
  where
    gridChildRunner :: Gtk.Grid -> Runner '[GridChild] (IO ()) (GtkM Gtk.Widget)
    gridChildRunner gridLayout = fullRun $ \(GridChild childOptions markup) runner handleEvent -> do
      child <- runMarkup runner handleEvent markup
      Gtk.gridAttach
        gridLayout
        child
        (fromIntegral $ gridChildColumn childOptions)
        (fromIntegral $ gridChildRow childOptions)
        (fromIntegral $ gridChildWidth childOptions)
        (fromIntegral $ gridChildHeight childOptions)
      pure child

runTextInput :: Runner '[TextInput ('[Text, DynamicText, TextChange, Activate] |-> BasicStyling)] (IO ()) (GtkM Gtk.Widget)
runTextInput = eventRun $ \(TextInput options) handleEvent -> do
  gtkEntry <- Gtk.new Gtk.Entry []
  applyOptions gtkEntry options handleEvent $ runTextText |-> runDynamicTextText |-> runTextChange |-> runActivate |-> runBasicStyling 
  Gtk.toWidget gtkEntry

runHotKey :: Runner '[HotKey] (IO ()) (GtkM Gtk.Widget)
runHotKey = fullRun $ \(HotKey f child) runner handleEvent -> do
  gtkWidget <- runMarkup runner handleEvent child
  eventControllerKey <- ask gtkEventControllerKey
  handler <- Gtk.onEventControllerKeyKeyPressed eventControllerKey $ mapCallback handleEvent f
  Gtk.on gtkWidget #destroy $ GLib.disconnectSignalHandler eventControllerKey handler
  pure gtkWidget
  where 
    mapCallback :: (e -> IO ()) -> (Key -> [Modifier] -> Maybe e) -> Word32 -> Word32 -> [Gdk.ModifierType] -> IO Bool
    mapCallback handleEvent f gtkKeyval gtkKeycode gtkModifiers = 
      let modifiers = foldr (mapModifier) [] gtkModifiers
      in case mapKey gtkKeyval >>= \k -> f k modifiers of
        Just e -> handleEvent e *> pure True
        Nothing -> pure False
    mapModifier :: Gdk.ModifierType -> [Modifier] -> [Modifier]
    mapModifier Gdk.ModifierTypeControlMask = (ModControl :)
    mapModifier Gdk.ModifierTypeMod1Mask = (ModAlt :)
    mapModifier Gdk.ModifierTypeShiftMask = (ModShift :)
    mapModifier Gdk.ModifierTypeSuperMask = (ModSuper :)
    mapModifier _ = id
    mapKey :: Word32 -> Maybe Key
    mapKey gtkKey = case gtkKey of
      Gdk.KEY_q -> Just KeyQ
      Gdk.KEY_Q -> Just KeyQ
      Gdk.KEY_w -> Just KeyW
      Gdk.KEY_W -> Just KeyW
      Gdk.KEY_e -> Just KeyE
      Gdk.KEY_E -> Just KeyE
      Gdk.KEY_r -> Just KeyR
      Gdk.KEY_R -> Just KeyR
      Gdk.KEY_t -> Just KeyT
      Gdk.KEY_T -> Just KeyT
      Gdk.KEY_z -> Just KeyZ
      Gdk.KEY_Z -> Just KeyZ
      Gdk.KEY_u -> Just KeyU
      Gdk.KEY_Z -> Just KeyU
      Gdk.KEY_i -> Just KeyI
      Gdk.KEY_I -> Just KeyI
      Gdk.KEY_o -> Just KeyO
      Gdk.KEY_O -> Just KeyO
      Gdk.KEY_p -> Just KeyP
      Gdk.KEY_P -> Just KeyP
      Gdk.KEY_a -> Just KeyA
      Gdk.KEY_A -> Just KeyA
      Gdk.KEY_s -> Just KeyS
      Gdk.KEY_S -> Just KeyS
      Gdk.KEY_d -> Just KeyD
      Gdk.KEY_D -> Just KeyD
      Gdk.KEY_f -> Just KeyF
      Gdk.KEY_F -> Just KeyF
      Gdk.KEY_j -> Just KeyJ
      Gdk.KEY_J -> Just KeyJ
      Gdk.KEY_k -> Just KeyK
      Gdk.KEY_K -> Just KeyK
      Gdk.KEY_l -> Just KeyL
      Gdk.KEY_L -> Just KeyL
      Gdk.KEY_y -> Just KeyY
      Gdk.KEY_Y -> Just KeyY
      Gdk.KEY_x -> Just KeyX
      Gdk.KEY_X -> Just KeyX
      Gdk.KEY_c -> Just KeyC
      Gdk.KEY_C -> Just KeyC
      Gdk.KEY_v -> Just KeyV
      Gdk.KEY_V -> Just KeyV
      Gdk.KEY_b -> Just KeyB
      Gdk.KEY_B -> Just KeyB
      Gdk.KEY_n -> Just KeyN
      Gdk.KEY_N -> Just KeyN
      Gdk.KEY_m -> Just KeyM
      Gdk.KEY_M -> Just KeyM
      Gdk.KEY_ISO_Enter -> Just KeyEnter
      Gdk.KEY_space -> Just KeySpace
      _ -> Nothing

runNotebook :: Runner '[Notebook] (IO ()) (GtkM Gtk.Widget)
runNotebook = fullRun $ \(Notebook pages) runner handleEvent -> do
  notebook <- Gtk.new Gtk.Notebook []
  forM_ pages $ \(label, content) -> do
    gtkLabel <- runMarkup runner handleEvent label
    gtkContent <- runMarkup runner handleEvent content
    Gtk.notebookAppendPage notebook gtkContent (Just gtkLabel)
  Gtk.toWidget notebook

runMenu :: Runner '[Menu] (IO ()) (GtkM Gtk.Widget)
runMenu = fullRun $ \(Menu items) runner handleEvent -> do
  menu <- Gtk.new Gtk.Menu []
  forM_ items $ \item -> do
    menuItem <- Gtk.new Gtk.MenuItem []
    gtkWidget <- runMarkup runner handleEvent item
    #add menuItem gtkWidget
    #add menu menuItem
  Gtk.toWidget menu

type GtkOption o = GtkM (T.Text, [Gtk.AttrOp o Gtk.AttrSet], o -> GtkM ())

type AllowedAttr o s r = GI.AttrSetC (GI.ResolveAttribute s o) o s r

type AllowedSignal o s info = (GLib.SignalInfo info, Gtk.GObject o, info ~ GI.ResolveSignal s o, GLib.HaskellCallbackType info ~ IO ())

applyOptions :: Gtk.IsWidget o => o -> Options options e -> (e -> IO ()) -> (Runner options (IO ()) (GtkOption o)) -> GtkM ()
applyOptions widget (Options options) handleEvent optionRunner = do
  gtkOptions <- sequenceA $ runMarkupExact optionRunner handleEvent <$> options
  let css = T.concat $ (\(a,_,_) -> a) <$> gtkOptions
      attributes = concat $ (\(_,a,_) -> a) <$> gtkOptions
      action = sequenceA $ (\(_,_,a) -> a widget) <$> gtkOptions
  Gtk.set widget attributes
  addCssToWidget widget css
  action
  pure ()


runBasicStyling :: Runner '[FontSize, FontWeight, FontStyle, FontColour, BackgroundColour] (IO ()) (GtkOption o)
runBasicStyling = runFontSize |-> runFontWeight |-> runFontStyle |-> runFontColour |-> runBackgroundColour

runTextLabel :: AllowedAttr o "label" T.Text => Runner ('[Text]) (IO ()) (GtkOption o)
runTextLabel = simpleRun $ \(Text t) -> pure ("", [#label Gtk.:= t], const (pure ()))

runTextText :: AllowedAttr o "text" T.Text => Runner ('[Text]) (IO ()) (GtkOption o)
runTextText = simpleRun $ \(Text t) -> pure ("", [#text Gtk.:= t], const (pure ()))

runDynamicTextLabel :: (AllowedSignal o "destroy" info, AllowedAttr o "label" T.Text) => Runner ('[DynamicText]) (IO ()) (GtkOption o)
runDynamicTextLabel = simpleRun $ \(DynamicText dynT) -> do
  t <- liftIO $ current (toBehavior dynT)
  pure ("", [#label Gtk.:= t], \o -> liftIO $ do
    unReg <- reactimate (toEvent dynT) $ simpleEventHandler $ \t -> Gtk.set o [#label Gtk.:= t]
    Gtk.on o #destroy $ liftES unReg
    pure ()
    )

runDynamicTextText :: (AllowedSignal o "destroy" info, AllowedAttr o "text" T.Text) => Runner ('[DynamicText]) (IO ()) (GtkOption o)
runDynamicTextText = simpleRun $ \(DynamicText dynT) -> do
  t <- liftIO $ current (toBehavior dynT)
  pure ("", [#text Gtk.:= t], \o -> liftIO $ do
    unReg <- reactimate (toEvent dynT) $ simpleEventHandler $ \t -> Gtk.set o [#text Gtk.:= t]
    Gtk.on o #destroy $ liftES unReg
    pure ()
    )

runOrientation :: AllowedAttr o "orientation" Gtk.Orientation => Runner ('[Orientation]) (IO ()) (GtkOption o)
runOrientation = simpleRun $ \orientation -> case orientation of
  Horizontal -> pure ("", [#orientation Gtk.:= Gtk.OrientationHorizontal], const (pure ()))
  Vertical -> pure ("", [#orientation Gtk.:= Gtk.OrientationVertical], const (pure ()))
  

runFontSize :: Runner ('[FontSize]) (IO ()) (GtkOption o)
runFontSize = simpleRun $ \(FontSize unit) ->
  let sizeText = case unit of
        Pixel i -> T.pack (show i) <> "px"
        Percent i -> T.pack (show i) <> "%"
  in pure ("font-size: " <> sizeText <> ";", [], const (pure ()))

runFontWeight :: Runner ('[FontWeight]) (IO ()) (GtkOption o)
runFontWeight = simpleRun $ \(FontWeight weight) -> pure ("font-weight: " <> T.pack (show weight) <> ";", [], const (pure ()))

runFontStyle :: Runner ('[FontStyle]) (IO ()) (GtkOption o)
runFontStyle = simpleRun $ \fontStyle ->
  let styleText = case fontStyle of
        RegularStyle -> ""
        ItalicStyle -> "font-style: italic;"
  in pure (styleText, [], const (pure ()))

runFontColour :: Runner ('[FontColour]) (IO ()) (GtkOption o)
runFontColour = simpleRun $ \(FontColour colour) -> pure ("color:" <> T.pack (sRGB24show colour) <> ";", [], const (pure ()))

runBackgroundColour :: Runner ('[BackgroundColour]) (IO ()) (GtkOption o)
runBackgroundColour = simpleRun $ \(BackgroundColour colour) -> pure ("background-color:" <> T.pack (sRGB24show colour) <> ";", [], const (pure ()))

runHorizontalExpand :: AllowedAttr o "hexpand" Bool => Runner ('[HorizontalExpand]) (IO ()) (GtkOption o)
runHorizontalExpand = simpleRun $ (\(HorizontalExpand b) -> pure ("", [#hexpand Gtk.:= b], const (pure ())))

runVerticalExpand :: AllowedAttr o "vexpand" Bool => Runner ('[VerticalExpand]) (IO ()) (GtkOption o)
runVerticalExpand = simpleRun $ (\(VerticalExpand b) -> pure ("", [#vexpand Gtk.:= b], const (pure ())))

runExpandable :: (AllowedAttr o "vexpand" Bool, AllowedAttr o "hexpand" Bool) => Runner ('[HorizontalExpand, VerticalExpand]) (IO ()) (GtkOption o)
runExpandable = runHorizontalExpand |-> runVerticalExpand

runActivate :: AllowedSignal o "activate" info => Runner ('[Activate]) (IO ()) (GtkOption o)
runActivate = eventRun (\(Activate e) handleEvent -> pure ("", [], \o -> Gtk.on o #activate (handleEvent e) *> pure ()))

runClick :: AllowedSignal o "clicked" info => Runner ('[Click]) (IO ()) (GtkOption o)
runClick = eventRun $ \(Click e) handleEvent -> pure ("", [], \o -> Gtk.on o #clicked (handleEvent e) *> pure ())

runToggle :: (Gtk.IsToggleButton o, AllowedSignal o "toggled" info) => Runner ('[Toggle]) (IO ()) (GtkOption o)
runToggle = eventRun $ \(Toggle e) handleEvent -> pure ("", [], \o -> Gtk.on o #toggled (e <$> Gtk.getToggleButtonActive o >>= handleEvent) *> pure ())

runHomogenousRows :: AllowedAttr o "rowHomogeneous" Bool => Runner ('[HomogenousRows]) (IO ()) (GtkOption o)
runHomogenousRows = simpleRun $ \HomogenousRows -> pure ("", [#rowHomogeneous Gtk.:= True], const (pure ()))

runHomogenousColumns :: AllowedAttr o "columnHomogeneous" Bool => Runner ('[HomogenousColumns]) (IO ()) (GtkOption o)
runHomogenousColumns = simpleRun $ \HomogenousColumns -> pure ("", [#columnHomogeneous Gtk.:= True], const (pure ()))

runTextChange :: (Gtk.IsEntry o, AllowedSignal o "changed" info) => Runner ('[TextChange]) (IO ()) (GtkOption o)
runTextChange = eventRun $ \(TextChange f) handleEvent -> pure ("", [], \o -> Gtk.on o #changed (Gtk.entryGetText o >>= handleEvent . f) *> pure ())

defaultGtkState :: Gtk.Window -> IO GtkState
defaultGtkState window = do
  idRef <- newIORef 0
  eventControllerKey <- Gtk.eventControllerKeyNew window
  pure $ GtkState idRef window eventControllerKey
    where 
      getColor :: Gtk.StyleContext -> IO (Colour Double)
      getColor styleContext = do
        gtkColour <- Gtk.styleContextGetColor styleContext []
        r <- Gdk.getRGBARed gtkColour
        g <- Gdk.getRGBAGreen gtkColour
        b <- Gdk.getRGBABlue gtkColour
        a <- Gdk.getRGBAAlpha gtkColour
        pure (sRGB r g b)
        
data GtkState = GtkState
  { gtkId :: IORef Int
  , gtkWindow :: Gtk.Window
  , gtkEventControllerKey :: Gtk.EventControllerKey
  }

runGtkWidgetUnblocking :: IO (GtkM Gtk.Widget -> IO ())
runGtkWidgetUnblocking = do
  showWidget <- newEmptyMVar 
  forkOS $ do
    Gtk.init Nothing
    win <- Gtk.new Gtk.Window []
    Gtk.on win #destroy Gtk.mainQuit
    gtkState <- defaultGtkState win
    #showAll win
    putMVar showWidget $ \widget -> do
      widgets <- Gtk.containerGetChildren win
      forM_ widgets $ \w -> Gtk.containerRemove win w
      widget <- runGtkM gtkState widget
      Gtk.containerAdd win widget
      #showAll win
    Gtk.main
  takeMVar showWidget

runGtkWidget :: GtkM Gtk.Widget -> IO ()
runGtkWidget widget = do
  Gtk.init Nothing
  win <- Gtk.new Gtk.Window []
  Gtk.on win #destroy Gtk.mainQuit
  gtkState <- defaultGtkState win
  widget <- runGtkM gtkState widget
  Gtk.containerAdd win widget
  #showAll win
  Gtk.main

newtype GtkM a = GtkM (R.ReaderT (GtkState) IO a) deriving (Functor, Applicative, Monad, MonadIO)

ask :: (GtkState -> a) -> GtkM a
ask f = GtkM $ f <$> R.ask

local :: (GtkState -> GtkState) -> GtkM a -> GtkM a
local f (GtkM s) = GtkM $ R.local f s

nextId :: GtkM Int
nextId = do
  ref <- ask gtkId
  i <- liftIO $ atomicModifyIORef ref (\i -> (succ i, i))
  pure i

withinIO :: (s -> GtkM a) -> GtkM (s -> IO a)
withinIO f = do
  state <- ask id
  pure $ \s -> do
    a <- runGtkM state (f s)
    pure a

runGtkM :: GtkState -> GtkM a -> IO a
runGtkM state (GtkM s) = R.runReaderT s state

addCssToWidget :: Gtk.IsWidget o => o -> T.Text -> GtkM ()
addCssToWidget widget css = do
  i <- nextId
  let widgetName = "e" <> T.pack (show i)
  cssProvider <- Gtk.cssProviderNew
  styleContext <- Gtk.widgetGetStyleContext widget
  Gtk.widgetSetName widget widgetName
  Gtk.cssProviderLoadFromData cssProvider $ T.encodeUtf8 ("#" <> widgetName <> "{" <> css <> "}")
  Gtk.styleContextAddProvider styleContext cssProvider $ fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
