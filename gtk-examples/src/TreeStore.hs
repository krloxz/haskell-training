---
-- A tree view example using a list store as model.
--
-- Haskell implementation of a Gnome example:
--   https://developer-old.gnome.org/gtkmm-tutorial/stable/sec-treeview-examples.html.en
--
-- By Carlos Gomez
---

{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

import Data.Int
import Data.Maybe
import Data.Text
import Text.Printf

import Data.GI.Base
import Data.GI.Base.GType
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  app <- new Gtk.Application
    [ #applicationId := "org.gtkmm.example"
    , #flags := [ Gio.ApplicationFlagsFlagsNone ]
    ]
  on app #activate (activateApp app)
  #run app Nothing
  return ()

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  treeStore <- newTreeStore
  tree <- new Gtk.TreeView [ #model := treeStore, #visible := True]
  newColumn "ID" 0 >>= #appendColumn tree
  newColumn "Name" 1 >>= #appendColumn tree
  on tree #rowActivated $ printRow treeStore

  scrollable <- new Gtk.ScrolledWindow [ #child := tree, #expand := True ]

  quitButton <- new Gtk.Button [ #label := "Quit", #hexpand := True, #halign := Gtk.AlignEnd ]
  buttonBox <- new Gtk.Box [ #margin := 5 ]
  #add buttonBox quitButton

  verticalBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #margin := 5 ]
  #add verticalBox scrollable
  #add verticalBox buttonBox

  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Gtk::TreeView (TreeStore) example"
    , #defaultWidth := 500
    , #defaultHeight := 200
    , #child := verticalBox
    ]
  on quitButton #clicked $ #close window
  #showAll window

  return ()

newTreeStore :: IO Gtk.TreeStore
newTreeStore = do
  store <- Gtk.treeStoreNew [gtypeInt, gtypeString]

  values <- sequence
    [ Gtk.toGValue(1 :: Int32)
    , Gtk.toGValue(Just "Billy Bob" :: Maybe String)
    ]
  row <- #append store Nothing
  #set store row [0, 1] values

  values <- sequence
    [ Gtk.toGValue(11 :: Int32)
    , Gtk.toGValue(Just "Billy Bob Junior" :: Maybe String)
    ]
  childRow <- #append store (Just row)
  #set store childRow [0, 1] values

  values <- sequence
    [ Gtk.toGValue(12 :: Int32)
    , Gtk.toGValue(Just "Sue Bob" :: Maybe String)
    ]
  childRow <- #append store (Just row)
  #set store childRow [0, 1] values

  values <- sequence
    [ Gtk.toGValue(2 :: Int32)
    , Gtk.toGValue(Just "Joey Jojo" :: Maybe String)
    ]
  row <- #append store Nothing
  #set store row [0, 1] values

  values <- sequence
    [ Gtk.toGValue(3 :: Int32)
    , Gtk.toGValue(Just "Rob McRoberts" :: Maybe String)
    ]
  row <- #append store Nothing
  #set store row [0, 1] values

  values <- sequence
    [ Gtk.toGValue(31 :: Int32)
    , Gtk.toGValue(Just "Xavier McRoberts" :: Maybe String)
    ]
  childRow <- #append store (Just row)
  #set store childRow [0, 1] values

  return store

newColumn :: Text -> Int32 -> IO Gtk.TreeViewColumn
newColumn title modelIndex = do
  column <- new Gtk.TreeViewColumn [ #title := title ]
  renderer <- Gtk.cellRendererTextNew
  #packStart column renderer True
  #addAttribute column renderer (pack "text") modelIndex
  return column

printRow :: Gtk.TreeStore -> Gtk.TreePath -> Gtk.TreeViewColumn -> IO ()
printRow model path column = do
  (_, row) <- Gtk.treeModelGetIter model path
  id <- #getValue model row 0 >>= Gtk.fromGValue :: IO Int32
  name <- #getValue model row 1 >>= Gtk.fromGValue :: IO (Maybe String)
  printf "Row activated: ID=%d, Name=%s\n" id $ fromMaybe "" name

-- >>> main
-- Row activated: ID=1, Name=Billy Bob
-- Row activated: ID=2, Name=Joey Jojo
-- Row activated: ID=3, Name=Rob McRoberts
--
