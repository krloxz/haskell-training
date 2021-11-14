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
import Data.Text
import Data.Text.Format.Numbers

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

  listStore <- newListStore

  tree <- new Gtk.TreeView [ #model := listStore, #visible := True]
  newTextColumn "ID" 0 >>= #appendColumn tree
  newTextColumn "Name" 1 >>= #appendColumn tree
  newFormattedColumn "Formatted number" 2 >>= #appendColumn tree
  newPercentageColumn "Some percentage" 3 >>= #appendColumn tree

  scrollable <- new Gtk.ScrolledWindow [ #child := tree, #expand := True ]

  quitButton <- new Gtk.Button [ #label := "Quit", #hexpand := True, #halign := Gtk.AlignEnd ]
  buttonBox <- new Gtk.Box [ #margin := 5 ]
  #add buttonBox quitButton

  verticalBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #margin := 5 ]
  #add verticalBox scrollable
  #add verticalBox buttonBox

  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Gtk::TreeView (ListStore) example"
    , #defaultWidth := 450
    , #defaultHeight := 200
    , #child := verticalBox
    ]
  on quitButton #clicked $ #close window
  #showAll window

  return ()

newListStore :: IO Gtk.ListStore
newListStore = do
  listStore <- Gtk.listStoreNew [gtypeInt, gtypeString, gtypeFloat, gtypeInt]

  values <- sequence
    [ Gtk.toGValue(1 :: Int32)
    , Gtk.toGValue(Just "Billy Bob" :: Maybe String)
    , Gtk.toGValue(1000:: Float)
    , Gtk.toGValue(15:: Int32) ]
  iter <- #append listStore
  #set listStore iter [0, 1, 2, 3] values

  values <- sequence
    [ Gtk.toGValue(2 :: Int32)
    , Gtk.toGValue(Just "Joey Jojo" :: Maybe String)
    , Gtk.toGValue(20000:: Float)
    , Gtk.toGValue(40:: Int32) ]
  iter <- #append listStore
  #set listStore iter [0, 1, 2, 3] values

  values <- sequence
    [ Gtk.toGValue(3 :: Int32)
    , Gtk.toGValue(Just "Rob McRoberts" :: Maybe String)
    , Gtk.toGValue(300000:: Float)
    , Gtk.toGValue(70:: Int32) ]
  iter <- #append listStore
  #set listStore iter [0, 1, 2, 3] values

  return listStore

newTextColumn :: Text -> Int32 -> IO Gtk.TreeViewColumn
newTextColumn title modelIndex = do
  column <- new Gtk.TreeViewColumn [ #title := title ]
  renderer <- Gtk.cellRendererTextNew
  #packStart column renderer True
  #addAttribute column renderer (pack "text") modelIndex
  return column

newFormattedColumn :: Text -> Int32 -> IO Gtk.TreeViewColumn
newFormattedColumn title modelIndex = do
  column <- new Gtk.TreeViewColumn [ #title := title ]
  renderer <- Gtk.cellRendererTextNew
  #packStart column renderer True
  #setCellDataFunc column renderer (Just formattedColumn)
  return column

newPercentageColumn :: Text -> Int32 -> IO Gtk.TreeViewColumn
newPercentageColumn title modelIndex = do
  column <- new Gtk.TreeViewColumn [ #title := title ]
  renderer <- Gtk.cellRendererProgressNew
  #packStart column renderer True
  #setCellDataFunc column renderer (Just percentageColumn)
  return column

formattedColumn :: Gtk.TreeViewColumn -> Gtk.CellRenderer -> Gtk.TreeModel -> Gtk.TreeIter -> IO ()
formattedColumn column renderer model iter = do
  gValue <- #getValue model iter 2
  value <- Gtk.fromGValue gValue :: IO Float
  formattedValue <- Gtk.toGValue (Just (prettyF (PrettyCfg 2 (Just ',') '.') value) :: Maybe Text)
  #setProperty renderer "text" formattedValue
  return ()

percentageColumn :: Gtk.TreeViewColumn -> Gtk.CellRenderer -> Gtk.TreeModel -> Gtk.TreeIter -> IO ()
percentageColumn column renderer model iter = do
  gValue <- #getValue model iter 3
  value <- Gtk.fromGValue gValue :: IO Int32
  percentage <- Gtk.toGValue (Just ((show value) ++ " %") :: Maybe String)
  #setProperty renderer "text" percentage
  #setProperty renderer "value" gValue
  return ()

-- >>> main
--
