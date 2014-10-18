{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module AppDelegate where
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

objc_import ["Cocoa/Cocoa.h"]

objc_interface [cunit|
@interface AppDelegate : NSObject<NSApplicationDelegate>
@property (weak) typename NSMenu *menu;
@end
|]

objc_implementation [] [cunit|
@implementation AppDelegate
@synthesize menu = _menu;
-(void) awakeFromNib
{
  typename NSMenuItem* preferences = [_menu itemWithTag: 1];
  if (preferences) {
    [preferences setAction:@selector(showPreferences:)];
  }
}
@end
|]

objc_emit
