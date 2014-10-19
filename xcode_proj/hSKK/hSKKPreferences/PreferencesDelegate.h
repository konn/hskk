//
//  AppDelegate.h
//  hSKKPreferences
//
//  Created by 石井 大海 on 2014/10/19.
//  Copyright (c) 2014年 Hiromi ISHII. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface PreferencesDelegate : NSObject <NSApplicationDelegate>

@property (weak) IBOutlet NSTextField *userDicField;
@property (weak) IBOutlet NSTableColumn *otherDicKindCol;
@property (weak) IBOutlet NSTableColumn *otherDicsLocationCol;
@property (weak) IBOutlet NSTextField *inlineCountField;
@property (weak) IBOutlet NSStepper *inlineCountStepper;
@property (weak) IBOutlet NSTextField *candidateLabelField;
@property (weak) IBOutlet NSButton *addButton;
@property (weak) IBOutlet NSButton *removeButton;

@end

