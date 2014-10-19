//
//  AppDelegate.m
//  hSKKPreferences
//
//  Created by 石井 大海 on 2014/10/19.
//  Copyright (c) 2014年 Hiromi ISHII. All rights reserved.
//

#import "PreferencesDelegate.h"

@interface PreferencesDelegate ()

@property (weak) IBOutlet NSWindow *window;
@end

NSString *kUserDefaultName = @"group.konn-san.com.inputmethod.hSKK";

@implementation PreferencesDelegate
{
    NSUserDefaults *defaults;
    NSUserDefaultsController *ctrl;
}

- (void)applicationWillHide:(NSNotification *)notification
{
    [defaults synchronize];
}

- (void)applicationWillResignActive:(NSNotification *)notification
{
    [defaults synchronize];
}


- (void)applicationWillBecomeActive:(NSNotification *)notification
{
    [defaults synchronize];
    [self.window makeKeyAndOrderFront:nil];
}

- (void)awakeFromNib {
    NSString *defaultsPath = [[NSBundle mainBundle] pathForResource:@"UserDefaults"
                                                             ofType:@"plist"];
    NSDictionary *dic = [NSDictionary dictionaryWithContentsOfFile:defaultsPath];
    NSArrayController *dictsCtrl = [[NSArrayController alloc]
                                    initWithContent: [dic objectForKey:@"otherDics"]];

    defaults = [[NSUserDefaults alloc] initWithSuiteName:kUserDefaultName];
    ctrl = [[NSUserDefaultsController alloc] initWithDefaults:defaults
                                                initialValues:dic];
    [ctrl setAppliesImmediately:YES];
    
    [dictsCtrl bind:@"contentArray" toObject:ctrl
        withKeyPath:@"values.otherDics"
            options:@{@"NSHandlesContentAsCompoundValue": @YES,
                      @"NSContinuouslyUpdatesValue": @YES}];
    [self.userDicField bind:@"value" toObject:ctrl
                withKeyPath:@"values.userDicPath"
                    options:@{@"NSContinuouslyUpdatesValue": @YES}];
    [self.candidateLabelField bind:@"value"
                          toObject:ctrl
                       withKeyPath:@"values.candidateLabel"
                           options:@{@"NSContinuouslyUpdatesValue": @YES}];
    [self.inlineCountField bind:@"value"
                       toObject:ctrl
                    withKeyPath:@"values.inlineCandidateCount"
                        options:@{@"NSContinuouslyUpdatesValue": @YES}];
    [self.inlineCountStepper bind:@"value"
                         toObject:ctrl
                      withKeyPath:@"values.inlineCandidateCount"
                          options:@{@"NSContinuouslyUpdatesValue": @YES}];
    [self.otherDicKindCol bind:@"selectedIndex"
                      toObject:dictsCtrl
                   withKeyPath:@"arrangedObjects.kind"
                       options:@{@"NSContinuouslyUpdatesValue": @YES,
                                 @"NSAllowsEditingMultipleValuesSelection": @YES,
                                 @"NSConditionallySetEditable": @YES,
                                 @"NSCreatesSortDescriptor": @YES,
                                 @"NSRaisesForNotApplicableKeys": @YES}];
    [self.otherDicsLocationCol bind:@"value"
                           toObject:dictsCtrl
                        withKeyPath:@"arrangedObjects.location"
                            options:@{@"NSContinuouslyUpdatesValue": @YES,
                                      @"NSAllowsEditingMultipleValuesSelection": @YES,
                                      @"NSConditionallySetEditable": @YES,
                                      @"NSCreatesSortDescriptor": @YES,
                                      @"NSRaisesForNotApplicableKeys": @YES}];
    [self.removeButton bind:@"enabled"
                   toObject:dictsCtrl withKeyPath:@"canRemove"
                    options:@{@"NSContinuouslyUpdatesValue": @YES}];
    [self.addButton setTarget:dictsCtrl];
    [self.addButton setAction:@selector(add:)];
    [self.removeButton setTarget:dictsCtrl];
    [self.removeButton setAction:@selector(remove:)];
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication*)sender {
    return YES;
}

- (void)applicationWillTerminate:(NSNotification *)aNotification {
    [defaults synchronize];
    // Insert code here to tear down your application
}

@end
