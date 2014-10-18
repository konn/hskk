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
    [[[NSUserDefaults alloc] initWithSuiteName:kUserDefaultName] synchronize];
    [self.window makeKeyAndOrderFront:nil];
}

- (void)awakeFromNib {
    NSString *defaultsPath = [[NSBundle mainBundle] pathForResource:@"UserDefaults"
                                                             ofType:@"plist"];
    NSDictionary *dic = [NSDictionary dictionaryWithContentsOfFile:defaultsPath];

    defaults = [[NSUserDefaults alloc] initWithSuiteName:kUserDefaultName];
    ctrl = [[NSUserDefaultsController alloc] initWithDefaults:defaults
                                                initialValues:dic];
    NSLog(@"initialized with: %@", [dic description]);
    [ctrl setAppliesImmediately:YES];
    
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
                      toObject:ctrl
                   withKeyPath:@"values.otherDics.kind"
                       options:@{@"NSContinuouslyUpdatesValue": @YES}];
    [self.otherDicsLocationCol bind:@"value"
                           toObject:ctrl
                        withKeyPath:@"values.otherDics.location"
                            options:@{@"NSContinuouslyUpdatesValue": @YES}];
    NSLog(@"Goooood morning!!!");
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication*)sender {
    return YES;
}

- (void)applicationWillTerminate:(NSNotification *)aNotification {
    [defaults synchronize];
    // Insert code here to tear down your application
}

@end
