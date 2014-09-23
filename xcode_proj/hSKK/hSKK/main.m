//
//  main.m
//  hSKK
//
//  Created by 石井 大海 on 2014/09/23.
//  Copyright (c) 2014年 Hiromi ISHII. All rights reserved.
//
#import <Cocoa/Cocoa.h>
#import <InputMethodKit/InputMethodKit.h>

const NSString* kConnectionName = @"HSKK_Connection";
IMKServer*      server;

int main(int argc, const char * argv[]) {
    NSString *identifier;
    identifier = [[NSBundle mainBundle] bundleIdentifier];
    NSLog(@"identifier: %@", identifier);
    server = [[IMKServer alloc] initWithName:(NSString*)kConnectionName
                            bundleIdentifier:identifier];
    
    NSLog(@"server bundle: %@", [[server bundle] description]);
    
    [[NSBundle mainBundle] loadNibNamed: @"MainMenu"
                                  owner: [NSApplication sharedApplication]
                        topLevelObjects: nil];
    [[NSApplication sharedApplication] run];
    return 0;
}
