//
//  HaskellDrawAppDelegate.m
//  HaskellDraw
//
//  Created by Stephen Blackheath on 20/05/09.
//  Copyright __MyCompanyName__ 2009. All rights reserved.
//

#import "HaskellDrawAppDelegate.h"

@implementation HaskellDrawAppDelegate

@synthesize window;


- (void)applicationDidFinishLaunching:(UIApplication *)application {    

    // Override point for customization after application launch
    [window makeKeyAndVisible];
}


- (void)dealloc {
    [window release];
    [super dealloc];
}


@end
