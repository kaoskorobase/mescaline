//
//  HaskellDrawAppDelegate.h
//  HaskellDraw
//
//  Created by Stephen Blackheath on 20/05/09.
//  Copyright __MyCompanyName__ 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface HaskellDrawAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;

@end

