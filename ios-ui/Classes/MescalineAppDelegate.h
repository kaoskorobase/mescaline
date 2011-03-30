//
//  MescalineAppDelegate.h
//  Mescaline
//
//  Created by Stefan Kersten on 30.03.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@class MescalineViewController;

@interface MescalineAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    MescalineViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet MescalineViewController *viewController;

@end

