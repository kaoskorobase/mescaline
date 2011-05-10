//
//  MescalineViewController.h
//  Mescaline
//
//  Created by Stefan Kersten on 30.03.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "FeatureSpace.h"

@interface MescalineViewController : UIViewController <FeatureSpaceDelegate>
{	
	BOOL drag;
    FeatureSpace *fSpace;
}

@property (retain) IBOutlet FeatureSpace *fSpace;


@end

