//
//  RegionView.h
//  ios-ui-cg
//
//  Created by z on 22.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "Region.h"

@class RegionView;

@protocol RegionViewDelegate
- (Region *)getRegion:(RegionView *)requestor withId:(int)regionId;
- (void)updateRegion:(int)regionIndex withPoint:(CGPoint)newCenter;
@end

@interface RegionView : UIView 
{
id <RegionViewDelegate> delegate;

@public
    int regionIndex;
}
@property int regionIndex;
@property (assign) id <RegionViewDelegate> delegate;


@end
