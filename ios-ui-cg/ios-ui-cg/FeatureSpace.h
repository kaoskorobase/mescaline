//
//  FeatureSpace.h
//  ios-ui-cg
//
//  Created by z on 10.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import <UIKit/UIKit.h>
@class FeatureSpace;

@protocol FeatureSpaceDelegate
- (NSArray *)getPoints:(FeatureSpace *)requestor;
- (NSArray *)getRegions:(FeatureSpace *)requestor;
- (BOOL)checkIfOverRegion:(CGPoint)position;
//- (void)scaleRegion:(CGFloat)scale;
- (void)moveRegion:(CGPoint)position;

@end

@interface FeatureSpace : UIView {
    id <FeatureSpaceDelegate> delegate;
    CGFloat scale;
}
@property CGFloat scale;
@property (assign) id <FeatureSpaceDelegate> delegate;
- (void)pinch:(UIPinchGestureRecognizer *)gesture;
@end
