//
//  RegionView.m
//  ios-ui-cg
//
//  Created by z on 22.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import "RegionView.h"
#import "Region.h"
#import <QuartzCore/QuartzCore.h>

@implementation RegionView

@synthesize regionIndex;
@synthesize delegate;

- (void)setup
{
    self.contentMode = UIViewContentModeRedraw;
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        [self setup];
        self.opaque = NO;
               
    }
    return self;
    
}

- (void)adjustAnchorPointForGestureRecognizer:(UIGestureRecognizer *)gestureRecognizer {
    if (gestureRecognizer.state == UIGestureRecognizerStateBegan) {
        UIView *regionView = gestureRecognizer.view;
        CGPoint locationInView = [gestureRecognizer locationInView:regionView];
        CGPoint locationInSuperview = [gestureRecognizer locationInView:regionView.superview];        
        self.layer.anchorPoint = CGPointMake(locationInView.x / regionView.bounds.size.width, locationInView.y / regionView.bounds.size.height);
        regionView.center = locationInSuperview;
    }
}

- (void)panRegion:(UIPanGestureRecognizer *)gestureRecognizer
{
    UIView *regionView = [gestureRecognizer view];
    [self adjustAnchorPointForGestureRecognizer:gestureRecognizer];
    if ([gestureRecognizer state] == UIGestureRecognizerStateBegan) 
    {
        [self.superview bringSubviewToFront:self];
    }
    if ([gestureRecognizer state] == UIGestureRecognizerStateBegan || [gestureRecognizer state] == UIGestureRecognizerStateChanged) {
        CGPoint translation = [gestureRecognizer translationInView:[regionView superview]];
        [regionView setCenter:CGPointMake([regionView center].x + translation.x, [regionView center].y + translation.y)];
        [gestureRecognizer setTranslation:CGPointZero inView:[regionView superview]];
        CGPoint p = CGPointMake(self.frame.origin.x/self.superview.bounds.size.width,self.frame.origin.y/self.superview.bounds.size.height);
        [self.delegate updateRegion:self.regionIndex withPoint:(CGPoint)p andSize:(CGFloat)self.frame.size.width/2] ;

    }
}

- (void)tapRegion:(UITapGestureRecognizer *)gestureRecognizer
{
 //   if ([gestureRecognizer state] == UIGestureRecognizerStateBegan || [gestureRecognizer state] == UIGestureRecognizerStateChanged) {
        

        CGFloat s = 3;
        CGAffineTransform tr = CGAffineTransformScale(self.superview.transform, 3, 3);
        CGFloat h = self.superview.frame.size.height;
        CGFloat w = self.superview.frame.size.width;
        CGPoint location = [gestureRecognizer locationInView:self];
        [UIView animateWithDuration:2.5 delay:0 options:0 animations:^{
            self.superview.transform = tr;
            self.superview.center = CGPointMake(0,0);
            //self.superview.center = self.center;
            //self.superview.center = location;



        } completion:^(BOOL finished) {}];
//        [self adjustAnchorPointForGestureRecognizer:gestureRecognizer];
   // }
}



- (void)scaleRegion:(UIPinchGestureRecognizer *)gestureRecognizer
{
    [self adjustAnchorPointForGestureRecognizer:gestureRecognizer];
    
    if ([gestureRecognizer state] == UIGestureRecognizerStateBegan || [gestureRecognizer state] == UIGestureRecognizerStateChanged) {
        [gestureRecognizer view].transform = CGAffineTransformScale([[gestureRecognizer view] transform], [gestureRecognizer scale], [gestureRecognizer scale]);
        [gestureRecognizer setScale:1];
        CGPoint p = CGPointMake(self.frame.origin.x/self.superview.bounds.size.width,self.frame.origin.y/self.superview.bounds.size.height);
        [self.delegate updateRegion:self.regionIndex withPoint:(CGPoint)p andSize:(CGFloat)self.frame.size.width/2] ;
//        [self.superview setNeedsDisplay];

    }
}

- (void)drawRegionatPoint:(CGPoint)p withRadius:(CGFloat)radius andColor:(UIColor*)color inContext:(CGContextRef)context
{
    
    UIGraphicsPushContext(context);
    CGContextSetAlpha(context,0.8);
    CGContextBeginPath(context);
    CGColorRef cr = [color CGColor];
    CGContextSetFillColorWithColor(context,cr);
    CGContextAddArc(context, 0+radius,0+radius, radius, 0, 2*M_PI, YES);
    CGContextFillPath(context);
    CGContextStrokePath(context);
    UIGraphicsPopContext();
}


- (void)drawRegion:(CGContextRef)context
{
        Region *reg = [self.delegate getRegion:self withId:(int)self.regionIndex];
        CGPoint p = [reg.location CGPointValue];
        UIColor* color = reg.color;
        float rad = reg.rad;
        [self drawRegionatPoint:p withRadius:rad andColor:color inContext:context];
    
}   
//

// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
    CGContextRef context = UIGraphicsGetCurrentContext();
//    NSLog(@"%i",self.regionIndex);

    [self drawRegion:context];
    NSLog(@"drawing subview");
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
}
*/

- (void)dealloc
{
    [super dealloc];
}

@end
