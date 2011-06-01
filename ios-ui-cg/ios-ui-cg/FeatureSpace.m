//
//  FeatureSpace.m
//  ios-ui-cg
//
//  Created by z on 10.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import "FeatureSpace.h"
#import "Region.h"
#import <QuartzCore/QuartzCore.h>


@implementation FeatureSpace

@synthesize delegate;
@synthesize regionZoomed;
//@synthesize scale;

- (void)setup
{
    self.contentMode = UIViewContentModeRedraw;
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        [self setup];

    }
        return self;

}

- (void)awakeFromNib
{
    [self setup];
}

+ (BOOL)scaleIsValid:(CGFloat)aScale
{
    return ((aScale > 0) && (aScale <= 1));
}

#define DEFAULT_SCALE 1.0;

- (void)setScale:(CGFloat)newScale
{
    if([FeatureSpace scaleIsValid:scale]){
        scale = newScale;
    }
}

- (CGFloat)scale
{
    return [FeatureSpace scaleIsValid:scale] ? scale : DEFAULT_SCALE;
}



- (void)drawUnitatPoint:(CGPoint)p withRadius:(CGFloat)radius inContext:(CGContextRef)context
{
    UIGraphicsPushContext(context);
    CGContextBeginPath(context);
    CGContextAddArc(context, p.x * self.bounds.size.width, p.y * self.bounds.size.height, radius, 0, 2*M_PI, YES);
    CGContextStrokePath(context);
    UIGraphicsPopContext();
}


- (void)drawRegionatPoint:(CGPoint)p withRadius:(CGFloat)radius andColor:(UIColor*)color inContext:(CGContextRef)context
{
    UIGraphicsPushContext(context);
    CGContextBeginPath(context);
    CGColorRef cr = [color CGColor];
    CGContextSetFillColorWithColor(context,cr);
    CGContextAddArc(context, p.x * self.bounds.size.width, p.y * self.bounds.size.height, radius, 0, 2*M_PI, YES);
    CGContextFillPath(context);
    CGContextStrokePath(context);
    UIGraphicsPopContext();
}


- (void)drawRegions:(CGContextRef)context
{
    NSArray *reg = [self.delegate getRegions:self];
    NSEnumerator *e = [reg objectEnumerator];
    Region * object;
    while ((object = [e nextObject])) {
        CGPoint p = [object.location CGPointValue];
        UIColor* color = object.color;
        float rad = object.rad;
        [self drawRegionatPoint:p withRadius:rad andColor:color inContext:context];
    }
}   

- (void)drawPoints:(CGContextRef)context
{
    NSArray *points = [self.delegate getPoints:self];
    
    NSEnumerator *e = [points objectEnumerator];
    Region * object;
    while ((object = [e nextObject])) {
        CGPoint p = [object.location CGPointValue];
        [self drawUnitatPoint:p withRadius:object.rad inContext:context];
    }
}

// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
    CGContextRef context = UIGraphicsGetCurrentContext();
   // [self drawRegions:context];
    [self drawPoints:context];
}



- (void)adjustAnchorPointForGestureRecognizer:(UIGestureRecognizer *)gestureRecognizer {
    if (gestureRecognizer.state == UIGestureRecognizerStateBegan) {
        UIView *fSpace = gestureRecognizer.view;
        CGPoint locationInView = [gestureRecognizer locationInView:fSpace];
        CGPoint locationInSuperview = [gestureRecognizer locationInView:fSpace.superview];        
        self.layer.anchorPoint = CGPointMake(locationInView.x / fSpace.bounds.size.width, locationInView.y / fSpace.bounds.size.height);
        fSpace.center = locationInSuperview;
    }
}

- (void)panFeatureSpace:(UIPanGestureRecognizer *)gestureRecognizer
{
    UIView *fSpace = [gestureRecognizer view];
    [self adjustAnchorPointForGestureRecognizer:gestureRecognizer];
    if ([gestureRecognizer state] == UIGestureRecognizerStateBegan) 
    {
//        [self.superview bringSubviewToFront:self];
    }

    if ([gestureRecognizer state] == UIGestureRecognizerStateBegan || [gestureRecognizer state] == UIGestureRecognizerStateChanged) {
        CGPoint translation = [gestureRecognizer translationInView:fSpace];
        [fSpace setCenter:CGPointMake([fSpace center].x + translation.x, [fSpace center].y + translation.y)];
        [gestureRecognizer setTranslation:CGPointZero inView:fSpace];
        //CGPoint p = CGPointMake(self.frame.origin.x/self.superview.bounds.size.width,self.frame.origin.y/self.superview.bounds.size.height);
      //  [self.delegate updateRegion:self.regionIndex withPoint:(CGPoint)p andSize:(CGFloat)self.frame.size.width/2] ;
        
    }
}


- (void)scaleFeatureSpace:(UIPinchGestureRecognizer *)gestureRecognizer
{
    [self adjustAnchorPointForGestureRecognizer:gestureRecognizer];
    
    if ([gestureRecognizer state] == UIGestureRecognizerStateBegan || [gestureRecognizer state] == UIGestureRecognizerStateChanged) {
        [gestureRecognizer view].transform = CGAffineTransformScale([[gestureRecognizer view] transform], [gestureRecognizer scale], [gestureRecognizer scale]);
        [gestureRecognizer setScale:1];
        //CGPoint p = CGPointMake(self.frame.origin.x/self.superview.bounds.size.width,self.frame.origin.y/self.superview.bounds.size.height);
        //[self.delegate updateRegion:self.regionIndex withPoint:(CGPoint)p andSize:(CGFloat)self.frame.size.width/2] ;
        
    }
}

- (void)dealloc
{
    [super dealloc];
}

@end
