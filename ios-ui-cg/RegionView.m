//
//  RegionView.m
//  ios-ui-cg
//
//  Created by z on 22.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import "RegionView.h"
#import "Region.h"

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
    //NSLog(@"loading subview");
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
