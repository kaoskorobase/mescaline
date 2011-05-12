//
//  FeatureSpace.m
//  ios-ui-cg
//
//  Created by z on 10.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import "FeatureSpace.h"
#import "Region.h"


@implementation FeatureSpace

@synthesize delegate;

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
   
    return self;

}
- (void)drawUnitatPoint:(CGPoint)p withRadius:(CGFloat)radius inContext:(CGContextRef)context
{
    
    UIGraphicsPushContext(context);
    CGContextBeginPath(context);

    
    
    //    // Define colors
//    CGColorRef red = [[UIColor redColor] CGColor];
//    CGColorRef blue = [[UIColor blueColor] CGColor];
//    CGColorRef green = [[UIColor greenColor] CGColor];
//    CGColorRef yellow = [[UIColor yellowColor] CGColor];
    
    CGContextAddArc(context, p.x * self.bounds.size.width, p.y * self.bounds.size.height, radius, 0, 2*M_PI, YES);
    CGContextStrokePath(context);
    UIGraphicsPopContext();
}


- (void)drawRegionatPoint:(CGPoint)p withRadius:(CGFloat)radius inContext:(CGContextRef)context
{
    
    UIGraphicsPushContext(context);
    CGContextBeginPath(context);
    
    
    
    //    // Define colors
    CGColorRef red = [[UIColor redColor] CGColor];
        CGColorRef blue = [[UIColor blueColor] CGColor];
        CGColorRef green = [[UIColor greenColor] CGColor];
        CGColorRef yellow = [[UIColor yellowColor] CGColor];
    //CGContextSetRGBFillColor(context, 0.2, 0.5, 1.0, 1);
    CGContextSetFillColorWithColor(context,green);
    CGContextAddArc(context, p.x * self.bounds.size.width, p.y * self.bounds.size.height, radius, 0, 2*M_PI, YES);
    //CGContextStrokePath(context);
    CGContextFillPath(context);
    CGContextStrokePath(context);
    UIGraphicsPopContext();
}


- (void)drawRegions:(NSArray *)regionlist inContext:(CGContextRef)context
{
    NSArray *regions = [self.delegate getRegions:self];
    NSEnumerator *e = [regions objectEnumerator];
    Region * object;
    while ((object = [e nextObject])) {
        CGPoint p = [object.location CGPointValue];
        [self drawRegionatPoint:p withRadius:object.rad inContext:context];
    }
}   

- (void)drawPoints:(NSArray *)pointlist inContext:(CGContextRef)context
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
    [self drawRegions: [self.delegate getRegions:self] inContext:context];
    [self drawPoints: [self.delegate getPoints:self] inContext:context];
}



- (void)dealloc
{
    [super dealloc];
}

@end
