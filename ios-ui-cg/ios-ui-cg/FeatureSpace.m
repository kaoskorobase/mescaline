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
@synthesize scale;

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
    self.scale = 1.0;
    return self;

}

- (void)awakeFromNib
{
    [self setup];
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
    
    
    
    //    // Define colors
    CGColorRef cr = [color CGColor];
    CGContextSetFillColorWithColor(context,cr);
    CGContextAddArc(context, p.x * self.bounds.size.width, p.y * self.bounds.size.height, radius, 0, 2*M_PI, YES);
    //CGContextStrokePath(context);
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
    [self drawRegions:context];
    [self drawPoints: [self.delegate getPoints:self] inContext:context];
}


- (void)pinch:(UIPinchGestureRecognizer *)gesture
{
    if((gesture.state == UIGestureRecognizerStateChanged) ||
       (gesture.state == UIGestureRecognizerStateEnded)){
        self.scale *=gesture.scale;
//        NSLog(@"%f", self.scale);
        CGPoint p = [gesture locationInView:self];
        if(![self.delegate checkIfOverRegion:p]){
            CGAffineTransform t = CGAffineTransformMakeScale(gesture.scale, gesture.scale);
            self.transform = t;
            NSLog(@"zooming region");
        }else{
            NSLog(@"zooming view");
        }
        //self.contentScaleFactor = gesture.scale;
        
        //[self setNeedsDisplay];
    }
    if(gesture.state == UIGestureRecognizerStateEnded){
        gesture.scale = 1;

    }
    
}




- (void)dealloc
{
    [super dealloc];
}

@end
