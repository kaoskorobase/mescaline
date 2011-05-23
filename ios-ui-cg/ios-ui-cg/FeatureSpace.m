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


- (void)pinch:(UIPinchGestureRecognizer *)gesture
{
    if((gesture.state == UIGestureRecognizerStateChanged) ||
       (gesture.state == UIGestureRecognizerStateEnded)){
        //self.scale *=gesture.scale;
        self.scale += gesture.scale;
        NSLog(@"%f", self.scale);
        CGAffineTransform t = CGAffineTransformMakeScale(gesture.scale, gesture.scale);
        self.transform = t;

//        BOOL over;
//        for (int i=0; i< gesture.numberOfTouches; i++) {
//            //CGPoint p = [gesture locationInView:self];
//            CGPoint p = [gesture locationOfTouch:(NSUInteger)i inView:(UIView *)self];
//            if([self.delegate checkIfOverRegion:p]){
//                over = YES; 
//            } else {
//                over = NO;
//                break;
//            }
//        }
//        if(!over){
//            CGAffineTransform t = CGAffineTransformMakeScale(gesture.scale, gesture.scale);
//            self.transform = t;
//            //NSLog(@"zooming View");
//        }else{
//            //NSLog(@"zooming Region");
////            [self.delegate scaleRegion:gesture.scale];
////            [self setNeedsDisplay];
//            gesture.scale = 1;
//        }
        
    }    
}

- (void)pan:(UIPanGestureRecognizer *)gesture
{
    CGPoint startPoint = [gesture locationInView:self];
    BOOL drag;
    //int tappedRegionIndex;
    if (gesture.state == UIGestureRecognizerStateBegan) {
//        if ([self.delegate checkIfOverRegion:(startPoint)]){
//            NSLog(@"i start drawing");
//            drag = YES;
//            
//        } else {
//            drag = NO;
//        }
    }
    if(gesture.state == UIGestureRecognizerStateChanged) {
//        NSLog(@"panning has begun");
        
        if (drag){
//            NSLog(@"i start drawing");
//            [self.delegate moveRegion:[gesture locationInView:self]];
        } 
    }
    if(gesture.state == UIGestureRecognizerStateEnded) {
    }
}



- (void)dealloc
{
    [super dealloc];
}

@end
