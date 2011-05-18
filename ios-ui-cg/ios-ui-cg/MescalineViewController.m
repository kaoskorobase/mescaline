//
//  MescalineViewController.m
//  Mescaline
//
//  Created by Stefan Kersten on 30.03.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "MescalineViewController.h"
#import "FakeModel.h"
#import "Region.h"

@implementation MescalineViewController

@synthesize fSpace;

- (void)releaseOutlets{
    self.fSpace = nil;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    self.fSpace.delegate = self;
    UIGestureRecognizer *pinchgr = [[UIPinchGestureRecognizer alloc] initWithTarget:self.fSpace action:@selector(pinch:)];
    [self.fSpace addGestureRecognizer:pinchgr];
    [pinchgr release];
    
}


- (NSArray *)getPoints:(FeatureSpace *)requestor
{
    FakeModel* model =  [FakeModel sharedManager];
    return [model getPointList];
}
- (NSArray *)getRegions:(FeatureSpace *)requestor
{
    FakeModel* model =  [FakeModel sharedManager];
    return model.regions;
}




- (BOOL)checkIfOverRegion:(CGPoint)currentPosition
{
	
	CGRect bounds = [self.view bounds];
	int width = bounds.size.width;
	int height = bounds.size.height;
	BOOL ret;
    FakeModel* model =  [FakeModel sharedManager];
	NSArray *regionlist = model.regions;
    NSEnumerator *e = [regionlist objectEnumerator];
    Region * object;
    while ((object = [e nextObject])) {
        CGPoint p = [object.location CGPointValue];
        double dist = sqrt(pow((p.x*width - currentPosition.x),2)  + pow((p.y*height - currentPosition.y),2));
        if (dist<=object.rad) {
            NSLog(@"%s\t%f\t%f","over circle, xvalue: ---> ",p.x*height,currentPosition.x);
            //NSLog(@"over circle");
            object.touched = YES;
                ret = YES;
                break;
            } else {
                //NSLog(@"%s\t%d\t%f","NOT over circle number:",i,dist);
                object.touched = NO;
                ret = NO;
         }

    }
	return ret;
}



- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event{
	//NSUInteger numTaps = [[touches anyObject] tapCount];
	//NSUInteger numTouches = [touches count];
    //[self updateLabelsFromTouches:touches];
	for (UITouch *touch in touches){
    //NSLog(@"%s\t%d","number of touches:",numTouches);
	//UITouch *touch = [touches anyObject];
        CGPoint startPoint = [touch locationInView:self.view];
        if ([self checkIfOverRegion:(startPoint)]){
            NSLog(@"i start drawing");
            drag = YES;
        } else {
            drag = NO;
        }
    }    
	
}
//
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event{
    FakeModel* model =  [FakeModel sharedManager];
	NSArray *regionlist = model.regions;
	UITouch *touch = [touches anyObject];
    //    id cr = [self.view getCairoContext];
    CGRect bounds = [self.view bounds];
	int width = bounds.size.width;
	int height = bounds.size.height;
    int cnt=0;
    NSArray * orderedTouches = [touches allObjects];
           
 
        if (drag) {		
            NSEnumerator *e = [regionlist objectEnumerator];
            Region * object;
            while ((object = [e nextObject])) {
                if (object.touched){
                    CGPoint currentPosition = [[orderedTouches objectAtIndex:cnt] locationInView:self.view];                    
                    float scaledx = currentPosition.x / width;
                    float scaledy = currentPosition.y / height;
                    CGPoint p = {scaledx,scaledy};
                    NSValue* point = [NSValue valueWithCGPoint:p];
                    object.location = point;
                    cnt++;
                }
            }
            [self.view setNeedsDisplay];
        }
   
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event{
    FakeModel* model =  [FakeModel sharedManager];
    
	NSArray *regionlist = model.regions;

    NSEnumerator *e = [regionlist objectEnumerator];
    Region * object;
    while ((object = [e nextObject])) {
        object.touched = NO;
    }
}

// Override to allow orientations other than the default portrait orientation.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    return YES;
}

- (void)didReceiveMemoryWarning {
	// Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
	
	// Release any cached data, images, etc that aren't in use.
}

- (void)viewDidUnload {
	// Release any retained subviews of the main view.
	// e.g. self.myOutlet = nil;
    [self releaseOutlets];
}


- (void)dealloc {
    [self releaseOutlets];
    [super dealloc];
}



@end
