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
#import "RegionView.h"
#import <QuartzCore/QuartzCore.h>


@implementation MescalineViewController

@synthesize fSpace;
- (void)releaseOutlets{
    self.fSpace = nil;
}

- (NSArray *)getPoints:(FeatureSpace *)requestor
{
    FakeModel* model =  [FakeModel sharedManager];
    return model.points;
}
- (NSArray *)getRegions:(FeatureSpace *)requestor
{
    FakeModel* model =  [FakeModel sharedManager];
    return model.regions;
}


- (Region *)getRegion:(RegionView *)requestor withId:(int)regionId
{
    FakeModel* model =  [FakeModel sharedManager];
    return [model.regions objectAtIndex:regionId];

}



- (void)adjustAnchorPointForGestureRecognizer:(UIGestureRecognizer *)gestureRecognizer {
    if (gestureRecognizer.state == UIGestureRecognizerStateBegan) {
        UIView *regionView = gestureRecognizer.view;
        CGPoint locationInView = [gestureRecognizer locationInView:regionView];
        CGPoint locationInSuperview = [gestureRecognizer locationInView:regionView.superview];
        
        regionView.layer.anchorPoint = CGPointMake(locationInView.x / regionView.bounds.size.width, locationInView.y / regionView.bounds.size.height);
        regionView.center = locationInSuperview;
    }
}


- (void)panRegion:(UIPanGestureRecognizer *)gestureRecognizer
{
    UIView *region = [gestureRecognizer view];
    
    [self adjustAnchorPointForGestureRecognizer:gestureRecognizer];
    
    if ([gestureRecognizer state] == UIGestureRecognizerStateBegan || [gestureRecognizer state] == UIGestureRecognizerStateChanged) {
        CGPoint translation = [gestureRecognizer translationInView:[region superview]];
        
        [region setCenter:CGPointMake([region center].x + translation.x, [region center].y + translation.y)];
        [gestureRecognizer setTranslation:CGPointZero inView:[region superview]];
    }
}

- (void)scaleRegion:(UIPinchGestureRecognizer *)gestureRecognizer
{
    [self adjustAnchorPointForGestureRecognizer:gestureRecognizer];
    
    if ([gestureRecognizer state] == UIGestureRecognizerStateBegan || [gestureRecognizer state] == UIGestureRecognizerStateChanged) {
        [gestureRecognizer view].transform = CGAffineTransformScale([[gestureRecognizer view] transform], [gestureRecognizer scale], [gestureRecognizer scale]);
        [gestureRecognizer setScale:1];
        [[gestureRecognizer view] setNeedsDisplay];
    }
}




// adds a set of gesture recognizers to one of our region subviews
- (void)addGestureRecognizersToRegionView:(UIView *)regionView
{
//    UIRotationGestureRecognizer *rotationGesture = [[UIRotationGestureRecognizer alloc] initWithTarget:self action:@selector(rotateRegion:)];
//    [regionView addGestureRecognizer:rotationGesture];
//    [rotationGesture release];
    
    UIPinchGestureRecognizer *pinchGesture = [[UIPinchGestureRecognizer alloc] initWithTarget:self action:@selector(scaleRegion:)];
    [pinchGesture setDelegate:self];
    [regionView addGestureRecognizer:pinchGesture];
    [pinchGesture release];
    
    UIPanGestureRecognizer *panGesture = [[UIPanGestureRecognizer alloc] initWithTarget:self action:@selector(panRegion:)];
    [panGesture setMaximumNumberOfTouches:2];
    [panGesture setDelegate:self];
    [regionView addGestureRecognizer:panGesture];
    [panGesture release];
    
//    UILongPressGestureRecognizer *longPressGesture = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(showResetMenu:)];
//    [regionView addGestureRecognizer:longPressGesture];
//    [longPressGesture release];
}

//- (BOOL)checkIfOverRegion:(CGPoint)currentPosition
//{
//	
//	CGRect bounds = [self.view bounds];
//	int width = bounds.size.width;
//	int height = bounds.size.height;
//	BOOL ret;
//    FakeModel* model =  [FakeModel sharedManager];
//	NSArray *regionlist = model.regions;
////    NSEnumerator *e = [regionlist objectEnumerator];
//    Region * object;
//    //while ((object = [e nextObject])) {
//    for (int i=0; i< [regionlist count]; i++) {
//        object = [regionlist objectAtIndex:i];
//        CGPoint p = [object.location CGPointValue];
//        double dist = sqrt(pow((p.x*width - currentPosition.x),2)  + pow((p.y*height - currentPosition.y),2));
////        NSLog(@"%f",object.rad);
//        if (dist<=object.rad) {
//            NSLog(@"%s\t%f\t%f","over circle, xvalue: ---> ",p.x*height,currentPosition.x);
//            //NSLog(@"over circle");
//            object.touched = YES;
//                ret = YES;
//                break;
//            } else {
//                //NSLog(@"NOT over circle number:");
//                object.touched = NO;
//                ret = NO;
//         }
//
//    }
//	return ret;
//}






- (void)addRegionsToView
{
    NSArray * regions = [self getRegions:self.fSpace];
    for (int i=0; i< [regions count]; i++) {
        Region * object;
        object = [regions objectAtIndex:i];
        CGPoint p = [object.location CGPointValue];
        CGRect myRegionRect = CGRectMake(p.x * self.fSpace.bounds.size.width, p.y * self.fSpace.bounds.size.height, object.rad * 2, object.rad*2);    
        UIView * tmp = [[RegionView alloc] initWithFrame:myRegionRect];
        [tmp setRegionIndex:i];
        [self.fSpace addSubview:tmp];
        [tmp setDelegate:self];
        [self addGestureRecognizersToRegionView:tmp];
        [tmp release];
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


- (void)viewDidLoad {
    [super viewDidLoad];
    self.fSpace.delegate = self;
    UIGestureRecognizer *pinchgr = [[UIPinchGestureRecognizer alloc] initWithTarget:self.fSpace action:@selector(pinch:)];
//    UIGestureRecognizer *pangr = [[UIPanGestureRecognizer alloc] initWithTarget:self.fSpace action:@selector(pan:)];
//    
    [self.fSpace addGestureRecognizer:pinchgr];
//    [self.fSpace addGestureRecognizer:pangr];
    [self addRegionsToView];
    
    [pinchgr release];
//    [pangr release];
    
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
