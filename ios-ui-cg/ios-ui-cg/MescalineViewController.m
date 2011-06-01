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


- (void)updateRegion:(int)regionIndex withPoint:(CGPoint)newCenter andSize:(CGFloat)size
{
    FakeModel* model =  [FakeModel sharedManager];
    NSValue* point = [NSValue valueWithCGPoint:newCenter];
    Region * object = [model.regions objectAtIndex:regionIndex];
    object.location = point;
    object.rad = size;
    
}

// adds a set of gesture recognizers to one of our region subviews
- (void)addGestureRecognizersToRegionView:(UIView *)regionView
{
//    UIRotationGestureRecognizer *rotationGesture = [[UIRotationGestureRecognizer alloc] initWithTarget:self action:@selector(rotateRegion:)];
//    [regionView addGestureRecognizer:rotationGesture];
//    [rotationGesture release];
    
    UIPinchGestureRecognizer *pinchGesture = [[UIPinchGestureRecognizer alloc] initWithTarget:regionView action:@selector(scaleRegion:)];
    [pinchGesture setDelegate:self];
    [regionView addGestureRecognizer:pinchGesture];
    [pinchGesture release];
    
    UIPanGestureRecognizer *panGesture = [[UIPanGestureRecognizer alloc] initWithTarget:regionView action:@selector(panRegion:)];
    [panGesture setMaximumNumberOfTouches:2];
    [panGesture setDelegate:self];
    [regionView addGestureRecognizer:panGesture];
    [panGesture release];
    UITapGestureRecognizer *tapGesture = [[UITapGestureRecognizer alloc] initWithTarget:regionView action:@selector(tapRegion:)];
    tapGesture.numberOfTapsRequired = 2;
    [tapGesture setDelegate:self];
    [regionView addGestureRecognizer:tapGesture];
    [tapGesture release];

//    UILongPressGestureRecognizer *longPressGesture = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(showResetMenu:)];
//    [regionView addGestureRecognizer:longPressGesture];
//    [longPressGesture release];
}

- (void)addGestureRecognizersToFeatureSpaceView:(UIView *)FeatureSpaceView
{
    //    UIRotationGestureRecognizer *rotationGesture = [[UIRotationGestureRecognizer alloc] initWithTarget:self action:@selector(rotateRegion:)];
    //    [regionView addGestureRecognizer:rotationGesture];
    //    [rotationGesture release];
    

    
    UIPinchGestureRecognizer *pinchGesture = [[UIPinchGestureRecognizer alloc] initWithTarget:FeatureSpaceView action:@selector(scaleFeatureSpace:)];
    [pinchGesture setDelegate:self];
    [FeatureSpaceView addGestureRecognizer:pinchGesture];
    [pinchGesture release];
    
    UIPanGestureRecognizer *panGesture = [[UIPanGestureRecognizer alloc] initWithTarget:FeatureSpaceView action:@selector(panFeatureSpace:)];
    [panGesture setMaximumNumberOfTouches:2];
    [panGesture setDelegate:self];
    [FeatureSpaceView addGestureRecognizer:panGesture];
    [panGesture release];
//    UITapGestureRecognizer *tapGesture = [[UITapGestureRecognizer alloc] initWithTarget:FeatureSpaceView action:@selector(tapRegion:)];
//    tapGesture.numberOfTapsRequired = 2;
//    [tapGesture setDelegate:self];
//    [FeatureSpaceView addGestureRecognizer:tapGesture];
//    [tapGesture release];
    
    //    UILongPressGestureRecognizer *longPressGesture = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(showResetMenu:)];
    //    [regionView addGestureRecognizer:longPressGesture];
    //    [longPressGesture release];
}





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
    [self addGestureRecognizersToFeatureSpaceView:fSpace];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(updateRegions:) name:@"regionUpdate" object:nil];

    //UIGestureRecognizer *pinchgr = [[UIPinchGestureRecognizer alloc] initWithTarget:self.fSpace action:@selector(pinch:)];
//    UIGestureRecognizer *pangr = [[UIPanGestureRecognizer alloc] initWithTarget:self.fSpace action:@selector(pan:)];
//    
//    [self.fSpace addGestureRecognizer:pinchgr];
//    [self.fSpace addGestureRecognizer:pangr];
    [self addRegionsToView];
    
    
//    [pinchgr release];
//    [pangr release];
    
}

- (void)updateRegions:(NSNotification *)notification
{
    NSLog(@"notification recieved");
    for(UIView *view in self.fSpace.subviews){
        [view setNeedsDisplay];
    }
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

//- (id)init
//{
//    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(updateRegions:) name:@"regioUpdate" object:nil];
//    [super init];
//}

@end
