//
//  MescalineViewController.m
//  Mescaline
//
//  Created by Stefan Kersten on 30.03.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "MescalineViewController.h"
#import "FakeModel.h"


@implementation MescalineViewController

@synthesize fSpace;

- (void)releaseOutlets{
    self.fSpace = nil;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    self.fSpace.delegate = self;
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
	NSArray *regionlist = [model getRegionList];
    NSEnumerator *e = [regionlist objectEnumerator];
    id object;
    while ((object = [e nextObject])) {
        CGPoint p = [object CGPointValue];
        double dist = sqrt(pow((p.x*height - currentPosition.x),2)  + pow((p.y*width - currentPosition.y),2));

    }

//	for (int i = 0; i < [regionlist count]; i++){
//		double dist = sqrt(pow((regionlist[i].x()*height - currentPosition.x),2)  + pow((regionlist[i].y()*width - currentPosition.y),2));
//		if (dist<=regionlist[i].size()) {
//			NSLog(@"%s\t%f\t%f","over circle, setting region to 1",regionlist[i].x()*height,currentPosition.x);
//			ret = YES;
//			regionsArray.at(i) = 1;
//			break;
//		} else {
//			//NSLog(@"%s\t%d\t%f","NOT over circle number:",i,dist);
//			regionsArray.at(i) = 0;
//			ret = NO;
//		}
//        
//	}
//	return ret;
    return YES;
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
