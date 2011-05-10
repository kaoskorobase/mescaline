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
    NSLog(@"%@",[model getPointList]);
    return [model getPointList];
}
- (NSArray *)getRegions:(FeatureSpace *)requestor
{
    FakeModel* model =  [FakeModel sharedManager];
    NSLog(@"%@",[model getRegionList]);
    return [model getRegionList];
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
