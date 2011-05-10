//
//  MescalineViewController.m
//  Mescaline
//
//  Created by Stefan Kersten on 30.03.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "MescalineViewController.h"
#import "FakeModel.h"
#include "GlobalTypes.h"
#include <cairo/cairo.h>
#import "CGView.h"

@implementation MescalineViewController



/*
// The designated initializer. Override to perform setup that is required before the view is loaded.
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}
*/

/*
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView {
}
*/


// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
    [super viewDidLoad];
    FakeModel* model =  [FakeModel sharedManager];
	regionsArray = std::vector<int> ([model numRegions],0);
//    CGView *cgv = [[CGView alloc] initWithFrame:self.view.frame];
//    self.view = cgv;
//    [cgv release];
    
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
}


- (void)dealloc {
    [super dealloc];
}

int active = rand()%45;

- (BOOL)checkIfOverRegion:(CGPoint)currentPosition
{
	
	CGRect bounds = [self.view bounds];
	int width = bounds.size.width;
	int height = bounds.size.height;
	BOOL ret;
    FakeModel* model =  [FakeModel sharedManager];
	RegionList regionlist = [model getRegionList];
    
	for (int i = 0; i < regionlist.size(); i++){
		double dist = sqrt(pow((regionlist[i].x()*height - currentPosition.x),2)  + pow((regionlist[i].y()*width - currentPosition.y),2));
		if (dist<=regionlist[i].size()) {
			NSLog(@"%s\t%f\t%f","over circle, setting region to 1",regionlist[i].x()*height,currentPosition.x);
			ret = YES;
			regionsArray.at(i) = 1;
			break;
		} else {
			//NSLog(@"%s\t%d\t%f","NOT over circle number:",i,dist);
			regionsArray.at(i) = 0;
			ret = NO;
		}

	}
	return ret;
}



- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event{
	//NSUInteger numTaps = [[touches anyObject] tapCount];
	NSUInteger numTouches = [touches count];
    //[self updateLabelsFromTouches:touches];
	
    NSLog(@"%s\t%d","number of touches:",numTouches);
	UITouch *touch = [touches anyObject];
	CGPoint startPoint = [touch locationInView:self.view];
	if ([self checkIfOverRegion:(startPoint)]){
		//NSLog(@"i start drawing");
		drag = YES;
	} else {
		drag = NO;
	}

	
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event{
    FakeModel* model =  [FakeModel sharedManager];
    
	RegionList regionlist = [model getRegionList];
	for(int i=0;i<regionlist.size();i++){
		regionsArray.at(i) = 0;
	}
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event{
    FakeModel* model =  [FakeModel sharedManager];
	RegionList regionlist = [model getRegionList];
	UITouch *touch = [touches anyObject];
//    id cr = [self.view getCairoContext];

	CGPoint currentPosition = [touch locationInView:self.view];
	if (drag) {		
		for(int i=0;i<regionlist.size();i++){			
			if(regionsArray.at(i) == 1){
				//NSLog(@"%d",myRegions.at(i));
				// get the position from the model
				CGPoint myPos = [model setPosition:currentPosition];
				// set region position
				regionlist.at(i) = Mescaline::Region(myPos.x/self.view.frame.size.height,myPos.y/self.view.frame.size.width,40);
				//NSLog(@"setting new position data");
			} else {
				//NSLog(@"%d",myRegions.at(i));
			}
			
		}
//        NSLog(@"%@",[self.view getCairoContext]);
//        [self.view drawSequencer];
        [self.view redraw: [self.view getCairoContext] : regionlist];

        [self.view setNeedsDisplay];
	}
}

-(RegionList)getRegionList
{
    FakeModel* model =  [FakeModel sharedManager];
	RegionList regionlist = [model getRegionList];
    return regionlist;
}

@end
