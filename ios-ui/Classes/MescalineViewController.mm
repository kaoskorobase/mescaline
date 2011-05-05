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
// NSLog(@"%i", [self.view test]);
//    [self.view makePoints:20];
    [super viewDidLoad];
	//gescheite init methode benötigt --> später...
	regionsArray = std::vector<int> ([[UIApplication sharedApplication].delegate model.numRegions],0);
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
	RegionList regionlist = [self.fmodel getRegionList];
	for (int i = 0; i < regionlist.size(); i++){
		double dist = sqrt(pow((regionlist[i].x()*height - currentPosition.x),2)  + pow((regionlist[i].y()*width - currentPosition.y),2));
		if (dist<=rs[i].size()) {
			NSLog(@"%s\t%f\t%f","over circle, setting region to 1",regionlist[i].x()*height,currentPosition.x);
			ret = YES;
			regionsArray.at(i) = 1;
			break;
		} else {
			//NSLog(@"%s\t%d\t%f","NOT over circle number:",i,dist);
			regionsArray.at(i) = 0;
			ret = NO;
		}
		//NSLog(@"%s\t%f\t%f\t%f\t%f","cx, mx, cy, my",rs[i].x()*height, currentPosition.x, rs[i].y()*width, currentPosition.y);	
		//		NSLog(@"%s\t%f\t%f\t%f\t%f","cx, mx, cy, my",rs[i].x()*height, currentPosition.x, rs[i].y()*width, currentPosition.y);	
	}
	return ret;
}

- (void)updateLabelsFromTouches:(NSSet *)touches {
    
    NSUInteger numTouches = [touches count];
    NSString *touchMsg = [[NSString alloc] initWithFormat:
                          @"%d touches detected", numTouches];
    NSLog(touchMsg);
}

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event{
	NSUInteger numTaps = [[touches anyObject] tapCount];
	NSUInteger numTouches = [touches count];
    [self updateLabelsFromTouches:touches];
	
    NSLog(@"%s\t%d","number of touches:",numTouches);
	UITouch *touch = [touches anyObject];
	CGPoint startPoint = [touch locationInView:self];
	if ([self checkIfOverRegion:(startPoint)]){
		//NSLog(@"i start drawing");
		drag = YES;
	} else {
		drag = NO;
	}
	
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event{
	for(int i=0;i<rs.size();i++){
		myRegions.at(i) = 0;
	}
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event{
	UITouch *touch = [touches anyObject];
	CGPoint currentPosition = [touch locationInView:self];
	if (drag) {		
		for(int i=0;i<rs.size();i++){			
			if(regionsArray.at(i) == 1){
				//NSLog(@"%d",myRegions.at(i));
				// get the position from the model
				CGPoint myPos = [[self fmodel] setPosition:currentPosition];
				// set region position
				rs.at(i) = Mescaline::Region(myPos.x/self.frame.size.height,myPos.y/self.frame.size.width,40);
				//NSLog(@"setting new position data");
			} else {
				//NSLog(@"%d",myRegions.at(i));
			}
			
		}
		[self.view setNeedsDisplay];
		
	}
	
	//NSLog(@"%f", myPos.x);
	//	NSLog(@"%f", currentPosition.y);
	
	
}
@end
