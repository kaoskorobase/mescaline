//
//  MescalineViewController.m
//  Mescaline
//
//  Created by Stefan Kersten on 30.03.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "MescalineViewController.h"

@implementation MescalineViewController

//- (FakeModel *)fmodel
//{
//	if (!fmodel) {
//		fmodel = [[FakeModel alloc] init];
//	}
//	return fmodel;
//}

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


/*
// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
    [super viewDidLoad];
}
*/


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
//
//- (void)checkIfOverRegion:(CGPoint)currentPosition
//{
//	NSLog(@"test");
//}
//
//- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event{
//	NSUInteger numTaps = [[touches anyObject] tapCount];
//	NSUInteger numTouches = [touches count];
//	
//	UITouch *touch = [touches anyObject];
//	CGPoint startPoint = [touch locationInView:self.view];
//	[self checkIfOverRegion:(startPoint)];
////	NSLog(@"%f", startPoint.x);
////	NSLog(@"%f", startPoint.y);
//}
//
//- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event{
//	
//	UITouch *touch = [touches anyObject];
//	CGPoint currentPosition = [touch locationInView:self.view];
//	CGPoint myPos = [[self fmodel] setPosition:currentPosition];
//	//NSLog(@"%f", myPos.x);
////	NSLog(@"%f", currentPosition.y);
//	
//	
//}

@end
