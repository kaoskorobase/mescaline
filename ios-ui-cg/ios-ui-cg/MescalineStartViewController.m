//
//  MescalineStartViewController.m
//  ios-ui-cg
//
//  Created by z on 10.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import "MescalineStartViewController.h"
#import "MescalineViewController.h"

@implementation MescalineStartViewController

- (void)drawFeatureSpace{
    MescalineViewController *mvc = [[MescalineViewController alloc] init];
    [self.navigationController pushViewController:mvc animated:YES];
    [mvc release];
}
- (void)drawSequencer{
    
}

- (IBAction)showFeatureSpace{
    [self drawFeatureSpace];
}
- (IBAction)showSequencer{
    [self drawSequencer];
}


@end
