//
//  FeatureSpace.m
//  ios-ui-cg
//
//  Created by z on 10.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import "FeatureSpace.h"
#import "GlobalTypes.h"



@implementation FeatureSpace

@synthesize delegate;

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
   
    return self;

}

- (void)drawRegions
{
    NSArray *regions = [self.delegate getRegions:self];
    for(int i = 0; i <= [regions count]; i++){
        NSLog(@"drawing regions");
        
    }
    NSLog(@"%@", self.delegate);
    
}

- (void)drawPoints:(NSArray *)pointlist
{
    NSLog(@"drawing points");
}

// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
    [self drawRegions];
    //[self drawPoints: [self.delegate getPoints:self]];
}



- (void)dealloc
{
    [super dealloc];
}

@end
