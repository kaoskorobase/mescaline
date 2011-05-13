//
//  Region.m
//  ios-ui-cg
//
//  Created by z on 12.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import "Region.h"


@implementation Region

@synthesize location;
@synthesize rad;
@synthesize color;

- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}

- (void)dealloc
{
    [super dealloc];
}

@end
