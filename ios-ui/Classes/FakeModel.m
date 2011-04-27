//
//  fake_model.m
//  Mescaline
//
//  Created by maule on 26.04.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "FakeModel.h"


@implementation FakeModel


- (CGPoint)setPosition:(CGPoint)currentPosition
{
	//[currentPosition sendMessage:argument];
	NSLog(@"%f", currentPosition.x);
	NSLog(@"%f", currentPosition.y);
	return currentPosition;
}

@end
