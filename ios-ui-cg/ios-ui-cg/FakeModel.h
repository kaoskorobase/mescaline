//
//  fake_model.h
//  Mescaline
//
//  Created by maule on 26.04.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "/usr/include/sqlite3.h"
#define kFilename    @"data.db"


@interface FakeModel : NSObject {
    sqlite3    *database;
@private
	NSArray* regionPositions; 
}
//@property int numRegions;

+(FakeModel*)sharedManager;

- (CGPoint)setPosition:(CGPoint)currentPosition;
- (NSArray *)getRegionList;
- (NSArray *)getPointList;
- (void)getData;
- (int)numRegions;

@end
