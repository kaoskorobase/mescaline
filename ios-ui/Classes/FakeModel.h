//
//  fake_model.h
//  Mescaline
//
//  Created by maule on 26.04.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

#import <UIKit/UIKit.h>
#import "/usr/include/sqlite3.h"
#define kFilename    @"data.db"
#include "GlobalTypes.h"

@interface FakeModel : NSObject {
    sqlite3    *database;

}
- (NSString *)dataFilePath;
- (CGPoint)setPosition:(CGPoint)currentPosition;
//- (PointList *)makePoints;
@end
