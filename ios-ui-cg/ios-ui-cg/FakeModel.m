//
//  fake_model.m
//  Mescaline
//
//  Created by maule on 26.04.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "FakeModel.h"
#import "Region.h"

static FakeModel* sharedManager = nil;

@implementation FakeModel


@synthesize regions;
@synthesize points;

+ (FakeModel*)sharedManager {
    @synchronized(self) {
        if (sharedManager == nil) {
            [[self alloc] init]; // assignment not done here
        }
    }
    return sharedManager;
}

+ (id)allocWithZone:(NSZone *)zone {
    @synchronized(self) {
        if (sharedManager == nil) {
            sharedManager = [super allocWithZone:zone];
            return sharedManager;  // assignment and return on first allocation
        }
    }
    return nil; //on subsequent allocation attempts return nil
}




- (NSArray*) regionPositions{
    NSMutableArray* arr;
    for (int i = 0; i < 6; ++i) {
        double xvalue = (200 + 100 * cos(M_PI * ((double)i / (double)6)))/300;
        double yvalue = (200 + 100 * sin(M_PI * ((double)i / (double)6)))/300;
        CGPoint p = {xvalue,yvalue};
        NSValue* point = [NSValue valueWithCGPoint:p];
        [arr addObject:point];
    }
    return arr;
}
- (int)numRegions{
    return 6;
}

- (NSString *)dataFilePath {
    NSArray *paths = NSSearchPathForDirectoriesInDomains(
                                                         NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory = [paths objectAtIndex:0];
    return [documentsDirectory stringByAppendingPathComponent:kFilename];
}


//int numRegions = 6;

- (NSArray *)defaultRegions
{
    #define ARC4RANDOM_MAX 0x100000000LL

    NSMutableArray* result = [[[NSMutableArray alloc] init] autorelease];
    NSArray* colors = [NSArray arrayWithObjects:[UIColor blackColor], [UIColor redColor], [UIColor greenColor],[UIColor yellowColor],[UIColor blueColor],[UIColor grayColor],nil];
    
    for (int i = 0; i < 6; ++i) {
        Region *r = [[Region new] autorelease];
        r.rad=50.0;
        double xvalue = (0.5 + 0.3 * cos(M_PI * ((double)i / (double)3)));
        double yvalue = (0.5 + 0.3 * sin(M_PI * ((double)i / (double)3)));
        //NSLog(@"%f\t%f",xvalue,yvalue);
        CGPoint p = {xvalue,yvalue};
        NSValue* point = [NSValue valueWithCGPoint:p];
        r.location = point;
        r.color = [colors objectAtIndex:i];
        [result addObject:r];
    }
    return result;
}


    
- (NSArray *)defaultPoints
{
    #define ARC4RANDOM_MAX 0x100000000LL

    NSMutableArray* result = [[[NSMutableArray alloc] init] autorelease];

    for (int i = 0; i < 292; ++i) {
        float xvalue = floorf(((double)arc4random() / ARC4RANDOM_MAX) * 100.0f) / 100;
        float yvalue = floorf(((double)arc4random() / ARC4RANDOM_MAX) * 100.0f) / 100;
        CGPoint p = {xvalue,yvalue};
        NSValue* point = [NSValue valueWithCGPoint:p];
        Region *r = [[Region new] autorelease];
        r.rad=4.0;
        r.location = point;
        r.color = [UIColor redColor];
        [result addObject:r];
    }
    return result;
}

//- (void)getData {
//
//    if (sqlite3_open([[self dataFilePath] UTF8String], &database)
//        != SQLITE_OK) {
//        sqlite3_close(database);
//        NSAssert(0, @"Failed to open database");
//        NSLog(@"Failed to open database");
//
//    }
//    NSString *query = @"SELECT sf.id, sf.url, u.id as uid, f.value FROM (SourceFile sf  join Unit u on u.sourceFile=sf.id)  join Feature f on u.id = f.unit where f.descriptor =3";
//    sqlite3_stmt *statement;
//
//    if (sqlite3_prepare_v2( database, [query UTF8String],
//                           -1, &statement, nil) == SQLITE_OK) {
//        NSLog(@"database query fired");
//
//        while (sqlite3_step(statement) == SQLITE_ROW) {
//
//            //sqlite3_int64 row = sqlite3_column_int64(statement, 0);
//            //int *rowData = sqlite3_column_text(statement, 2);
//            //char *rowData = (char *)sqlite3_column_text(statement, 1);
//            //NSString *aUrl = [NSString stringWithUTF8String:(char *)sqlite3_column_text(statement, 1)];
//            //NSLog(@"s id = %i", row);
//            //float x = rand()/(float)RAND_MAX;
//            //float y = rand()/(float)RAND_MAX;
//            NSLog(@"database record fetched");
//  //          ps.push_back(Mescaline::Point(x, y));
//
//           
//        }
//        sqlite3_finalize(statement);
//    } 
//	//return ps;
//}

-(id)init {
    self = [super init];
    self.regions = [self defaultRegions];
    self.points = [self defaultPoints];
    return self;
}
@end
