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

- (void)schas{
    Region * r = [self.regions objectAtIndex:3];
    float xvalue = floorf(((double)arc4random() / ARC4RANDOM_MAX) * 100.0f) / 100;
    float yvalue = floorf(((double)arc4random() / ARC4RANDOM_MAX) * 100.0f) / 100;
    CGPoint p = {xvalue,yvalue};
    NSValue* point = [NSValue valueWithCGPoint:p];
    r.location = point;
    [[NSNotificationCenter defaultCenter]
     postNotificationName:@"regionUpdate" object:nil];
    
}

- (void)startRepeatingTimer{
    
    NSTimer *timer = [NSTimer scheduledTimerWithTimeInterval:1.5 target:self selector:@selector(schas) userInfo:@"schasikov" repeats:YES];
//    self.repeatingTimer = timer;
}
-(id)init {
    self = [super init];
    self.regions = [self defaultRegions];
    self.points = [self defaultPoints];
    [self startRepeatingTimer];
    return self;
}
@end
