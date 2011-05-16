//
//  Region.h
//  ios-ui-cg
//
//  Created by z on 12.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface Region : NSObject {

@public
    NSValue* location;
    float rad;
    UIColor* color;
    
}
@property (nonatomic,retain) NSValue* location;
@property (assign) float rad;
@property (assign) UIColor* color;

@end
