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
    CGFloat rad;
    UIColor* color;
    BOOL touched;
    
}
@property (nonatomic,retain) NSValue* location;
@property (assign) CGFloat rad;
@property (assign) BOOL touched;
@property (assign) UIColor* color;

@end
