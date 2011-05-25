//
//  ios_ui_cgAppDelegate.h
//  ios-ui-cg
//
//  Created by z on 25.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface ios_ui_cgAppDelegate : NSObject <UIApplicationDelegate> {

}

@property (nonatomic, retain) IBOutlet UIWindow *window;

@property (nonatomic, retain, readonly) NSManagedObjectContext *managedObjectContext;
@property (nonatomic, retain, readonly) NSManagedObjectModel *managedObjectModel;
@property (nonatomic, retain, readonly) NSPersistentStoreCoordinator *persistentStoreCoordinator;

- (void)saveContext;
- (NSURL *)applicationDocumentsDirectory;

@end
