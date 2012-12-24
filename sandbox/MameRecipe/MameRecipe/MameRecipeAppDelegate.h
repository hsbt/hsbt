//
//  MameRecipeAppDelegate.h
//  MameRecipe
//
//  Created by SHIBATA Hiroshi on 2012/10/18.
//  Copyright (c) 2012年 SHIBATA Hiroshi. All rights reserved.
//

#import <UIKit/UIKit.h>

@class MameRecipesListViewController;

@interface MameRecipeAppDelegate : UIResponder <UIApplicationDelegate>
@property (strong, nonatomic) UIWindow *window;
@property (strong, nonatomic) MameRecipesListViewController *viewController;
@property (copy, nonatomic) NSArray *recipes;

@end