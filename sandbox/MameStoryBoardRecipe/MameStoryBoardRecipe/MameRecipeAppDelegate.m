//
//  MameRecipeAppDelegate.m
//  MameStoryBoardRecipe
//
//  Created by SHIBATA Hiroshi on 2012/11/28.
//  Copyright (c) 2012年 SHIBATA Hiroshi. All rights reserved.
//

#import "MameRecipeAppDelegate.h"
#import "MameRecipesListViewController.h"
#import "MameRecipesSource.h"

@implementation MameRecipeAppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
  UINavigationController *navigationController = (UINavigationController *)self.window.rootViewController;
  MameRecipesListViewController *controller = (MameRecipesListViewController *)navigationController.topViewController;
  controller.dataSource = [[MameRecipesSource alloc] init];
  return YES;
}

@end