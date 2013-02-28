//
//  MameRecipesListViewController.h
//  MameRecipe
//
//  Created by SHIBATA Hiroshi on 2012/12/24.
//  Copyright (c) 2012年 SHIBATA Hiroshi. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "MameRecipesListDataSource.h"

@interface MameRecipesListViewController : UITableViewController
@property(nonatomic, strong) id <MameRecipesListDataSource> dataSource;

- (void)finishedEditingRecipe:(MameRecipe *)recipe;
@end