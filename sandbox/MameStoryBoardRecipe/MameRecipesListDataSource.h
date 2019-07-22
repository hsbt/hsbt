//
//  MameRecipesListDataSource.h
//  MameRecipe
//
//  Created by SHIBATA Hiroshi on 2012/12/24.
//  Copyright (c) 2012年 SHIBATA Hiroshi. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "MameRecipe.h"

@protocol MameRecipesListDataSource <NSObject>
- (NSInteger)recipeCount;
- (MameRecipe *)recipeAtIndex:(NSInteger)index;
- (void)deleteRecipeAtIndex:(NSInteger)index;
- (MameRecipe *)createNewRecipe;
- (NSUInteger)indexOfRecipe:(MameRecipe *)recipe;
@end
