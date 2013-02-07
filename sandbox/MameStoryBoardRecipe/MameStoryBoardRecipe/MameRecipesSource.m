//
//  MameRecipesSource.m
//  MameRecipe
//
//  Created by SHIBATA Hiroshi on 2012/12/24.
//  Copyright (c) 2012年 SHIBATA Hiroshi. All rights reserved.
//

#import "MameRecipesSource.h"
#import "MameRecipe.h"

@interface MameRecipesSource()
@property(nonatomic, strong) NSMutableArray *recipes;
@end

@implementation MameRecipesSource

- (NSArray *)recipes {
  if(!_recipes) {
    NSMutableArray *localRecipes = [NSMutableArray array];
    MameRecipe *recipe = [[MameRecipe alloc] init];
    recipe.directions = @"0 - Edamame 0";
    recipe.title = @"0 - One Fine Food";
    recipe.preparationTime = [NSNumber numberWithInt:30];
    recipe.image = [UIImage imageNamed:@"edamame.jpg"];
    [localRecipes addObject:recipe];

    recipe = [[MameRecipe alloc] init];
    recipe.directions = @"1 - Edamame 1";
    recipe.title = @"1 - Two Fine Food";
    recipe.preparationTime = [NSNumber numberWithInt:40];
    recipe.image = [UIImage imageNamed:@"edamame.jpg"];
    [localRecipes addObject:recipe];

    self.recipes = localRecipes;
  }
  return _recipes;
}

#pragma mark Recipe List Data Source Methods
- (NSInteger)recipeCount {
  return [self.recipes count];
}

- (MameRecipe *)recipeAtIndex:(NSInteger)index {
  return [self.recipes objectAtIndex:index];
}

- (void)deleteRecipeAtIndex:(NSInteger)index {
  [self.recipes removeObjectAtIndex:index];
}

- (MameRecipe *)createNewRecipe {
    MameRecipe *recipe = [[MameRecipe alloc] init];
    [self.recipes addObject:recipe];
    return recipe;
}

@end
