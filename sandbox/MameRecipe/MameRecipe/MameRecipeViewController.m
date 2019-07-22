//
//  MameRecipeViewController.m
//  MameRecipe
//
//  Created by SHIBATA Hiroshi on 2012/10/18.
//  Copyright (c) 2012年 SHIBATA Hiroshi. All rights reserved.
//

#import "MameRecipeViewController.h"

@interface MameRecipeViewController ()

@end

@implementation MameRecipeViewController

- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
    self.recipeTitle.text = self.recipe.title;
    self.directionsView.text = self.recipe.directions;
    self.prepTime.text = [self.formatter stringFromNumber:self.recipe.preparationTime];
    if(self.recipe.image) {
        self.imageView.image = self.recipe.image;
    }
}

- (void)viewDidLoad {
  [super viewDidLoad];
  self.formatter = [[NSNumberFormatter alloc] init];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (IBAction)dismiss:(id)sender {
  [self dismissViewControllerAnimated:YES completion:nil];
}
@end
