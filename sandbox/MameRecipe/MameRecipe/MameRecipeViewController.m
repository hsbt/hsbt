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
    if(self.recipe.image) {
        self.imageView.image = self.recipe.image;
    }
}

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
