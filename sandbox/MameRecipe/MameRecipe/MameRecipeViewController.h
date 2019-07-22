//
//  MameRecipeViewController.h
//  MameRecipe
//
//  Created by SHIBATA Hiroshi on 2012/10/18.
//  Copyright (c) 2012年 SHIBATA Hiroshi. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "MameRecipe.h"

@interface MameRecipeViewController : UIViewController

@property (weak, nonatomic) IBOutlet UILabel *recipeTitle;
@property(nonatomic, strong) MameRecipe *recipe;
@property (weak, nonatomic) IBOutlet UITextView *directionsView;
@property (weak, nonatomic) IBOutlet UIImageView *imageView;
@property(nonatomic, strong) IBOutlet UILabel *prepTime;
@property(nonatomic, strong) NSNumberFormatter *formatter;
- (IBAction)dismiss:(id)sender;
@end
