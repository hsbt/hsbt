//
//  MameRecipeEditorViewController.h
//  MameStoryBoardRecipe
//
//  Created by SHIBATA Hiroshi on 2013/01/17.
//  Copyright (c) 2013年 SHIBATA Hiroshi. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "Mamerecipe.h"

@interface MameRecipeEditorViewController : UIViewController
<UITextFieldDelegate>

@property(nonatomic, strong) MameRecipe *recipe;
@property(nonatomic, strong) NSNumberFormatter *formatter;
@property(nonatomic, strong) IBOutlet UITextField *titleField;
@property(nonatomic, strong) IBOutlet UITextView *directionsText;
@property(nonatomic, strong) IBOutlet UILabel *prepTimeLabel;
@property(nonatomic, strong) IBOutlet UIImageView *recipeImage;
@property(nonatomic, strong) IBOutlet UIStepper *prepTimeStepper;

@end
