//
//  MameRecipeDirectionsEditorViewController.h
//  MameStoryBoardRecipe
//
//  Created by SHIBATA Hiroshi on 2013/02/13.
//  Copyright (c) 2013年 SHIBATA Hiroshi. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "MameRecipe.h"

@interface MameRecipeDirectionsEditorViewController : UIViewController
<UITextViewDelegate>

@property(nonatomic, strong) MameRecipe *recipe;
@property(nonatomic, strong) IBOutlet UITextView *textView;
@end
