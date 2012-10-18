//
//  MameRecipe.h
//  MameRecipe
//
//  Created by SHIBATA Hiroshi on 2012/10/18.
//  Copyright (c) 2012年 SHIBATA Hiroshi. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface MameRecipe : NSObject

@property(nonatomic, copy) NSString *title;
@property(nonatomic, copy) NSString *directions;
@property(nonatomic, strong) UIImage *image;

@end
