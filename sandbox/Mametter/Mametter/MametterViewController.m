//
//  MametterViewController.m
//  Mametter
//
//  Created by SHIBATA Hiroshi on 2012/08/28.
//  Copyright (c) 2012年 SHIBATA Hiroshi. All rights reserved.
//

#import "MametterViewController.h"
#import <Twitter/Twitter.h>

@interface MametterViewController ()

@end

@implementation MametterViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
}

- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return (interfaceOrientation != UIInterfaceOrientationPortraitUpsideDown);
}

-(IBAction) handleTweetButtonTapped: (id) sender
{
  if ([TWTweetComposeViewController canSendTweet]) {
    TWTweetComposeViewController *tweetVC =
      [[TWTweetComposeViewController alloc] init];
      [tweetVC setInitialText:
         @"I just finished the first project in iOS SDK Development. #pragsios"];
      [self presentViewController:tweetVC animated:YES completion:NULL];
  }else{
    NSLog (@"Can't send tweet");
  }
}
@end
