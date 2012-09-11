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
@synthesize twitterWebView;

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
}

- (void)viewDidUnload
{
    [self setTwitterWebView:nil];
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
      [tweetVC setInitialText: NSLocalizedString(
         @"I just finished the first project in iOS SDK Development. #pragsios",
         nil)];
      [self presentViewController:tweetVC animated:YES completion:NULL];
  }else{
    NSLog (@"Can't send tweet");
  }
}

-(IBAction) handleShowMyTweetsTapped: (id) sender {
    [self.twitterWebView loadRequest:
     [NSURLRequest requestWithURL:
      [NSURL URLWithString:@"http://www.twitter.com/hsbt"]]];
}

@end
