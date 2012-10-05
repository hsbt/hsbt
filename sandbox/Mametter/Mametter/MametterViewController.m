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
- (void) reloadTweets;
@property (weak, nonatomic) IBOutlet UIWebView *twitterWebView;
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
    if ([SLComposeViewController isAvailableForServiceType: SLServiceTypeTwitter]) {
        SLComposeViewController *tweetVC = [SLComposeViewController composeViewControllerForServiceType: SLServiceTypeTwitter];
        [tweetVC setInitialText: @"I just finished the first project in iOS SDK Development. #pragsios"];
        [self presentViewController:tweetVC animated:YES completion:NULL];
    }else{
        NSLog (@"Can't send tweet");
    }
}

-(void) reloadTweets {
    [self.twitterWebView loadRequest:
     [NSURLRequest requestWithURL:
      [NSURL URLWithString:@"http://www.twitter.com/hsbt"]]];
}

-(IBAction) handleShowMyTweetsTapped: (id) sender {
    if ([SLComposeViewController isAvailableForServiceType: SLServiceTypeTwitter]) {
        SLComposeViewController *tweetVC = [SLComposeViewController composeViewControllerForServiceType: SLServiceTypeTwitter];
        [tweetVC setInitialText: NSLocalizedString (
                                                    @"I just finished the first project in iOS SDK Development. #pragsios",
                                                    nil)];
        tweetVC.completionHandler = ^(SLComposeViewControllerResult result) {
            if (result == SLComposeViewControllerResultDone) {
                [self dismissViewControllerAnimated:YES completion:NULL];
                [self reloadTweets];
            }
        };
        [self presentViewController:tweetVC animated:YES completion:NULL];
    }
}
@end