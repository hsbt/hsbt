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
- (void) handleTwitterData: (NSData*) data
              urlResponse: (NSHTTPURLResponse*) urlResponse
                    error: (NSError*) error;
@property (nonatomic, strong) IBOutlet UITextView *twitterTextView;
@end

@implementation MametterViewController
@synthesize twitterTextView;

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
}

- (void)viewDidUnload
{
    [self setTwitterTextView:nil];
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
        [tweetVC setInitialText: NSLocalizedString (@"I just finished the first project in iOS SDK Development. #pragsios", nil)];
        tweetVC.completionHandler = ^(SLComposeViewControllerResult result) {
            if (result == SLComposeViewControllerResultDone) {
                [self dismissViewControllerAnimated:YES completion:NULL]; [self reloadTweets];
            }
        };
        [self presentViewController:tweetVC animated:YES completion:NULL];
    }
}

-(void) reloadTweets {
    NSURL *twitterAPIURL = [NSURL URLWithString:
                            @"http://api.twitter.com/1/statuses/user_timeline.json"]; NSDictionary *twitterParams = @ {
        @"screen_name" : @"pragprog", };
    SLRequest *request = [SLRequest requestForServiceType:SLServiceTypeTwitter
                                            requestMethod:SLRequestMethodGET
                                                      URL:twitterAPIURL
                                               parameters:twitterParams];
    [request performRequestWithHandler:^(NSData *responseData,
                                         NSHTTPURLResponse *urlResponse,
                                         NSError *error) {
        [self handleTwitterData:responseData
                    urlResponse:urlResponse
                          error:error];
    }];
}

-(IBAction) handleShowMyTweetsTapped: (id) sender {
    [self reloadTweets];
}

-(void) handleTwitterData: (NSData*) data
              urlResponse: (NSHTTPURLResponse*) urlResponse
                    error: (NSError*) error {
    NSError *jsonError = nil;
    NSJSONSerialization *jsonResponse = [NSJSONSerialization JSONObjectWithData:data
                                                                        options:0
                                                                          error:&jsonError];
    NSLog (@"jsonResponse: %@", jsonResponse);
    if (!jsonError && [jsonResponse isKindOfClass:[NSArray class]]) {
      dispatch_async(dispatch_get_main_queue(), ^{
          NSArray *tweets = (NSArray*) jsonResponse;
          NSSortDescriptor *sortByText = [NSSortDescriptor sortDescriptorWithKey:@"text" ascending:YES];
          NSArray *sortDescriptors = @[sortByText];
          tweets = [tweets sortedArrayUsingDescriptors:sortDescriptors];
          for (NSDictionary *tweetDict in tweets) {
            NSString *tweetText = [NSString stringWithFormat:@"%@: %@ (%@)",
                                   [tweetDict valueForKeyPath:@"user.name"],
                                   [tweetDict valueForKey:@"text"],
                                   [tweetDict valueForKey:@"created_at"]];
            self.twitterTextView.text = [NSString stringWithFormat:@"%@%@\n\n", self.twitterTextView.text, tweetText];
        }
      });
    }
}
@end