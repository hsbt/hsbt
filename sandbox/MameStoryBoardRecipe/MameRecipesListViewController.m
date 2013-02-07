//
//  MameRecipesListViewController.m
//  MameRecipe
//
//  Created by SHIBATA Hiroshi on 2012/12/24.
//  Copyright (c) 2012年 SHIBATA Hiroshi. All rights reserved.
//

#import "MameRecipesListViewController.h"
#import "MameRecipeEditorViewController.h"
#import "MameRecipeViewController.h"

@interface MameRecipesListViewController ()

@end

@implementation MameRecipesListViewController

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    if([@"presentRecipeDetail" isEqualToString:segue.identifier]) {
        NSIndexPath *index = [self.tableView indexPathForCell:sender];
        MameRecipe *recipe = [self.dataSource recipeAtIndex:index.row];
        [[segue destinationViewController] setRecipe:recipe];
    }
    if([@"addNewRecipe" isEqualToString:segue.identifier]) {
        MameRecipe *recipe = [self.dataSource createNewRecipe];
        //UIViewController *topVC = [[segue destinationViewController]
        //                           topViewController];
        //MameRecipeEditorViewController *editor = (MameRecipeEditorViewController *)topVC;
        MameRecipeEditorViewController *editor = [segue destinationViewController];
        editor.recipe = recipe;
    }
}

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return [self.dataSource recipeCount];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:CellIdentifier];
    }
    
    // Configure the cell...
  MameRecipe *recipe = [self.dataSource recipeAtIndex:indexPath.row];
  cell.textLabel.text = [recipe title];
  cell.imageView.image = [recipe image];
  cell.detailTextLabel.text = [NSString stringWithFormat:@"%@ %@",
                               [recipe preparationTime],
                               NSLocalizedString(@"minutes", nil)];
  return cell;
}

// Override to support editing the table view.
- (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath
{
  if (editingStyle == UITableViewCellEditingStyleDelete) {
    [self.dataSource deleteRecipeAtIndex:indexPath.row];
    [tableView deleteRowsAtIndexPaths:[NSArray arrayWithObject:indexPath] withRowAnimation:UITableViewRowAnimationFade];
  } else if (editingStyle == UITableViewCellEditingStyleInsert) {
  }
}

@end
