# Contributing to the DWCJ
We're glad you're interested in contributing to DWCJ, and appreciate our contributors work in making our product even better. This guide is designed to help you get started with contributing to the project. We'll walk you through the process of setting up your development environment, submitting pull requests, and working with the DWCJ community.

## Getting your environment set up

In order to contribute to our product, you'll need to not only be able to develop with the DWCJ, but access the DWCJ's engine as well. The process to do this is covered in our documentation website's [Contributors Installation Guide](https://dwcj.org/docs/installation/contributors). We update this guide as newer and more streamlined methods of installation become available.

## Start contributing

### Creating an Issue
If you find something in the engine that needs improvement, the first thing to do is check and see if the issue already exists in [our list of current issues](https://github.com/DwcJava/engine/issues). If not, please submit a new issue using [this page](https://github.com/DwcJava/engine/issues/new/choose) to determine the best type of issue to create. This forum will help populate the issue with the correct information.

### Solving an Issue

Create a branch from the issue that has been created to address the problem you'd like to solve! 
While solving an issue, there are some standards that our code is held to before it is accepted into the engine.

 - Code must pass a check from SonarLint. Many popular code editors have extensions which can help ensure SonarLint compatibility - see [this link](https://www.sonarsource.com/products/sonarlint/?gads_campaign=SL-Class02-Brand&gads_ad_group=SonarLint&gads_keyword=sonarlint&gclid=CjwKCAjw6IiiBhAOEiwALNqnccUSLicubKGDwthpXSnu9uU5gvXcFQoNNUr_jQuKYkN73OC7WScxhRoCsvsQAvD_BwE) to find the extension for your IDE!
 - Code must pass a check from [Checkstyle](https://checkstyle.sourceforge.io/). 
 - Code must pass unit tests that have been written previously. If no tests exist for a component or piece of code you are contributing, please create these tests using [Junit5](https://junit.org/junit5/).
 - When committing your changes, please follow [these guidelines](https://www.conventionalcommits.org/en/v1.0.0-beta.3/) when authoring commits.

### Opening a Pull Request

Once you've completed your changes, please open a pull request on the branch you're working on.  Don't forget to  [link PR to issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue)  if you are solving one (if you created your branch from an issue directly, this will be done for you automatically). Enable the checkbox to  [allow maintainer edits](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/allowing-changes-to-a-pull-request-branch-created-from-a-fork)  so the branch can be updated for a merge. 

Once you submit your PR, a DWCJ team member will review your changes. We may ask questions or request additional information, or for changes to be made before a PR can be merged, either using  [suggested changes](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/incorporating-feedback-in-your-pull-request)  or pull request comments.

Once your work has been reviewed, it can be accepted, and your PR can be merged!

**Thank you** for your interest in helping improve the DWCJ!