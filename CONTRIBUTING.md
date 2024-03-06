# Contributing to the DWCJ
We're glad you're interested in contributing to DWCJ, and appreciate our contributors work in making our product even better. This guide is designed to help you get started with contributing to the project. We'll walk you through the process of setting up your development environment, submitting pull requests, and working with the DWCJ community.

> 💡 **Tip:** For those who already have their environment set up, skip to the [Start Contributing](#start-contributing) section.

## Setting Up Your Environment

In order to contribute to the DWCj's engine, the four following steps will need to be accomplished, all of which will be detailed in this section:

1. Java and Maven download and configuration
2. BBj download and installation
3. Downloading DWCJ files and building the .jar file
4. Configuring the application in the Enterprise Manager

### 1. Java and Maven Download and Configuration

In order to use the DWCJ, you must first have Java and Maven installed and properly configured. If you already
have Java and Maven downloaded, please skip to [**Step 2**](#section2). If you also have 
BBj installed on your system, please skip to [**Step 3**](#section3).

#### Java

Java OpenJDK17 can be found [by following this link](https://adoptium.net/temurin/releases/). It is recommended to allow the installation to handle setting the `JAVA_HOME` variable during installation, where applicable.

#### Maven

Maven should also be downloaded, and can be found [at this link](https://maven.apache.org/download.cgi). It is 
recommended to configure your system environment variables with Maven - a guide for installation and configuration 
for Windows users can be found [here](https://phoenixnap.com/kb/install-maven-windows).

<a name='section2'></a>

### 2. BBj Download and Installation

[This video](https://www.youtube.com/watch?v=Ovk8kznQfGs&ab_channel=BBxCluesbyBASISEurope) can help with the installation of BBj if you need assistance with setup. The installation section of the BASIS website can be found [at this link](https://basis.cloud/download-product)

> 💡 **Tip:** It is recommended to use the latest stable revision build of BBj, and to select "BBj" from the list of options, without Barista or Addon.


Once BBj has been installed, it is also necessary to install the needed dependencies from the BBj library. This is done by navigating to the `lib` directory inside your bbx folder, and
running the following commands:

```bash
mvn install:install-file "-Dfile=BBjStartup.jar" "-DgroupId=com.basis.lib" "-DartifactId=BBjStartup" "-Dversion=23.06-SNAPSHOT" "-Dpackaging=jar"
mvn install:install-file "-Dfile=BBj.jar" "-DgroupId=com.basis.lib" "-DartifactId=BBj" "-Dversion=23.06-SNAPSHOT" "-Dpackaging=jar"
mvn install:install-file "-Dfile=BBjUtil.jar" "-DgroupId=com.basis.lib" "-DartifactId=BBjUtil" "-Dversion=23.06-SNAPSHOT" "-Dpackaging=jar"
mvn install:install-file "-Dfile=BBjsp.jar" "-DgroupId=com.basis.lib" "-DartifactId=BBjsp" "-Dversion=23.06-SNAPSHOT" "-Dpackaging=jar"
```

<a name='section3'></a>

### 3. Download/Clone and Package DWCJ

The following steps will explain downloading (or cloning) the files for the 
DWCJ from GitHub. 

> 📝 **Note:** This tutorial will use Microsoft’s VS Code as the development IDE. Other IDEs may be used, and may come with other features or functionality. It can be [downloaded from this link.](https://code.visualstudio.com/Download)

#### Clone DWCJ Github Repo

Clone the code from this repository onto your development machine - this can be done using the command line and Git, or another Git tool. 

You should now have a folder named "**engine**" in the location you chose to
clone the code to. This is where you'll find the files needed to add to the
classpath later on in the tutorial, so make sure you take note of this location
for future use.

#### Compile and Package Code

Now the code from GitHub must be compiled and packaged. To start this process,
navigate to the "**engine**" folder that was just cloned from GitHub.

![Opening a folder](https://dwcj.org/assets/images/image9-371df24785c37671d1755ecfb1b03380.jpg)
<br/>

![Selecting a folder](https://dwcj.org/assets/images/image10-c7dbae9acf70c8cb581c580106d9866d.jpg)
<br/>

Once the folder has been opened, you should see the various files and folders 
open in the panel on the left of your screen. First, navigate to the pom.xml
file and open this in your editor. 

![pom.xml window](https://dwcj.org/assets/images/image30-1d4ca783d910ab827b5351057f1d0dc8.jpg)
<br/>

Now Maven will be used to package the engine and create a .jar file. Begin the process 
by running `mvn clean` in the engine directory.

![Maven clean command](https://dwcj.org/assets/images/image32-5b0ec6a173f51c72e562cd148f77b9c1.jpg)
<br/>

If this runs properly, you should see **BUILD SUCCESS**:

![Maven clean output](https://dwcj.org/assets/images/image33-dc4f275ef3dd920fc9f776d03b6009af.jpg)
<br/>

Finally, we’ll package the code into a .jar file we can use. We’ll do this by running 
`mvn package` from the command line in the engine directory. If this runs correctly, 
a final **BUILD SUCCESS** message should display:


![Maven compile command and output](https://dwcj.org/assets/images/image34-934b8bc1b084dfdf8598848287e68fed.jpg)
<br/>

After completing these steps, you should have a .jar file that you can use in the BASIS 
enterprise manager. The version of the .jar file will differ as releases are pushed to 
Github. This file should be found in “engine>target>lib”, as shown below:

![Final packaged JAR](https://dwcj.org/assets/images/image35-54cfc9be083aca7a668e9d479b6bcbe4.jpg)

<a name='packageSection'></a>

### 4. Configuration in the Enterprise Manager

The BBj Enterprise Manager will now be utilized to ensure we can run the DWCJ. 
First, we’ll add the necessary .jar file a custom classpath, and then 
create a web application from which we will launch an instance of a DWCJ app.

Open the Enterprise Manager by navigating to 
[http://localhost:8888/bbjem/emapp](http://localhost:8888/bbjem/emapp)
in a browser window. This will take you to a login screen, where the
username is ***admin*** and the password is ***admin123***.

![Restarting Barista from the Windows start menu](https://dwcj.org/assets/images/image17-58ff98b94a69ff9b6855a85ca6f758ab.jpg)
<br/>

<a name='classpathSection'></a>

#### Configuring Java Settings

After opening the Enterprise Manager, navigate to the Java Settings tab.
To do so, double click on **BBjServices** on the lefthand toolbar, and
then double click **Java Settings**.

![Java settings sidebar option](https://dwcj.org/assets/images/image18-6aca0e2f3305f3ea3a5cf23695f7b1fd.jpg)
<br/>

On reaching the Java Settings tab, add a new custom classpath. This can
be done by using the **Classpath** tab at the botton of the screen.

![Classpath tab option](https://dwcj.org/assets/images/image19-b5f16f7767b13e9e1e27ed811256428a.jpg)
<br/>

To add a new custom classpath, use the green "**+**" button near the
middle of your Enterprise Manger window. Name your classpath something like
"**DWCJ**".

Note that two classpaths need to be added: BBj's default classpath as well
as the .jar file that was packaged in **[this step](#packageSection)**.

![Adding a new classpath](https://dwcj.org/assets/images/image20-9449b3944891399173f46d3b49047893.jpg)
<br/>

The first classpath to add is BBj's default classpath. This can be done by
selecting **Existing Classpath**, which will populate a list of the various
classpaths already configured in the Enterprise Manager. Select
**bbj_default** and press **Add**.

![Selecting bbj_default](https://dwcj.org/assets/images/image21-11df18432c1b447702da6049fb2fae20.jpg)
<br/>

Select the same green "**+**" button on the right side of the acreen again.
This time select "**Jar File(s)**". Navigate to the location you cloned the
DWCJ code into. The folder will be called "**engine**". From there, select
the "**target**" folder. Inside this folder, you should see the "**webforj-foundation-XX.XX.jar**" file, with the X's replaced by numeric values. The path may look similar to "**C:\engine\target\webforj-foundation-XX.XX.jar**". Select the .jar file and then click the "**Open**"
button.

![Opening the DWCJ .jar file](https://dwcj.org/assets/images/image22-0f18a08865f7f5db648e9897473ddd3e.jpg)
<br/>

Once this is done, save your work by clikcing the save button near the top left of the Enterprise Manager window.

![Saving new custom classpath](https://dwcj.org/assets/images/image23-9af5f6a4129e69b992900c68ab792fca.jpg)
<br/>

### Create our DWCJ Application

The DWCJ application needs to be added into the Enterprise Manager. To do this, start by navigating to the Applications option on the left sidebar. Do this by double clicking the "**Web**" option, then double click "**App Deployment**", and finally "**Applications**". 

![Selecting the Application option](https://dwcj.org/assets/images/image24-a3737a6e12446bb6a0df3ad7934294c4.jpg)
<br/>

This should display a list of applications. Create a new application using the "**+**" button in the panel.

![Adding a new application](https://dwcj.org/assets/images/image25-bd31f09f451643884b336b180b829054.jpg)
<br/>

Give your application a name - "**dwcj**" will be used for this example. The
program file will be dwcj.bbj - don't worry if you're unfamiliar with the 
.bbj extension. The working directory will be the location where the files
from GitHub were cloned, inside of the "**\bbj**" folder. The pathname may look
something like "**C:\engine\bbj\\**". Select the dwcj classpath that was 
created in the **[classpath section](#classpathSection)** in the Classpath
field. Finally, check the "**DWC Web App Enabled**" box. 

![Application configuration options](https://dwcj.org/assets/images/image26-5bc2d80c51f44dd3e05e73a244febf30.jpg)
<br/>

To complete the configuration needed in the Enterprise Manager, save your 
application using the save button towards the top left of your window. 

![Save your work in the application screen](https://dwcj.org/assets/images/image27-7793f221886d5980a35a73e7da730bf7.jpg)
<br/>

After saving, click on the "**Launch in Browser**" button at the top right
of the Enterprise Manager window. 

![Launch your application button](https://dwcj.org/assets/images/image28-c3383a42f0c3c9ad90873f382ecd83d2.jpg)
<br/>

This will launch your application in a new, discrete browser window. You
may want to copy the URL from this window and paste it into a browser.
If you've successfully followed this guide, you should see the following
welcome screen, unless additional configuration steps have been taken 
in the DWCJ code.

![Launch your application button](https://dwcj.org/assets/images/image29-491b1295a4af5ecd6773c1c412526a93.jpg)
<br/>

## Start Contributing

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