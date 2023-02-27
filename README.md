# dwcj

Java API for BBj DWC Dynamic Web Client

## Setup Instructions

1. Download and Install BBj 22.13 or higher, on OpenJDK17

2. Installation of needed deps
To install the bbj lib dependencies locally, execute the following three lines in your
local `<bbx>/lib directory`:

```shell
mvn install:install-file -Dfile=BBjStartup.jar -DgroupId=com.basis.lib -DartifactId=BBjStartup -Dversion=1.0 -Dpackaging=jar
mvn install:install-file -Dfile=BBj.jar -DgroupId=com.basis.lib -DartifactId=BBj -Dversion=1.0 -Dpackaging=jar
mvn install:install-file -Dfile=BBjUtil.jar -DgroupId=com.basis.lib -DartifactId=BBjUtil -Dversion=1.0 -Dpackaging=jar
```

3. Clone this project

4. Add a custom classpath with

* if you're developing inside the framework: the compiled class files in the target/classes folder
* if you're developing with the framework: the dwcj.jar (it builds with Maven)
* also add (bbj-internal) to this classpath
* My entry looks like this in BBj.properties (sorry, it's Windows):
  basis.classpath.dwcj=C\:\\DWC4JavaWorkspace\\dwcj\\target\\classes\\;(bbj_internal)

4. create a webapp deployment in EM for dwcj.bbj

* make sure to set the working directory or PREFIX so that DWCjHelper.bbj is found at runtime
* you can also simply run the program with the correct classpath in GUI, it tries to self-deploy

5. navigate to

http://localhost:8888/webapp/dwcj

If you see the sample app all is well.

## Write your own class

1. Look at the sample app in the repo
2. write your own, in your own Java project. Use the classes in org.dwcj.* for UI
3. Add your stuff to the classpath you created
4. pass the classname by one of the following

http://localhost:8888/webapp/dwcj?class=your.class.name.Here

OR

in config.bbj: SET DWCJCLASSNAME=your.class.name.Here

OR

as first program arg in EM ("class=full.package.and.Classname")

## Debugging

To get comprehensive debug output, either add

SET DEBUG=1

to your <bbxdir>/cfg/config.bbx, or add

DEBUG

as a program argument to the app deployment.

## Javadoc

The Javadoc of the engine can be found here:

https://dwcjava.github.io/engine/

