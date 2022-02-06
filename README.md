# dwcj
Java API for BBj DWC Dynamic Web Client

## Setup Instructions

1. Download and Install BBj 21.14 or higher, on OpenJDK11
2. Clone this project
3. Add a custom classpath with
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

as first program arg in EM (just the classname)
