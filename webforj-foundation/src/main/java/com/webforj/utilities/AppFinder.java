package com.webforj.utilities;

import java.io.File;
import java.io.IOException;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * AppFinder - scans Classpath to find App implementations.
 */
@SuppressWarnings({"java:S3740", "java:S3776"}) // allow raw types
public class AppFinder {

  private final Class appBaseClass;
  private final TreeSet<String> appImplmentations = new TreeSet<>();

  private final List<String> cpEntriesToCheck;

  public AppFinder(List<String> cpEntriesToCheck) throws ClassNotFoundException {
    this.cpEntriesToCheck = cpEntriesToCheck;
    appBaseClass = Class.forName("com.webforj.App");
  }


  private void checkClass(String className) {


    Class tmpClass = null;

    try {
      tmpClass = Class.forName(className, false, this.getClass().getClassLoader());
    } catch (NoClassDefFoundError | ClassNotFoundException e) {
      return;
    }
    // ignore class in case of exceptions, this class is apparently not for us!

    if (tmpClass != null && this.appBaseClass.isAssignableFrom(tmpClass)
        && !className.equals("com.webforj.App")
        && !className.equals("com.webforj.utilities.WelcomeApp")) {
      this.appImplmentations.add(className);
    }
  }

  /**
   * Find app implementations.
   *
   * @return the list of classes extending App.
   */
  public SortedSet<String> getAppImplmentations() {

    if (this.cpEntriesToCheck != null) {
      for (String s : cpEntriesToCheck) {
        process(s);
      }
    }
    return this.appImplmentations;
  }

  private void process(String filepath) {
    processFile(filepath, "");
  }


  private void processFile(String base, String curFile) {
    String nextfile = base + File.separatorChar + curFile;
    File currentDirectory = new File(nextfile);

    // it's a JAR file
    if (currentDirectory.getName().endsWith(".jar")) {
      try {
        if (currentDirectory.exists()) {
          processJar(new ZipFile(currentDirectory));
        }
      } catch (IOException e) {
        throw new RuntimeException(e); // NOSONAR
      }
    } else {

      Set<File> myDirectories = new HashSet<>();

      File[] myFiles = currentDirectory.listFiles();

      if (myFiles == null || myFiles.length == 0) {
        return;
      }

      for (File myFile : myFiles) {
        if (myFile.isDirectory()) {
          myDirectories.add(myFile);
        } else {
          if (myFile.getName().endsWith(".class")) {
            String className = getClassName(
                curFile + ((curFile.isBlank()) ? "" : File.separator) + myFile.getName());
            checkClass(className);
          }
        }
      }

      for (Object myDirectory : myDirectories) {
        processFile(base,
            curFile + ((curFile.isBlank()) ? "" : File.separator) + ((File) myDirectory).getName());
      }
    }
  }

  private String getClassName(String fileName) {
    String className = fileName.replace(File.separatorChar, '.');
    className = className.replace('/', '.');
    return className.substring(0, fileName.length() - 6);
  }

  private void processJar(ZipFile file) {
    Enumeration files = file.entries(); // NOSONAR

    while (files.hasMoreElements()) {
      Object tfile = files.nextElement();
      ZipEntry child = (ZipEntry) tfile;
      if (child.getName().endsWith(".class")) {
        checkClass(getClassName(child.getName()));
      }
    }
  }
}


