package org.dwcj.util;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class AppFinder {

    private final Class appBaseClass;
    private final TreeSet<String> appImplmentations = new TreeSet<>();

    final private List<String> cpEntriesToCheck;

    public AppFinder(List<String>cpEntriesToCheck) throws ClassNotFoundException {
        this.cpEntriesToCheck = cpEntriesToCheck;
        appBaseClass = Class.forName("org.dwcj.App");
    }


    private void checkClass(String className) {


        Class tmpClass = null;
        try {
            tmpClass = Class.forName(className,false, this.getClass().getClassLoader());
        } catch (ClassNotFoundException e) {
            //ignore this exception; this class is obviously not for us
            return;
        }

        if (tmpClass != null && this.appBaseClass.isAssignableFrom(tmpClass) && !className.equals("org.dwcj.App") && !className.equals("org.dwcj.util.WelcomeApp")) {
            this.appImplmentations.add(className);
        }
    }

    public Set<String> getAppImplmentations() {

        if (this.cpEntriesToCheck != null) {
            Iterator<String> it = cpEntriesToCheck.iterator();
            while (it.hasNext()) {
                process(it.next());
            }
        }
        return this.appImplmentations;
    }

    private void process(String filepath){
        processFile(filepath,"");
    }

    private void processFile(String base, String curFile) {
        String nextfile = base + File.separatorChar + curFile;
        File currentDirectory = new File(nextfile);

        // it's a JAR file
        if (currentDirectory.getName().endsWith(".jar")) {
            try {
                processJar(new ZipFile(currentDirectory));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            return;
        }
        else {

            Set myDirectories = new HashSet();

            File[] myFiles = currentDirectory.listFiles();

            if (myFiles == null || myFiles.length == 0) {
                return;
            }

            for (int i = 0; i < myFiles.length; i++) {
                File myFile = myFiles[i];
                if (myFile.isDirectory()) {
                    myDirectories.add(myFiles[i]);
                } else {
                    if (myFile.getName().endsWith(".class")) {
                        String className = getClassName(curFile +((curFile == "") ? "" : File.separator) + myFile.getName());
                        checkClass(className);
                    }
                }
            }

            for (Iterator i = myDirectories.iterator(); i.hasNext(); ) {
                processFile(base, curFile + ((curFile =="")?"":File.separator) +
                        ((File)i.next()).getName());
            }
        }
    }

    private String getClassName(String fileName) {
        String className =  fileName.replace(File.separatorChar,'.');
        className =  className.replace('/','.');
        return className.substring(0, fileName.length() - 6);
    }

    private void processJar(ZipFile file) {
        Enumeration files = file.entries();

        while (files.hasMoreElements()) {
            Object tfile = files.nextElement();
            ZipEntry child = (ZipEntry) tfile;
            if (child.getName().endsWith(".class")) {
                checkClass(getClassName(child.getName()));
            }
        }
    }


    public static void main(String[] args) throws ClassNotFoundException {
        List<String> cpentruied;
        cpentruied = new ArrayList<>();
        cpentruied.add("/Users/beff/testfish_lib/common/mysql-connector-java-8.0.19.jar");
        AppFinder af = new AppFinder(cpentruied);
        System.out.println(af.getAppImplmentations());
    }
}



