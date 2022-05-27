package org.dwcj.models;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;

public class Icon {

    private File file;

    public Icon() {}
 /*
    public Icon(String file) {
        try {
            this.file = new File(new URL(file).toURI());
        } catch (MalformedURLException e) {
            this.file = new File(file);
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
    }

  */

    /*
    public static Icon loadFromFile(File file) {


    }

    public static Icon loadFromURL(String url) {

    }

     */

    public File getFile() { return file; }
}
