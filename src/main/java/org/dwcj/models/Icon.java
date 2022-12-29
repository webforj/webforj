package org.dwcj.models;

import org.apache.commons.io.IOUtils;
import org.dwcj.Environment;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;

public class Icon {

    private File file;

 /*
    public Icon(String file) {
        try {
            this.file = new File(new URL(file).toURI());
        } catch (MalformedURLException e) {
            this.file = new File(file);
        } catch (URISyntaxException e) {
            Environment.logError(e);
        }
    }

  */


    public Icon loadFromFile(File file) {
        this.file = file;
        return this;
    }

    public Icon loadFromURL(String url) {
        try {
            this.file = new File(new URL(url).toURI());
        } catch (URISyntaxException | MalformedURLException e) {
            Environment.logError(e);
        }
        return this;
    }

    public Icon loadFromResources(String resource) {
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        InputStream inputStream = classLoader.getResourceAsStream(resource);
        try (OutputStream outputStream = new FileOutputStream(this.file)) {
            IOUtils.copy(inputStream, outputStream);
        } catch (IOException e) {
            Environment.logError(e);
        }
        return this;
    }

    public File getFile() { return file; }
}
