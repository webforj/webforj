package org.dwcj.util.files;

import org.dwcj.Environment;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

/**
 * In DWCJ we oftentimes need file contents as String
 * This class helps keeping code free from too many redundancies
 */
public class FileLoader {

    private FileLoader(){}

    /**
     *
     * @param fileName the path to the file, which will be resolved using the Classloader that is also running the app
     * @return the contents of the file as String
     */
    public static String getResourceFileAsString(String fileName)  {
        ClassLoader classLoader = Environment.getInstance().getClass().getClassLoader();
        try (InputStream is = classLoader.getResourceAsStream(fileName)) {
            if (is == null) return "";
            try (InputStreamReader isr = new InputStreamReader(is);
                 BufferedReader reader = new BufferedReader(isr)) {
                return reader.lines().collect(Collectors.joining(System.lineSeparator()));
            }
        } catch (IOException e) {
            Environment.logError(e);
            return "";
        }
    }
}
