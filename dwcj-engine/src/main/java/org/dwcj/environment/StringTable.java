package org.dwcj.environment;

import com.basis.startup.type.BBjException;
import org.dwcj.Environment;

import java.util.NoSuchElementException;

/**
 * Access the STBL String Table,
 * which is persistent, and global scope in this thread
 * Values can be changed programmatically or set in the config file, e.g.:
 * SET COMPANY=Acme
 * Then you can access the value with
 * String val = org.dwcj.environment.StringTable.getString("COMPANY")
 */
public final class StringTable {

    private StringTable(){}

    /**
     * Access a value in the String Table (STBL).
     *
     * @param key the key of the variable to access
     * @return the contents of the field
     * @throws NoSuchElementException in case the string table entry does not exist
     */
    public static String getString(String key){
        try {
            return Environment.getInstance().getBBjAPI().getStbl(key);
        } catch (BBjException e) {
            throw new NoSuchElementException("Element "+key+" does not exist!");
        }
    }

    /**
     *
     * @param key the key of the variable to access
     * @param value the contents to set in the field
     * @return the value just set
     */
    public static String setString(String key, String value){
        try {
            Environment.getInstance().getBBjAPI().setStbl(key, value);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return value;
    }

    /**
     * Clear an entry from the string table
     * @param key the key of the variable to remove
     */
    public static void clearString(String key){
        try {
            Environment.getInstance().getBBjAPI().setStbl("!CLEAR",key);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }
}
