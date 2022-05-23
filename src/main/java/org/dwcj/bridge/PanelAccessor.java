package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.panels.AbstractDwcjPanel;

/**
 * This class implements the accessor to BBj specifics in the AbstractDwcjPanel-derived set of panel class
 * it's not for customer use, only for use in the "friend" classes
 * Pattern see Tulach, p.75ff
 */
public abstract class PanelAccessor {
    private static volatile PanelAccessor DEFAULT;

    public PanelAccessor() {
    }

    /**
     * get the accessor instance to access the protected methods in the Dwcj Panel instances
     * @return the accessor instance
     */
    public static PanelAccessor getDefault() {
        PanelAccessor a = DEFAULT;
        if (a != null) {
            return a;
        }
        try {
            Class.forName(AbstractDwcjPanel.class.getName(), true, AbstractDwcjPanel.class.getClassLoader());
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return DEFAULT;
    }

    /**
     * set the accessor instance for static access
     *
     * @param accessor the instance of the accessor implementation
     */
    public static void setDefault(PanelAccessor accessor) {
        if (DEFAULT != null) {
            throw new IllegalStateException();
        }
        DEFAULT = accessor;
    }


    /**
     *
     * @param panel the panel that contains the BBj window
     * @return the BBjWindow object behind the panel
     * @throws IllegalAccessException
     */
    public abstract BBjWindow getBBjWindow(AbstractDwcjPanel panel) throws IllegalAccessException;

}

