package org.dwcj.bridge;

import org.dwcj.Environment;
import org.dwcj.component.window.AbstractWindow;

import com.basis.bbj.proxies.sysgui.BBjWindow;

/**
 * This class implements the accessor to BBj specifics in the AbstractPanel-derived set of panel class
 * it's not for customer use, only for use in the "friend" classes
 * Pattern see Tulach, p.75ff
 */
public abstract class PanelAccessor {
    private static PanelAccessor accessor;

    protected PanelAccessor() {
    }

    /**
     * get the accessor instance to access the protected methods in the Dwcj Panel instances
     * @return the accessor instance
     */
    public static PanelAccessor getDefault() {
        PanelAccessor a = accessor;
        if (a != null) {
            return a;
        }
        try {
            Class.forName(AbstractWindow.class.getName(), true, AbstractWindow.class.getClassLoader());
        } catch (Exception e) {
            Environment.logError(e);
        }
        return accessor;
    }

    /**
     * set the accessor instance for static access
     *
     * @param accessor the instance of the accessor implementation
     */
    public static void setDefault(PanelAccessor accessor) {
        if (PanelAccessor.accessor != null) {
            throw new IllegalStateException();
        }
        PanelAccessor.accessor = accessor;
    }


    /**
     *
     * @param panel the panel that contains the BBj window
     * @return the BBjWindow object behind the panel
     * @throws IllegalAccessException
     */
    public abstract BBjWindow getBBjWindow(AbstractWindow panel) throws IllegalAccessException;

}

