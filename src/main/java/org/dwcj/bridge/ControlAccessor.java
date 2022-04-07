package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.panels.AbstractDwcjPanel;

/**
 * This class implements the accessor to BBj specifics in the AbstractDwcjPanel-derived set of panel class
 * it's not for customer use, only for use in the "friend" classes
 * Pattern see Tulach, p.75ff
 */
public abstract class ControlAccessor {
    private static volatile ControlAccessor DEFAULT;

    public ControlAccessor() {
    }

    /**
     * factory to obtain the instance of the accessor
     *
     * @return - the accessor
     */
    public static ControlAccessor getDefault() {
        ControlAccessor a = DEFAULT;
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
     * Set the accessor instance into the static field
     *
     * @param accessor - the accessor instance
     */
    public static void setDefault(ControlAccessor accessor) {
        if (DEFAULT != null) {
            throw new IllegalStateException();
        }
        DEFAULT = accessor;
    }

    /**
     * @param ctrl - get the BBjControl under the AbstractDwcControl
     * @return - the BBjControl
     * @throws IllegalAccessException
     */
    public abstract BBjControl getBBjControl(AbstractDwcControl ctrl) throws IllegalAccessException;

    public abstract void create(AbstractDwcControl ctrl,AbstractDwcjPanel panel) throws IllegalAccessException;
}

