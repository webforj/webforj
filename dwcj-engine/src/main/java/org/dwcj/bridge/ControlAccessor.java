package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjControl;

import org.dwcj.Environment;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.panels.AbstractPanel;

/**
 * This class implements the accessor to BBj specifics in the AbstractPanel-derived set of panel class
 * it's not for customer use, only for use in the "friend" classes
 * Pattern see Tulach, p.75ff
 */
public abstract class ControlAccessor {
    private static ControlAccessor accessor;

    protected ControlAccessor() {
    }

    /**
     * factory to obtain the instance of the accessor
     *
     * @return - the accessor
     */
    public static ControlAccessor getDefault() {
        ControlAccessor a = accessor;
        if (a != null) {
            return a;
        }
        try {
            Class.forName(AbstractPanel.class.getName(), true, AbstractPanel.class.getClassLoader());
        } catch (Exception e) {
            Environment.logError(e);
        }
        return accessor;
    }

    /**
     * Set the accessor instance into the static field
     *
     * @param accessor - the accessor instance
     */
    public static void setDefault(ControlAccessor accessor) {
        if (ControlAccessor.accessor != null) {
            throw new IllegalStateException();
        }
        ControlAccessor.accessor = accessor;
    }

    /**
     * @param ctrl - get the BBjControl under the AbstractDwcControl
     * @return - the BBjControl
     * @throws IllegalAccessException
     */
    public abstract BBjControl getBBjControl(AbstractComponent ctrl) throws IllegalAccessException;

    public abstract void create(AbstractComponent ctrl,AbstractPanel panel) throws IllegalAccessException;
}

