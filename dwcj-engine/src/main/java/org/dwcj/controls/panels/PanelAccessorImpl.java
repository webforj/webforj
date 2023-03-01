package org.dwcj.controls.panels;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.App;
import org.dwcj.bridge.PanelAccessor;

/**
 * This class implements the accessor to BBj specifics in the AbstractPanel-derived set of panel class
 * Pattern see Tulach, p.75ff
 */
final class PanelAccessorImpl extends PanelAccessor {

    @Override
    public BBjWindow getBBjWindow(AbstractPanel panel) throws IllegalAccessException {

        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        String caller = stack[2].getClassName();
        if (caller.startsWith("org.dwcj.")

        ) return panel.getBBjWindow();
        App.consoleLog(caller + ": You're not allowed to access this method!");
        throw new IllegalAccessException(caller + ": You're not allowed to access this method!");

    }
}
