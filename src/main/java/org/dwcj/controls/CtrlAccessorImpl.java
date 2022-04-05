package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.App;
import org.dwcj.bridge.ControlAccessor;

/**
 * This class implements the accessor to BBj specifics in the AbstractDwcjPanel-derived set of panel class
 * Pattern see Tulach, p.75ff
 */
final class CtrlAccessorImpl extends ControlAccessor {


    @Override
    public BBjControl getBBjControl(AbstractDwcControl ctrl) throws IllegalAccessException {

        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        String caller = stack[2].getClassName();
        if (
                caller.startsWith("org.dwcj.events") ||
                caller.startsWith("org.dwcj.controls")
        )
            return ctrl.getControl();

        throw new IllegalAccessException(caller+": You're not allowed to access this method!");
    }
}
