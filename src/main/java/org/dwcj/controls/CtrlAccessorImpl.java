package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.App;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

/**
 * This class implements the accessor to BBj specifics in the AbstractDwcjPanel-derived set of panel class
 * Pattern see Tulach, p.75ff
 */
final class CtrlAccessorImpl extends ControlAccessor {


    @Override
    public BBjControl getBBjControl(AbstractDwcControl ctrl) throws IllegalAccessException {

        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        String caller = stack[2].getClassName();
        if (caller.startsWith("org.dwcj.")) return ctrl.getControl();

        App.consoleLog(caller + ": You're not allowed to access this method!");
        throw new IllegalAccessException(caller + ": You're not allowed to access this method!");
    }

    @Override
    public void create(AbstractDwcControl ctrl, AbstractDwcjPanel panel) throws IllegalAccessException {

        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        String caller = stack[2].getClassName();

        if (caller.startsWith("org.dwcj.")) {
            try {
                Method m = null;
                m = ctrl.getClass().getDeclaredMethod("create", Class.forName("org.dwcj.panels.AbstractDwcjPanel"));
                m.setAccessible(true);
                m.invoke(ctrl,panel);
            } catch (Exception e) {
                App.consoleLog("Cannot invoke create method - Error :"+e.getMessage());
                e.printStackTrace();
            }
            return;
        }

        App.consoleLog(caller + ": You're not allowed to access this method!");
        throw new IllegalAccessException(caller + ": You're not allowed to access this method!");
    }
}
