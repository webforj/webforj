package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.App;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.panels.AbstractDwcjPanel;

import java.lang.reflect.Method;

/**
 * This class implements the accessor to BBj specifics in the AbstractDwcjPanel-derived set of panel class
 * Pattern see Tulach, p.75ff
 */
final class CtrlAccessorImpl extends ControlAccessor {


    public static final String YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD = ": You're not allowed to access this method!";

    @Override
    public BBjControl getBBjControl(AbstractDwcControl ctrl) throws IllegalAccessException {

        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        String caller = stack[2].getClassName();
        if (caller.startsWith("org.dwcj.")) return ctrl.getControl();

        App.consoleLog(caller + YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD);
        throw new IllegalAccessException(caller + YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD);
    }

    @Override
    @SuppressWarnings("java:S3011") //allow increasing acessibility
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

        App.consoleLog(caller + YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD);
        throw new IllegalAccessException(caller + YOU_RE_NOT_ALLOWED_TO_ACCESS_THIS_METHOD);
    }
}
