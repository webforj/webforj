package org.dwcj;

import com.basis.startup.type.BBjException;
import org.dwcj.exceptions.DwcAppInitializeException;

public abstract class App {

    public App() {
        preRun();
        try {
            run();
        } catch (DwcAppInitializeException e) {
            e.printStackTrace();
        }
    }

    public static void consoleLog(String output) {
        try {
            Environment.getInstance().getSysGui().executeScript("console.log('" + output + "')");
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public static int msgbox(String alert) {
        return Environment.getInstance().getDwcjHelper().msgbox(alert, 0, "");
    }

    public static int msgbox(String alert, int options) {
        return Environment.getInstance().getDwcjHelper().msgbox(alert, options, "");
    }

    public static int msgbox(String alert, int options, String title) {
        return Environment.getInstance().getDwcjHelper().msgbox(alert, options, title);
    }

    private void preRun() {
        Environment.getInstance().getBBjAPI().setCustomEventCallback("doTerminate", "terminate");
    }

    public void terminate() {
        Environment.getInstance().getBBjAPI().postPriorityCustomEvent("doTerminate", null);
        Environment.cleanup();
    }

    abstract public void run() throws DwcAppInitializeException;

}
