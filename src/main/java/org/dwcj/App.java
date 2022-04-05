package org.dwcj;

import com.basis.startup.type.BBjException;
import org.dwcj.exceptions.DwcAppInitializeException;

/**
 * This is the central class representing an app. In order to implement an app, extend this class and
 * override the run() method.
 *
 */
public abstract class App {

    public App() {
        preRun();
        try {
            run();
        } catch (DwcAppInitializeException e) {
            e.printStackTrace();
        }
    }

    /**
     * Log a String to the browser console (console.out)
     * @param output The message to log
     */
    public static void consoleLog(String output) {
        try {

            Environment.getInstance().getSysGui().executeScript("console.log(\"" + output + "\")");
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    /**
     * Shows a message box
     * @param alert The message to show
     * @return
     */
    public static int msgbox(String alert) {
        return Environment.getInstance().getDwcjHelper().msgbox(alert, 0, "");
    }

    /**
     *
     * @param alert The message to show
     * @param options
     * @return
     */
    public static int msgbox(String alert, int options) {
        return Environment.getInstance().getDwcjHelper().msgbox(alert, options, "");
    }

    /**
     *
     * @param alert The message to show
     * @param options
     * @param title
     * @return
     */
    public static int msgbox(String alert, int options, String title) {
        return Environment.getInstance().getDwcjHelper().msgbox(alert, options, title);
    }

    private void preRun() {
        Environment.getInstance().getBBjAPI().setCustomEventCallback("doTerminate", "terminate");
    }

    /**
     * Call this method to terminate your App.
     */
    public void terminate() {
        Environment.getInstance().getBBjAPI().postPriorityCustomEvent("doTerminate", null);
        Environment.cleanup();
    }

    /**
     * Override this method to implement your app behavior
     * @throws DwcAppInitializeException
     */
    abstract public void run() throws DwcAppInitializeException;

}
