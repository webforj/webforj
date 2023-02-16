package org.dwcj.demos;

import org.dwcj.App;
import org.dwcj.annotations.InlineStyleSheet;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.templates.AppTemplateTest;
import org.dwcj.controls.panels.AppPanel;

// @InlineStyleSheet(value = "css/styles.css", local = true)


public class AppLayoutDemo extends App{

    @Override
    public void run() throws DwcAppInitializeException {
        AppPanel panel = new AppPanel();
        AppTemplateTest myApp = new AppTemplateTest();
        panel.add(myApp);
    }
}
