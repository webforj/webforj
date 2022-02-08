package org.dwcj.pages;

import org.dwcj.App;
import org.dwcj.controls.Label;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.panels.AppPanel;


public class Index extends App {

    @Override
    public void run() throws DwcAppInitializeException {

        AppPanel panel = new AppPanel();

        panel.add(new Label("Index:"));

        panel.add(new Label("finished"));


    }


}
