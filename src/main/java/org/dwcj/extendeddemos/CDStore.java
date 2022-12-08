package org.dwcj.extendeddemos;

import org.dwcj.App;
import org.dwcj.exceptions.DwcAppInitializeException;


public class CDStore extends App {
    @Override
    public void run() throws DwcAppInitializeException {
        App.busy("initializing...");
        try {
            new CDStoreMainPanel();
        } catch (Exception e) {
            App.msgbox("Error initializing App!",0,"Error");
            throw new RuntimeException(e);
        }
    }
}
