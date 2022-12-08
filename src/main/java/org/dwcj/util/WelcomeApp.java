package org.dwcj.util;

import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.controls.button.Button;
import org.dwcj.controls.button.events.ButtonPushEvent;
import org.dwcj.controls.label.Label;
import org.dwcj.controls.panels.AppPanel;
import org.dwcj.controls.panels.Div;
import org.dwcj.exceptions.DwcAppInitializeException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

public class WelcomeApp extends App {

    private AppPanel panel;

    @Override
    public void run() throws DwcAppInitializeException {
        panel = new AppPanel();
        panel.setStyle("display","inline-grid");
        panel.setStyle("gap","20px");
        panel.setStyle("padding","20px");
        panel.setStyle("max-width","600px");
        panel.setStyle("margin-left","calc( max(0px , 100vw / 2 - 300px ))");

        Label headline = new Label("<html><h1>Welcome to DWCJ");
        panel.add(headline);

        buildAppList();

    }

    private void buildAppList() {

        Label wait = new Label("please wait...");
        panel.add(wait);


        ArrayList<String> cplist = null;
        AppFinder af = null;
        Set<String> applist = null;

        try {
            cplist = (ArrayList<String>) Environment.getInstance().getBBjAPI().getObjectTable().get("dwcjcp");
        } catch (BBjException e) { //
        }
        if (cplist != null){
            try {
                af = new AppFinder(cplist);
                applist = af.getAppImplmentations();
            } catch (ClassNotFoundException e) { //
            }
        }

        if (applist == null){
            wait.setText("Could not determine deployed apps.");
        } else {
            wait.setVisible(false);
            showAppList(applist);
        }

    }

    private void showAppList(Set<String> applist) {

        if (applist.isEmpty()) {
            panel.add(new Label("<html><h2>Oops. Looks like there are no DWCJ Apps in your Classpath</h2>"));
            panel.add(new Label("<html><p>Write your first App that extends <a href='https://dwcjava.github.io/engine/org/dwcj/App.html' target='_new'>org.dwcj.App</a>. Then " +
                    "build it and put it in the Classpath of the DWC app deployment.</p>" +
                    "<p>There is a <a href='https://github.com/DwcJava/engine' target='_new'>Readme in the project's home on GitHub</a> that explains the first steps.</p>" +
                    "<p>The <a href='https://github.com/BBj-Plugins/DWCJ' target='_new'>DWCJ BBjPlugin</a> allows you to configure the classpath and to set the start class for your app." +
                    "</p>"));
        } else {
            panel.add(new Label("<html><h2>The following Apps are available in this Classpath:</h2>"));

            Div tbl = new Div();
            panel.add(tbl);
            tbl.setStyle("display", "inline-grid");
            tbl.setStyle("grid-template-columns", "5fr 1fr");
            tbl.setStyle("gap", "20px");

            Iterator<String> it = applist.iterator();
            while (it.hasNext()) {
                String app = it.next();
                Label classNameLabel = new Label(app);
                tbl.add(classNameLabel);

                Button launch = new Button("Launch");
                launch.setUserData("classname", app);
                tbl.add(launch);
                launch.onClick(this::onLaunchClick);

            }
            panel.add(new Label("<html><p>In production systems, you may want to set the classname as an argument " +
                    "to the deployed app to avoid the loading time by scanning the classpath!</p>"));
        }
        panel.add(new Label("<html><p>If you do not see an App that you have created recently, please remember that you may have to compile / build it first in your IDE.</p>"));
    }

    private void onLaunchClick(ButtonPushEvent buttonPushEvent) {
        String className = buttonPushEvent.getControl().getUserData("classname").toString();

        panel.setVisible(false);

        try {
            Class.forName(className).newInstance();
        } catch (Exception e) {
            e.printStackTrace();
            msgbox("cannot launch app!",0,"Error");
        } finally {
            panel.setVisible(false);
        }
    }
}
