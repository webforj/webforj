package org.dwcj.utilities;

import com.basis.startup.type.BBjException;
import java.util.ArrayList;
import java.util.Set;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.annotation.InlineStyleSheet;
import org.dwcj.component.button.Button;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.text.Label;
import org.dwcj.component.window.Frame;
import org.dwcj.component.window.Panel;
import org.dwcj.exceptions.DwcjAppInitializeException;

/**
 * The Welcome App is an automatically created index of App classes.
 */
@InlineStyleSheet(/* css */"""
    .BBjTopLevelWindow-center {
      overflow: scroll
    }
     """)
public class WelcomeApp extends App {

  private Frame panel;

  @Override
  public void run() throws DwcjAppInitializeException {
    panel = new Frame();
    panel.setStyle("display", "inline-grid");
    panel.setStyle("gap", "20px");
    panel.setStyle("padding", "20px");
    panel.setStyle("max-width", "600px");
    panel.setStyle("margin-left", "calc( max(0px , 100vw / 2 - 300px ))");

    Label headline = new Label("<html><h1>Welcome to DWCJ");
    panel.add(headline);

    buildAppList();

  }

  private void buildAppList() {

    Label wait = new Label("please wait...");
    panel.add(wait);


    ArrayList<String> cplist = null;
    AppFinder af;
    Set<String> applist = null;

    try {
      cplist =
          (ArrayList<String>) Environment.getCurrent().getBBjAPI().getObjectTable().get("dwcjcp");
    } catch (BBjException e) { //
    }
    if (cplist != null) {
      try {
        af = new AppFinder(cplist);
        applist = af.getAppImplmentations();
      } catch (ClassNotFoundException e) { //
      }
    }

    if (applist == null) {
      wait.setText("Could not determine deployed apps.");
    } else {
      wait.setVisible(false);
      showAppList(applist);
    }

  }

  private void showAppList(Set<String> applist) {

    if (applist.isEmpty()) {
      panel.add(
          new Label("<html><h2>Oops. Looks like there are no DWCJ Apps in your Classpath</h2>"));
      panel.add(new Label(
          "<html><p>Write your first App that extends <a href='https://dwcjava.github.io/engine/org/dwcj/App.html' target='_new'>org.dwcj.App</a>. Then "
              + "build it and put it in the Classpath of the DWC app deployment.</p>"
              + "<p>There is a <a href='https://github.com/DwcJava/engine' target='_new'>Readme in the project's home on GitHub</a> that explains the first steps.</p>"
              + "<p>The <a href='https://github.com/BBj-Plugins/DWCJ' target='_new'>DWCJ BBjPlugin</a> allows you to configure the classpath and to set the start class for your app."
              + "</p>"));
    } else {
      panel.add(new Label("<html><h2>The following Apps are available in this Classpath:</h2>"));

      Panel tbl = new Panel();
      panel.add(tbl);
      tbl.setStyle("display", "inline-grid");
      tbl.setStyle("grid-template-columns", "5fr 1fr");
      tbl.setStyle("gap", "20px");

      for (String app : applist) {
        Label classNameLabel = new Label(app);
        tbl.add(classNameLabel);

        Button launch = new Button("Launch");
        launch.setUserData("classname", app);
        tbl.add(launch);
        launch.onClick(this::onLaunchClick);

      }
      panel.add(new Label(
          "<html><p>In production systems, you may want to set the classname as an argument "
              + "to the deployed app to avoid the loading time by scanning the classpath!</p>"));
    }
    panel.add(new Label("<html><p>If you do not see an App that you have created recently, "
        + "please remember that you may have to compile / build it first in your IDE.</p>"));
  }

  private void onLaunchClick(ButtonClickEvent buttonPushEvent) {
    String className = buttonPushEvent.getComponent().getUserData("classname").toString();

    panel.setVisible(false);

    try {

      Environment.getCurrent().getSysGui()
          .executeScript("window.location=window.location+'?class=" + className + "'");
      Class.forName(className).getDeclaredConstructor().newInstance();
    } catch (Exception e) {
      Environment.logError(e);
      msgbox("cannot launch app!", 0, "Error");
    } finally {
      panel.setVisible(false);
    }
  }
}
