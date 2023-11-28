package org.dwcj.utilities;

import com.basis.startup.type.BBjException;

import java.util.*;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.annotation.InlineStyleSheet;
import org.dwcj.component.button.Button;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.text.Label;
import org.dwcj.component.window.Frame;
import org.dwcj.component.window.Panel;
import org.dwcj.environment.ObjectTable;
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
  private String baseUrl = "";
  private String urlTail = "";

  /**
   * The App that is used as an index page when multiple classes extend App and none is pinned in
   * the app deployment.
   *
   * @throws DwcjAppInitializeException when the initialization fails
   */
  @Override
  public void run() throws DwcjAppInitializeException {
    panel = new Frame();
    panel.setStyle("display", "inline-grid");
    panel.setStyle("gap", "20px");
    panel.setStyle("padding", "20px");
    panel.setStyle("max-width", "600px");
    panel.setStyle("margin-left", "calc( max(0px , 100vw / 2 - 300px ))");



    String url = App.getUrl();
    String name = App.getApplicationName();
    this.baseUrl = url.substring(0, url.indexOf(name) + name.length());
    if (url.length() > this.baseUrl.length()) {
      this.urlTail = url.substring(this.baseUrl.length());
      if (this.urlTail.startsWith("/")) {
        this.urlTail = this.urlTail.substring(1);
      }
      while (urlTail.contains("/")) {
        urlTail = urlTail.substring(0, urlTail.indexOf("/"));
      }

    }

    buildAppList();

  }

  /**
   * create the list of apps by scanning the classpath.
   */
  private void buildAppList() {

    Label wait = new Label("please wait...");
    panel.add(wait);


    ArrayList<String> cplist = null;
    AppFinder af;
    SortedSet<String> applist = null;

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
      if (applist.size() == 1) {
        launchClass(applist.first());
        return;
      }

      // determine if route contains some application that can be launched
      if (!this.urlTail.isBlank()) {
        for (String entry : applist) {
          try {
            if (Class.forName(entry).getSimpleName().equals(this.urlTail)) {
              // store base url in object table so that interested partied know of the automatic
              // forwarding
              ObjectTable.put("dwcj_base_url", this.baseUrl + "/" + this.urlTail);
              launchClass(entry);
              return;
            }
          } catch (ClassNotFoundException e) {
            // ignore
          }
        }
      }

      showAppList(applist);
    }

  }

  /**
   * Shows the list of classess that extend App.
   *
   * @param applist the list of apps found in the classpath
   */
  private void showAppList(Set<String> applist) {

    Label headline = new Label("<html><h1>Welcome to DWCJ");
    panel.add(headline);

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

  /**
   * The event handler when the user clicks the button to launch one of the Apps.
   *
   * @param buttonPushEvent the event payload
   */
  private void onLaunchClick(ButtonClickEvent buttonPushEvent) {
    String className = buttonPushEvent.getComponent().getUserData("classname").toString();
    String tail = "";
    try {
      tail = Class.forName(className).getSimpleName();
      App.getPage()
          .executeJs("window.history.replaceState({},'title','" + baseUrl + "/" + tail + "');");
      ObjectTable.put("dwcj_base_url", this.baseUrl + "/" + tail);
      launchClass(className);
    } catch (ClassNotFoundException e) {
      // ignore
    }

  }


  /**
   * Launch the App class directly by instantiating it and calling initialize.
   *
   * @param className the fully qualified name of the app class.
   */
  private void launchClass(String className) {
    panel.setVisible(false);
    try {
      App app = (App) Class.forName(className).getDeclaredConstructor().newInstance();
      app.initialize();

    } catch (Exception e) {
      Environment.logError(e);
      msgbox("cannot launch app!", 0, "Error");
    } finally {
      panel.setVisible(false);
    }
  }

}
