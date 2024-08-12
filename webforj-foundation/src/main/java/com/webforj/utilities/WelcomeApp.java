package com.webforj.utilities;

import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.component.button.Button;
import com.webforj.component.button.event.ButtonClickEvent;
import com.webforj.component.text.Label;
import com.webforj.component.window.Frame;
import com.webforj.component.window.Panel;
import com.webforj.environment.ObjectTable;
import com.webforj.exceptions.WebforjLaunchException;
import com.webforj.exceptions.WebforjAppInitializeException;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * The Welcome App is an automatically created index of App classes.
 */
public class WelcomeApp extends App {

  private Frame panel;
  private String baseUrl = "";
  private String urlTail = "";
  private String urlTailWithoutParam = "";

  /**
   * The App that is used as an index page when multiple classes extend App and none is pinned in
   * the app deployment.
   *
   * @throws WebforjAppInitializeException when the initialization fails
   */
  @Override
  public void run() throws WebforjAppInitializeException {
    panel = new Frame();
    panel.setStyle("display", "inline-grid");
    panel.setStyle("gap", "20px");
    panel.setStyle("padding", "20px");
    panel.setStyle("max-width", "600px");
    panel.setStyle("margin-left", "calc( max(0px , 100vw / 2 - 300px ))");
    panel.setStyle("overflow", "auto", Frame.Area.CENTER);

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

      int questionMarkPosition = urlTail.indexOf("?");
      if (questionMarkPosition != -1) {
        urlTailWithoutParam = urlTail.substring(0, questionMarkPosition);
      } else {
        urlTailWithoutParam = urlTail;
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
      if (!this.urlTailWithoutParam.isBlank()) {
        for (String entry : applist) {
          try {
            if (Class.forName(entry).getSimpleName().equals(this.urlTailWithoutParam)) {
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
          "<html><p>Write your first App that extends <a href='https://dwcjava.github.io/engine/org/dwcj/App.html' target='_new'>com.webforj.App</a>. Then "
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
    panel.destroy();
    try {
      App app = (App) Class.forName(className).getDeclaredConstructor().newInstance();
      app.initialize();
    } catch (ClassNotFoundException | NoSuchMethodException | InstantiationException
        | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to launch class: " + className, e);
    } catch (InvocationTargetException e) {
      throw new WebforjRuntimeException("Constructor threw an exception for class: " + className,
          e);
    } catch (WebforjAppInitializeException e) {
      throw new WebforjLaunchException("Failed to launch class: " + className, e);
    }
  }
}
