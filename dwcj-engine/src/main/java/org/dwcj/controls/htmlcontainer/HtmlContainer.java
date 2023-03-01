package org.dwcj.controls.htmlcontainer;

import com.basis.bbj.proxies.sysgui.BBjHtmlView;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.htmlcontainer.events.HtmlContainerJavascriptEvent;
import org.dwcj.controls.htmlcontainer.events.HtmlContainerOnScriptFailedEvent;
import org.dwcj.controls.htmlcontainer.events.HtmlContainerOnScriptLoadedEvent;
import org.dwcj.controls.htmlcontainer.events.HtmlContainerPageLoadedEvent;
import org.dwcj.controls.htmlcontainer.sinks.HtmlContainerNativeJavascriptEventSink;
import org.dwcj.controls.htmlcontainer.sinks.HtmlContainerOnScriptFailedEventSink;
import org.dwcj.controls.htmlcontainer.sinks.HtmlContainerOnScriptLoadedEventSink;
import org.dwcj.controls.htmlcontainer.sinks.HtmlContainerPageLoadedEventSink;
import org.dwcj.controls.panels.AbstractPanel;
import org.dwcj.interfaces.Focusable;
import org.dwcj.interfaces.TabTraversable;
import org.dwcj.util.BBjFunctionalityHelper;

import java.awt.*;
import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * A HtmlContainer control
 */
public final class HtmlContainer extends AbstractDwcControl implements Focusable, TabTraversable {

  private BBjHtmlView bbjHtmlView;

  private final ArrayList<Consumer<HtmlContainerOnScriptLoadedEvent>> scriptLoadedEvents = new ArrayList<>();
  private HtmlContainerOnScriptLoadedEventSink onScriptLoadedSink = null;
  private final ArrayList<Consumer<HtmlContainerOnScriptFailedEvent>> scriptFailedEvents = new ArrayList<>();
  private HtmlContainerOnScriptFailedEventSink onScriptFailedSink = null;
  private final ArrayList<Consumer<HtmlContainerJavascriptEvent>> javascriptEvents = new ArrayList<>();
  private HtmlContainerNativeJavascriptEventSink javascriptEventSink = null;
  private final ArrayList<Consumer<HtmlContainerPageLoadedEvent>> pageLoadedEvents = new ArrayList<>();
  private HtmlContainerPageLoadedEventSink pageLoadedEventSink = null;

  private ArrayList<String> executeScript = new ArrayList<>();
  private ArrayList<String> executeAsyncScript = new ArrayList<>();
  private String injectScript = "";
  private Boolean injectScriptTop = false;
  private String injectURL = "";
  private Boolean injectURLTop = false;
  private Boolean autoNavigate = false;
  private String downloadDirectory = "";
  private String url = "";
  private Boolean reload = false;
  private String userAgent = "";

  public HtmlContainer() {
  }

  public HtmlContainer(String text) {
    setText(text);
  }

  @Override
  protected void create(AbstractPanel p) {
    try {
      BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      ctrl = w.addHtmlView(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1,
          getText(), flags);
      ctrl.setNoEdge(true);
      bbjHtmlView = (BBjHtmlView) ctrl;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public HtmlContainer onScriptLoaded(Consumer<HtmlContainerOnScriptLoadedEvent> callback) {
    if (this.ctrl != null) {
      if (this.onScriptLoadedSink == null) {
        this.onScriptLoadedSink = new HtmlContainerOnScriptLoadedEventSink(this);
      }
      this.onScriptLoadedSink.addCallback(callback);
    } else {
      this.scriptLoadedEvents.add(callback);
    }
    return this;

  }

  public HtmlContainer onScriptFailed(Consumer<HtmlContainerOnScriptFailedEvent> callback) {
    if (this.ctrl != null) {
      if (this.onScriptFailedSink == null) {
        this.onScriptFailedSink = new HtmlContainerOnScriptFailedEventSink(this);
      }
      this.onScriptFailedSink.addCallback(callback);
    } else {
      this.scriptFailedEvents.add(callback);
    }
    return this;

  }

  /**
   * Executes a script in the context of the currently loaded page without waiting
   * for the result.
   * 
   * If the control is not yet attache to panel, the control will queue the script
   * and execute it when the control is
   * attached to a panel. The result of the script will be null in this case.
   * 
   * It is recommended to invoke this method only after the control has been
   * attached to a panel.
   * 
   * @param script The script to execute
   */
  public void executeAsyncScript(String script) {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.executeAsyncScript(script);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }

    this.executeAsyncScript.add(script);
  }

  /**
   * Executes a script in the context of the currently loaded page and returns the
   * result.
   * 
   * If the control is not yet attache to panel, the control will queue the script
   * and execute it when the control is
   * attached to a panel. The result of the script will be null in this case.
   * 
   * It is recommended to invoke this method only after the control has been
   * attached to a panel.
   * 
   * @param script The script to execute
   * @return The result of the script
   */
  public Object executeScript(String script) {
    if (this.ctrl != null) {
      try {
        return bbjHtmlView.executeScript(script);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }

    this.executeScript.add(script);
    return null;
  }

  public Boolean isAutoNavigate() {
    if (this.ctrl != null) {
      try {
        return bbjHtmlView.getAutoNavigate();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.autoNavigate;
  }

  public String getClientType() {
    if (this.ctrl != null) {
      try {
        return bbjHtmlView.getClientType();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return null;
  }

  public String getClientVersion() {
    if (this.ctrl != null) {
      try {
        return bbjHtmlView.getClientVersion();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return null;
  }

  public Image getImage() {
    if (this.ctrl != null) {
      try {
        return (Image) bbjHtmlView.getImage();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return null;
  }

  @Override
  public String getText() {
    if (this.ctrl != null) {
      try {
        return bbjHtmlView.getText();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return super.getText();
  }

  public String getUrl() {
    if (this.ctrl != null) {
      try {
        return bbjHtmlView.getUrl();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.url;
  }

  public String getUserAgent() {
    if (this.ctrl != null) {
      try {
        return bbjHtmlView.getUserAgent();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.userAgent;
  }

  public HtmlContainer injectScript(String script) {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.injectScript(script);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.injectScript = script;
    return this;
  }

  public HtmlContainer injectScript(String script, Boolean top) {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.injectScript(script, top);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.injectScript = script;
    this.injectScriptTop = top;
    return this;
  }

  public HtmlContainer injectUrl(String url) {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.injectUrl(url);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.injectURL = url;
    return this;
  }

  public HtmlContainer injectUrl(String url, Boolean top) {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.injectUrl(url, top);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.injectURL = url;
    this.injectURLTop = top;
    return this;
  }

  public Boolean print() {
    if (this.ctrl != null) {
      try {
        return bbjHtmlView.print();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return false;
  }

  public HtmlContainer setAutoNavigate(Boolean autoNavigate) {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.setAutoNavigate(autoNavigate);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.autoNavigate = autoNavigate;
    return this;
  }

  public HtmlContainer setDownloadDirectory(String downloadDir) {
    if (this.ctrl != null) {
      bbjHtmlView.setDownloadDirectory(downloadDir);
    }
    this.downloadDirectory = downloadDir;
    return this;
  }

  public HtmlContainer setUrl(String url) {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.setUrl(url);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.url = url;
    return this;
  }

  public HtmlContainer setUrl(String url, Boolean reload) {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.setUrl(url, reload);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.url = url;
    this.reload = reload;
    return this;
  }

  public HtmlContainer setUserAgent(String userAgent) {
    if (this.ctrl != null) {
      bbjHtmlView.setUserAgent(userAgent);
    }
    return this;
  }

  /**
   * register a callback method to be called after the page loaded event in the
   * htmlcontainer is reached
   * 
   * @param callback the callback method
   * @return the control itself
   */
  public HtmlContainer onPageLoaded(Consumer<HtmlContainerPageLoadedEvent> callback) {
    if (this.ctrl != null) {
      if (this.pageLoadedEventSink == null) {
        this.pageLoadedEventSink = new HtmlContainerPageLoadedEventSink(this);
      }
      this.pageLoadedEventSink.addCallback(callback);
    } else {
      this.pageLoadedEvents.add(callback);
    }
    return this;
  }

  /**
   * register a callback method to be called from JavaScript with
   * basisDispatchNativeEvent
   * see <a href=
   * "https://documentation.basis.com/BASISHelp/WebHelp/bbjevents/BBjNativeJavaScriptEvent/BBjNativeJavaScriptEvent.htm">BBjNativeJavaScriptEvent</a>
   * 
   * @param callback the callback method
   * @return the control itself
   */
  public HtmlContainer onJavascriptEvent(Consumer<HtmlContainerJavascriptEvent> callback) {
    if (this.ctrl != null) {
      if (this.javascriptEventSink == null) {
        this.javascriptEventSink = new HtmlContainerNativeJavascriptEventSink(this);
      }
      this.javascriptEventSink.addCallback(callback);
    } else {
      this.javascriptEvents.add(callback);
    }
    return this;
  }

  @Override
  public Boolean isFocusable() {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.isFocusable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.focusable;
  }

  @Override
  public HtmlContainer setFocusable(Boolean focusable) {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.setFocusable(focusable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.focusable = focusable;
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  @Override
  public HtmlContainer setTabTraversable(Boolean traversable) {
    if (this.ctrl != null) {
      try {
        bbjHtmlView.setTabTraversable(traversable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.tabTraversable = traversable;
    return this;
  }

  @Override
  public HtmlContainer setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public HtmlContainer setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public HtmlContainer setEnabled(Boolean enabled) {
    super.setEnabled(enabled);
    return this;
  }

  @Override
  public HtmlContainer setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public HtmlContainer setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public HtmlContainer setId(String elementId) {
    super.setId(elementId);
    return this;
  }

  @Override
  public HtmlContainer setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public HtmlContainer addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public HtmlContainer removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp()))
      throw new IllegalAccessException("catchUp cannot be called twice");
    super.catchUp();

    if (!this.scriptLoadedEvents.isEmpty()) {
      this.onScriptLoadedSink = new HtmlContainerOnScriptLoadedEventSink(this);
      while (!this.scriptLoadedEvents.isEmpty()) {
        this.onScriptLoadedSink.addCallback(this.scriptLoadedEvents.remove(0));
      }
    }

    if (!this.scriptFailedEvents.isEmpty()) {
      this.onScriptFailedSink = new HtmlContainerOnScriptFailedEventSink(this);
      while (!this.scriptFailedEvents.isEmpty()) {
        this.onScriptFailedSink.addCallback(this.scriptFailedEvents.remove(0));
      }
    }

    if (!this.javascriptEvents.isEmpty()) {
      this.javascriptEventSink = new HtmlContainerNativeJavascriptEventSink(this);
      while (!this.javascriptEvents.isEmpty()) {
        this.javascriptEventSink.addCallback(this.javascriptEvents.remove(0));
      }
    }

    if (!this.pageLoadedEvents.isEmpty()) {
      this.pageLoadedEventSink = new HtmlContainerPageLoadedEventSink(this);
      while (!this.pageLoadedEvents.isEmpty()) {
        this.pageLoadedEventSink.addCallback(this.pageLoadedEvents.remove(0));
      }
    }

    if (!this.executeAsyncScript.isEmpty()) {
      for (String script : this.executeAsyncScript) {
        this.executeAsyncScript(script);
      }
    }

    if (!this.executeScript.isEmpty()) {
      for (String script : this.executeScript) {
        this.executeScript(script);
      }
    }

    if (!this.injectScript.equals("")) {
      this.injectScript(this.injectScript, this.injectScriptTop);
    }

    if (!this.injectURL.equals("")) {
      this.injectUrl(this.injectURL, this.injectURLTop);
    }

    if (Boolean.TRUE.equals(this.autoNavigate)) {
      this.setAutoNavigate(this.autoNavigate);
    }

    if (!this.downloadDirectory.equals("")) {
      this.setDownloadDirectory(this.downloadDirectory);
    }

    if (!this.url.equals("")) {
      this.setUrl(this.url, this.reload);
    }

    if (!this.userAgent.equals("")) {
      this.setUserAgent(this.userAgent);
    }

    if (Boolean.FALSE.equals(this.focusable)) {
      this.setFocusable(this.focusable);
    }

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }
  }
}