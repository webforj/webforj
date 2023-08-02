package org.dwcj.component.htmlcontainer;

import com.basis.bbj.proxies.sysgui.BBjHtmlView;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.htmlcontainer.event.HtmlContainerJavascriptEvent;
import org.dwcj.component.htmlcontainer.event.HtmlContainerScriptFailEvent;
import org.dwcj.component.htmlcontainer.event.HtmlContainerScriptLoadEvent;
import org.dwcj.component.htmlcontainer.event.HtmlContainerPageLoadEvent;
import org.dwcj.component.htmlcontainer.sink.HtmlContainerNativeJavascriptEventSink;
import org.dwcj.component.htmlcontainer.sink.HtmlContainerScriptFailEventSink;
import org.dwcj.component.htmlcontainer.sink.HtmlContainerScriptLoadEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.concern.HasEnable;
import org.dwcj.concern.HasFocus;
import org.dwcj.concern.HasTabTraversal;
import org.dwcj.utilities.BBjFunctionalityHelper;
import org.dwcj.component.htmlcontainer.sink.HtmlContainerPageLoadEventSink;
import java.awt.*;
import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * A HtmlContainer control
 */
public final class HtmlContainer extends AbstractDwcComponent
    implements HasFocus, HasEnable, HasTabTraversal {

  private BBjHtmlView bbjHtmlView;

  private final ArrayList<Consumer<HtmlContainerScriptLoadEvent>> scriptLoadedEvents =
      new ArrayList<>();
  private HtmlContainerScriptLoadEventSink onScriptLoadSink = null;
  private final ArrayList<Consumer<HtmlContainerScriptFailEvent>> scriptFailedEvents =
      new ArrayList<>();
  private HtmlContainerScriptFailEventSink onScriptFailSink = null;
  private final ArrayList<Consumer<HtmlContainerJavascriptEvent>> javascriptEvents =
      new ArrayList<>();
  private HtmlContainerNativeJavascriptEventSink javascriptEventSink = null;
  private final ArrayList<Consumer<HtmlContainerPageLoadEvent>> pageLoadedEvents =
      new ArrayList<>();
  private HtmlContainerPageLoadEventSink pageLoadEventSink = null;

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

  public HtmlContainer() {}

  public HtmlContainer(String text) {
    setText(text);
  }

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      control = w.addHtmlView(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, BASISNUMBER_1, getText(), flags);
      control.setNoEdge(true);
      bbjHtmlView = (BBjHtmlView) control;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public HtmlContainer onScriptLoad(Consumer<HtmlContainerScriptLoadEvent> callback) {
    if (this.control != null) {
      if (this.onScriptLoadSink == null) {
        this.onScriptLoadSink = new HtmlContainerScriptLoadEventSink(this);
      }
      this.onScriptLoadSink.addCallback(callback);
    } else {
      this.scriptLoadedEvents.add(callback);
    }
    return this;

  }

  public HtmlContainer onScriptFail(Consumer<HtmlContainerScriptFailEvent> callback) {
    if (this.control != null) {
      if (this.onScriptFailSink == null) {
        this.onScriptFailSink = new HtmlContainerScriptFailEventSink(this);
      }
      this.onScriptFailSink.addCallback(callback);
    } else {
      this.scriptFailedEvents.add(callback);
    }
    return this;

  }

  /**
   * Executes a script in the context of the currently loaded page without waiting for the result.
   *
   * <p>
   * If the control is not yet attache to panel, the control will queue the script and execute it
   * when the control is attached to a panel. The result of the script will be null in this case.
   *
   * It is recommended to invoke this method only after the control has been attached to a panel.
   * </p>
   *
   * @param script The script to execute
   */
  public void executeAsyncScript(String script) {
    if (this.control != null) {
      try {
        bbjHtmlView.executeAsyncScript(script);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }

    this.executeAsyncScript.add(script);
  }

  /**
   * Executes a script in the context of the currently loaded page and returns the result.
   *
   * <p>
   * If the control is not yet attache to panel, the control will queue the script and execute it
   * when the control is attached to a panel. The result of the script will be null in this case.
   *
   * It is recommended to invoke this method only after the control has been attached to a panel.
   * </p>
   *
   * @param script The script to execute
   * @return The result of the script
   */
  public Object executeScript(String script) {
    if (this.control != null) {
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
    if (this.control != null) {
      try {
        return bbjHtmlView.getAutoNavigate();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.autoNavigate;
  }

  public String getClientType() {
    if (this.control != null) {
      try {
        return bbjHtmlView.getClientType();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return null;
  }

  public String getClientVersion() {
    if (this.control != null) {
      try {
        return bbjHtmlView.getClientVersion();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return null;
  }

  public Image getImage() {
    if (this.control != null) {
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
    if (this.control != null) {
      try {
        return bbjHtmlView.getText();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return super.getText();
  }

  public String getUrl() {
    if (this.control != null) {
      try {
        return bbjHtmlView.getUrl();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.url;
  }

  public String getUserAgent() {
    if (this.control != null) {
      try {
        return bbjHtmlView.getUserAgent();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.userAgent;
  }

  public HtmlContainer injectScript(String script) {
    if (this.control != null) {
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
    if (this.control != null) {
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
    if (this.control != null) {
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
    if (this.control != null) {
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
    if (this.control != null) {
      try {
        return bbjHtmlView.print();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return false;
  }

  public HtmlContainer setAutoNavigate(Boolean autoNavigate) {
    if (this.control != null) {
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
    if (this.control != null) {
      bbjHtmlView.setDownloadDirectory(downloadDir);
    }
    this.downloadDirectory = downloadDir;
    return this;
  }

  public HtmlContainer setUrl(String url) {
    if (this.control != null) {
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
    if (this.control != null) {
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
    if (this.control != null) {
      bbjHtmlView.setUserAgent(userAgent);
    }
    return this;
  }

  /**
   * register a callback method to be called after the page loaded event in the htmlcontainer is
   * reached
   *
   * @param callback the callback method
   * @return the control itself
   */
  public HtmlContainer onPageLoad(Consumer<HtmlContainerPageLoadEvent> callback) {
    if (this.control != null) {
      if (this.pageLoadEventSink == null) {
        this.pageLoadEventSink = new HtmlContainerPageLoadEventSink(this);
      }
      this.pageLoadEventSink.addCallback(callback);
    } else {
      this.pageLoadedEvents.add(callback);
    }
    return this;
  }

  /**
   * register a callback method to be called from JavaScript with basisDispatchNativeEvent see
   * <a href=
   * "https://documentation.basis.com/BASISHelp/WebHelp/bbjevents/BBjNativeJavaScriptEvent/BBjNativeJavaScriptEvent.htm">BBjNativeJavaScriptEvent</a>
   *
   * @param callback the callback method
   * @return the control itself
   */
  public HtmlContainer onJavascriptEvent(Consumer<HtmlContainerJavascriptEvent> callback) {
    if (this.control != null) {
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
  public HtmlContainer focus() {
    super.focusComponent();
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.control != null) {
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
    if (this.control != null) {
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
  public HtmlContainer setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
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
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();

    if (!this.scriptLoadedEvents.isEmpty()) {
      this.onScriptLoadSink = new HtmlContainerScriptLoadEventSink(this);
      while (!this.scriptLoadedEvents.isEmpty()) {
        this.onScriptLoadSink.addCallback(this.scriptLoadedEvents.remove(0));
      }
    }

    if (!this.scriptFailedEvents.isEmpty()) {
      this.onScriptFailSink = new HtmlContainerScriptFailEventSink(this);
      while (!this.scriptFailedEvents.isEmpty()) {
        this.onScriptFailSink.addCallback(this.scriptFailedEvents.remove(0));
      }
    }

    if (!this.javascriptEvents.isEmpty()) {
      this.javascriptEventSink = new HtmlContainerNativeJavascriptEventSink(this);
      while (!this.javascriptEvents.isEmpty()) {
        this.javascriptEventSink.addCallback(this.javascriptEvents.remove(0));
      }
    }

    if (!this.pageLoadedEvents.isEmpty()) {
      this.pageLoadEventSink = new HtmlContainerPageLoadEventSink(this);
      while (!this.pageLoadedEvents.isEmpty()) {
        this.pageLoadEventSink.addCallback(this.pageLoadedEvents.remove(0));
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

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }
  }
}
