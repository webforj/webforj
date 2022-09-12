package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjHtmlView;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.JavascriptEvent;
import org.dwcj.events.PageLoadedEvent;
import org.dwcj.events.sinks.BBjNativeJavascriptEventSink;
import org.dwcj.events.sinks.BBjPageLoadedEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.awt.*;
import java.util.function.Consumer;

/**
 * A HtmlContainer control
 */
public final class HtmlContainer extends AbstractDwcControl implements IStyleable {

    private BBjHtmlView bbjHtmlView;

    public HtmlContainer() {
    }

    public HtmlContainer(String text) {
        setText(text);
    }

    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addHtmlView(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText());
            ctrl.setNoEdge(true);
            bbjHtmlView = (BBjHtmlView) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void executeAsyncScript(String script) {
        try {
            bbjHtmlView.executeAsyncScript(script);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public Object executeScript(String script) {
        try {
            return bbjHtmlView.executeScript(script);
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public boolean getAutoNavigate() {
        try {
            return bbjHtmlView.getAutoNavigate();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public String getClientType() {
        try {
            return bbjHtmlView.getClientType();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getClientVersion() {
        try {
            return bbjHtmlView.getClientVersion();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public Image getImage() {
        try {
            return (Image) bbjHtmlView.getImage();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public String getText() {
        try {
            return bbjHtmlView.getText();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getUrl() {
        try {
            return bbjHtmlView.getUrl();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getUserAgent() {
        try {
            return bbjHtmlView.getUserAgent();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public void injectScript(String script) {
        try {
            bbjHtmlView.injectScript(script);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void injectScript(String script, boolean top) {
        try {
            bbjHtmlView.injectScript(script,top);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void injectUrl(String url) {
        try {
            bbjHtmlView.injectUrl(url);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void injectUrl(String url, boolean top) {
        try {
            bbjHtmlView.injectUrl(url, top);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean print() {
        try {
            return bbjHtmlView.print();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public void setAutoNavigate(boolean autoNavigate) {
        try {
            bbjHtmlView.setAutoNavigate(autoNavigate);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setUrl(String url) {
        try {
            bbjHtmlView.setUrl(url);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setUrl(String url, boolean reload) {
        try {
            bbjHtmlView.setUrl(url, reload);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setUserAgent(String userAgent) {
        bbjHtmlView.setUserAgent(userAgent);
    }

    /**
     * register a callback method to be called after the page loaded event in the htmlcontainer is reached
     * @param callback the callback method
     * @return the control itself
     */
    public HtmlContainer onPageLoaded(Consumer<PageLoadedEvent> callback) {
        new BBjPageLoadedEventSink(this, callback);
        return this;
    }

    /**
     * register a callback method to be called from JavaScript with basisDispatchNativeEvent
     * see https://documentation.basis.com/BASISHelp/WebHelp/bbjevents/BBjNativeJavaScriptEvent/BBjNativeJavaScriptEvent.htm
     * @param callback the callback method
     * @return the control itself
     */
    public HtmlContainer onJavascriptEvent(Consumer<JavascriptEvent> callback) {
        new BBjNativeJavascriptEventSink(this, callback);
        return this;
    }

    @Override
    public HtmlContainer setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public IStyleable addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public IStyleable removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }


}
