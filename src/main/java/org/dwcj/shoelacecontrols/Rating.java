package org.dwcj.shoelacecontrols;

import com.basis.bbj.proxies.event.BBjNativeJavaScriptEvent;
import com.basis.bbj.proxies.event.BBjPageLoadedEvent;
import com.basis.bbj.proxies.sysgui.BBjHtmlView;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.ValueChangedEvent;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

public class Rating extends AbstractShoelaceControl {

    final private String uuid = "id" + java.util.UUID.randomUUID().toString().replace("-", "");


    private int max = 5;
    private Double value = 0.0;
    private Double precision = 1.0;
    private BBjHtmlView htmlv;
    private Consumer<ValueChangedEvent> onValueChangedCallback;

    public Rating() {
    }

    @Override
    public void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            htmlv = w.addHtmlView(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            htmlv.setCallback(Environment.getInstance().getBBjAPI().ON_PAGE_LOADED, Environment.getInstance().getDwcjHelper().getEventProxy(this, "_cbPageLoaded"), "onEvent");
            htmlv.setCallback(Environment.getInstance().getBBjAPI().ON_NATIVE_JAVASCRIPT, Environment.getInstance().getDwcjHelper().getEventProxy(this, "_cbJS"), "onEvent");
            htmlv.setText("<sl-rating precision='" + precision + "' value='" + value + "' max='" + max + "' id='" + uuid + "'></sl-rating>");
            ctrl = htmlv;
            htmlv.setNoEdge(true);

        } catch (Exception e) {
            e.printStackTrace();
        }
        loadShoelaceLib();
    }

    public void _cbPageLoaded(BBjPageLoadedEvent ev) throws BBjException {
        htmlv.clearCallback(Environment.getInstance().getBBjAPI().ON_PAGE_LOADED);
        String scr = "document.getElementById('" + uuid + "').addEventListener('sl-change', " +
                "event => {var custom=new CustomEvent(" +
                "'custom_event',{bubbles:true,cancelable:true});" +
                "custom.value=event.target.value;" +
                "window.basisDispatchCustomEvent(event.target,custom);})";
        htmlv.executeScript(scr);
    }

    public void _cbJS(BBjNativeJavaScriptEvent ev) throws BBjException {
        value = Double.valueOf(ev.getEventMap().get("value"));
        if (this.onValueChangedCallback != null) {
            ValueChangedEvent vev = new ValueChangedEvent(this, value);
            onValueChangedCallback.accept(vev);
        }
    }

    public void onValueChanged(Consumer<ValueChangedEvent> callback) {
        this.onValueChangedCallback = callback;
    }

    public int getMaxValue() {
        return this.max;
    }

    public void setMaxValue(int max) {
        this.max = max;
        try {
            htmlv.executeScript("document.getElementById('" + uuid + "').setAttribute('max','" + max + "');");
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public Double getValue() {
        return value;
    }

    public void setValue(Double value) {
        this.value = value;
        try {
            htmlv.executeScript("document.getElementById('" + uuid + "').setAttribute('value','" + value + "');");
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public Double getPrecision() {
        return precision;

    }

    public void setPrecision(Double precision) {
        this.precision = precision;
        try {
            htmlv.executeScript("document.getElementById('" + uuid + "').setAttribute('precision','" + precision + "');");
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

}
