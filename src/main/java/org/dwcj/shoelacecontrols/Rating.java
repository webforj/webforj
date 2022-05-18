package org.dwcj.shoelacecontrols;

import com.basis.bbj.proxies.event.BBjNativeJavaScriptEvent;
import com.basis.bbj.proxies.event.BBjPageLoadedEvent;
import com.basis.bbj.proxies.sysgui.BBjHtmlView;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.RatingValueChangedEvent;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

public final class Rating extends AbstractShoelaceControl {

    final private String uuid = "id" + java.util.UUID.randomUUID().toString().replace("-", "");

    private int max = 5;
    private Double value = 0.0;
    private Double precision = 1.0;
    private BBjHtmlView htmlv;
    private Consumer<RatingValueChangedEvent> onValueChangedCallback;

    public Rating() {
    }


    void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
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

    private void _cbPageLoaded(BBjPageLoadedEvent ev) throws BBjException {
        htmlv.clearCallback(Environment.getInstance().getBBjAPI().ON_PAGE_LOADED);
        String scr = "document.getElementById('" + uuid + "').addEventListener('sl-change', " + "event => {var custom=new CustomEvent(" + "'custom_event',{bubbles:true,cancelable:true});" + "custom.value=event.target.value;" + "window.basisDispatchCustomEvent(event.target,custom);})";
        htmlv.executeScript(scr);
    }

    /**
     * The native JS event propagated from the BBj side
     * note: this method is private to hide it from the API. BBj uses reflection to invoke it
     *
     * @param ev
     * @throws BBjException
     */
    private void _cbJS(BBjNativeJavaScriptEvent ev) throws BBjException {
        value = Double.valueOf(ev.getEventMap().get("value"));
        if (this.onValueChangedCallback != null) {
            RatingValueChangedEvent vev = new RatingValueChangedEvent(this, value);
            onValueChangedCallback.accept(vev);
        }
    }

    /**
     * register a callback for value changed
     *
     * @param callback
     * @return
     */
    public Rating onValueChanged(Consumer<RatingValueChangedEvent> callback) {
        this.onValueChangedCallback = callback;
        return this;
    }

    /**
     * returns the maximum value of the rating widget (IOW the number of stars)
     *
     * @return the maximum
     */
    public int getMaxValue() {
        return this.max;
    }

    /**
     * sets the maximum value of the rating control (IOW the number of stars)
     * Default is 5
     *
     * @param max the max value
     * @return the control itself
     */
    public Rating setMaxValue(int max) {
        this.max = max;
        try {
            htmlv.executeScript("document.getElementById('" + uuid + "').setAttribute('max','" + max + "');");
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * get the currently selected value of the rating control
     *
     * @return the selected value
     */
    public Double getValue() {
        return value;
    }

    /**
     * set the rating value
     *
     * @param value the value to be set
     * @return the control itself
     */
    public Rating setValue(Double value) {
        this.value = value;
        try {
            htmlv.executeScript("document.getElementById('" + uuid + "').setAttribute('value','" + value + "');");
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * query the precision of the rating control.
     *
     * @return - the precision currently set
     */
    public Double getPrecision() {
        return precision;
    }

    /**
     * set the precision of the rating control. It supports "fractional stars", so you could set 4.5 stars if the precision is set to .5
     * Default precision is 1.0
     *
     * @param precision the precision to be set, e.g. .5
     * @return the control itself
     */
    public Rating setPrecision(Double precision) {
        this.precision = precision;
        try {
            htmlv.executeScript("document.getElementById('" + uuid + "').setAttribute('precision','" + precision + "');");
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

}
