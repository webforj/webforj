package org.dwcj.panels;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.events.DivClickEvent;
import org.dwcj.events.sinks.DivClickEventSink;

import java.util.function.Consumer;

/**
 * This class represents a div container, which behaves as a panel and
 * can be styled and hold other divs (panels) and controls
 */
public final class Div extends AbstractDwcjPanel {

    private DivClickEventSink divClickEventSink;

    @Override
    protected void create(AbstractDwcjPanel p) {
        BBjWindow w = p.getBBjWindow();
        try {
            byte[] flags = new byte[]{(byte) 0x00, (byte) 0x10, (byte) 0x88, (byte) 0x00};
            //todo honor visible flag if set before addition to panel
            wnd = w.addChildWindow(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", flags, Environment.getInstance().getSysGui().getAvailableContext());
            ctrl = wnd;
        } catch (BBjException e) {
            e.printStackTrace();
        }

    }

    /**
     * register an event callback for the click event
     *
     * @param callback A method to receive the click event
     * @return the control itself
     */
    public Div onClick(Consumer<DivClickEvent> callback) {
        if (this.divClickEventSink ==null)
            this.divClickEventSink = new DivClickEventSink(this, callback);
        else this.divClickEventSink.addCallback(callback);
        return this;
    }


    @Override
    public Div setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public Div setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public Div setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public Div setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public Div setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public Div setID(String id){
        super.setControlID(id);
        return this;
    }

    @Override
    public Div setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    @Override
    public Div addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public Div removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }
}




