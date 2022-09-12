package org.dwcj.panels;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.events.DivClickEvent;
import org.dwcj.events.sinks.BBjDivClickEventSink;

import java.util.function.Consumer;

/**
 * This class represents a div container, which behaves as a panel and
 * can be styled and hold other divs (panels) and controls
 */
public final class Div extends AbstractDwcjPanel {

    private BBjDivClickEventSink divClickEventSink;

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
            this.divClickEventSink = new BBjDivClickEventSink(this, callback);
        else this.divClickEventSink.addCallback(callback);
        return this;
    }

}




