package org.dwcj.events.sinks.fontChooser;

import com.basis.bbj.proxies.event.BBjFileChooserCancelEvent;
import com.basis.bbj.proxies.event.BBjFileChooserChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.FontChooser;
import org.dwcj.events.fontChooser.FontChooserCancelEvent;
import org.dwcj.events.fontChooser.FontChooserChangeEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class FontChooserCancelEventSink {

    private ArrayList<Consumer<FontChooserCancelEvent>> targets;

    private final FontChooser fontChooser;

    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public FontChooserCancelEventSink(FontChooser fc, Consumer<FontChooserCancelEvent> callback) {
        this.targets.add(callback);
        this.fontChooser = fc;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(fc);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_FILECHOOSER_CANCEL,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "cancelEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.ctrl = bbjctrl;
    }

    public void changeEvent(BBjFileChooserCancelEvent ev) {
        FontChooserCancelEvent dwc_ev = new FontChooserCancelEvent(this.fontChooser);
        Iterator<Consumer<FontChooserCancelEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwc_ev);
    }

    public void addCallback(Consumer<FontChooserCancelEvent> callback) { targets.add(callback); }
}
